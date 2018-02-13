namespace Awelon
open Data.ByteString
open System.Collections.Generic
open System.Collections.ObjectModel
open Stowage


// Awelon is encoded in ASCII (minus C0 and DEL), and is syntactically
// simple, similar to Forth. A program is a sequence of named operations
// with first-class blocks (which contain programs) and namespaces. The
// operations are mostly user-defined words, but we'll also support some
// syntactic sugars (for embedded texts, natural numbers), annotations,
// and external binary or code resources.
//
//   Program = Action*
//   Action = Block | NS '/' Action | Atom
//   Block = '[' Program ']'
//   NS = Word
//   Atom = Word | Annotation | Nat | Text | Resource | Label 
//
//   Word = [a-z][a-z0-9_]*
//   Annotation = '(' Word ')'
//   Nat = '0' | [1-9][0-9]*
//   Text = '"' (not '"')* '"'
//   Resource = BinRef | CodeRef
//   BinRef = '%' RscHash
//   CodeRef = '$' RscHash
//   RscHash = (see Stowage.RscHash)
//   Label = LabelPut | LabelGet
//   LabelPut = ':' Word
//   LabelGet = '.' Word
// 
// This module implements a hand-written parser for Awelon code. 
module Parse =

    /// A Word is a ByteString with regex `[a-z][a-z0-9_]*`
    type Word = ByteString

    /// A Natural number is `0 | [1-9][0-9]*`. 
    /// Not processed into a number type by this module.
    type NatTok = ByteString

    /// Embedded Text in Awelon code.
    type Text = ByteString

    /// A RscHash is from Stowage
    type RscHash = Stowage.RscHash

    type TokenType =
        | TokWord       // word
        | TokAnno       // (anno)
        | TokNat        // 0 42 128
        | TokText       // "text"
        | TokLblPut     // :label
        | TokLblGet     // .label
        | TokBinRef     // %secureHash
        | TokCodeRef    // $secureHash

    /// A token is a labeled bytestring.
    type Token = struct(TokenType * ByteString)

    type Action =
        | Block of Program      // [Program]
        | NS of Word * Action   // ns/Action
        | Atom of Token         // simple action
    and Program = Action list   // Action*


    /// Awelon uses characters in ASCII minus C0 and DEL. Most
    /// of these are permitted only within embedded texts. For
    /// embedding Unicode, you might try binary resoures.
    let inline isAwelonChar c = (126uy >= c) && (c >= 32uy)

    let inline isWordStart c = (byte 'z' >= c) && (c >= byte 'a')
    let inline isNumChar c = (byte '9' >= c) && (c >= byte '0')
    let inline isWordChar c = isWordStart c || isNumChar c || (byte '_' = c)

    let isValidWord w =
        if (BS.isEmpty w) then false else
        isWordStart (BS.unsafeHead w) && BS.forall isWordChar (BS.unsafeTail w)

    let inline private startsWith pred s = 
        (not (BS.isEmpty s)) && (pred (BS.unsafeHead s))

    let tryParseWord (r:ByteString) : (struct(Word * ByteString)) option =
        if startsWith (isWordStart) r 
            then Some (BS.span isWordChar r)
            else None

    let private hasWordSep r = not (startsWith isWordChar r)

    /// Test match for regex `0 | [1-9][0-9]*`.
    let isValidNatTok s =
        if BS.isEmpty s then false 
        else if (byte '0' = (BS.unsafeHead s)) then (1 = BS.length s) 
        else BS.forall isNumChar s

    // Accept '0' | [1-9][0-9]* if followed by a word separator
    let tryParseNatTok (r:ByteString) : (struct(ByteString * ByteString)) option =
        let struct(n,r') = BS.span isNumChar r
        let ok = (not (BS.isEmpty n)) // empty token not permitted 
              && ((1 = BS.length n) || (byte '0' <> (BS.unsafeHead n))) // only 0 starts with 0
              && (hasWordSep r')
        if not ok then None else
        Some (struct(n,r'))

    let inline isTextChar c = (c <> 34uy) && (isAwelonChar c)
    let isValidText s = BS.forall isTextChar s

    let isValidToken (struct(tt,s) : Token) : bool =
        match tt with
        | TokWord | TokAnno | TokLblPut | TokLblGet -> isValidWord s
        | TokText -> isValidText s
        | TokNat -> isValidNatTok s
        | TokBinRef | TokCodeRef -> RscHash.isValidHash s

    let isValidProgram (p0:Program) : bool =
        let rec loop ps p =
            match p with
            | (a::p') ->
                match a with
                | Block b -> loop (p'::ps) b
                | NS (w,a') -> isValidWord w && loop ps (a'::p')
                | Atom t -> isValidToken t
            | [] ->
                match ps with
                | (p'::ps') -> loop ps' p'
                | [] -> true
        loop [] p0

    /// On Parse Error, we'll return the parser state.
    /// The lists in this case represent stacks.
    type ParseState =
        { cx    : (struct(Word list * Action list)) list // block context and NS
          ns    : Word list     // next operation's namespace
          p     : Action list   // current open block
          s     : ByteString    // remaining bytes
        }

    type ParseResult =
        | ParseOK of Program
        | ParseFail of ParseState
    
    let private finiParse cx ns p s =
        let ok = BS.isEmpty s && List.isEmpty cx && List.isEmpty ns
        if ok then ParseOK (List.rev p) else
        let st = { cx = cx; ns = ns; p = p; s = s }
        ParseFail st

    let rec private wrapNS (ns : Word list) (a:Action) : Action =
        match ns with
        | [] -> a
        | (w::ns') -> wrapNS ns' (NS(w,a))

    let inline private matchChar c s = 
        startsWith ((=) (byte c)) s

    let inline tok t w = struct(t,w)

    let private tryParseHash (s:ByteString) : (struct(RscHash * ByteString)) option =
        let struct(h,s') = BS.span (RscHash.isHashByte) s
        let ok = (RscHash.size = BS.length h) && (hasWordSep s')
        if not ok then None else Some (struct(h,s'))

    // Parse a single token. (Does not parse namespaces or blocks.)
    let tryParseToken (s:ByteString) : (struct(Token * ByteString)) option =
        if BS.isEmpty s then None else
        let c0 = BS.unsafeHead s
        if isWordStart c0 then
            let struct(w,s') = BS.span isWordChar s
            Some (struct(tok TokWord w, s'))
        else if isNumChar c0 then
            match tryParseNatTok s with
            | Some (struct(n,s')) -> Some (struct(tok TokNat n, s'))
            | _ -> None
        else if(byte '"' = c0) then // embedded texts
            let struct(txt,s') = BS.span isTextChar (BS.unsafeTail s)
            if (matchChar '"' s') 
                then Some (struct(tok TokText txt, BS.unsafeTail s'))
                else None
        else if(byte '(' = c0) then // annotation
            match tryParseWord (BS.unsafeTail s) with
            | Some (struct(w,s')) when matchChar ')' s' ->
                Some (struct(Anno w, BS.unsafeTail s'))
            | _ -> None
        else if(byte ':' = c0) then // record label 
            match tryParseWord (BS.unsafeTail s) with
            | Some (struct(w,s')) -> Some (struct(tok TokLblPut w, s'))
            | _ -> None
        else if(byte '.' = c0) then // record label access
            match tryParseWord (BS.unsafeTail s) with
            | Some (struct(w,s')) -> Some (struct(tok TokLblGet w, s'))
            | _ -> None
        else if(byte '%' = c0) then // binary resource
            match tryParseHash (BS.unsafeTail s) with
            | Some (struct(h,s')) -> Some (struct(tok TokBinRef h, s'))
            | _ -> None
        else if(byte '$' = c0) then // stowage resource
            match tryParseHash (BS.unsafeTail s) with
            | Some (struct(h,s')) -> Some (struct(tok TokCodeRef h, s'))
            | _ -> None
        else None

    let rec parse' cx ns p s =
        if BS.isEmpty s then finiParse cx ns p s else
        let c0 = BS.unsafeHead s
        if ((byte ' ' = c0) && (List.isEmpty ns)) then
            // forbid whitespace within namespace!
            parse' cx [] p (BS.unsafeTail s)
        else if(byte '[' = c0) then
            parse' (struct(ns,p)::cx) [] (BS.unsafeTail s)
        else if((byte ']' = c0) && (List.isEmpty ns)) then
            match cx with
            | (struct(ns',p')::cx') ->
                let a = wrapNS ns' (Block(List.rev p))
                parse' cx' [] (a :: p') (BS.unsafeTail s)
            | [] -> finiParse cx ns p s
        else if isWordStart c0 then
            let struct(w,s') = BS.span isWordChar s
            if matchChar '/' s' then
                // word is namespace prefix
                parse' cx (w :: ns) p (BS.unsafeTail s')
            else
                // word is normal action
                let a = wrapNS ns (Atom (tok TokWord w))
                parse' cx [] (a :: p) s'
        else 
            match tryParseToken s with
            | Some (struct(a,s')) ->
                parse' cx [] ((wrapNS ns (Atom a)) :: p) s'
            | _ -> parseFini cx ns p s

    /// Parse from a ByteString.
    ///
    /// I assume Awelon programs are relatively small, up to a few dozen
    /// kilobytes due to editable views. If we want to model streaming
    /// code, do so at the dictionary layer to simplify caching, forking,
    /// versioning, review, undo, and editing of the stream. Individual
    /// definitions should still be small.
    let inline parse (s:ByteString) : ParseResult = parse' [] [] [] s

    // Write program to byte string. Currently this will perform
    // a naive recursive write, so there may be stack issues if
    // the input program is very large or deep.
    //
    // This writer tries to minimize unnecessary whitespace. 
    let rec writeProgram (ws:bool) (p:Program) (dst:ByteDst) : unit =
        match p with
        | (a :: p') -> writeAction ws p' a dst
        | [] -> ()
    and writeAction (ws:bool) (p:Program) (a:Action) (dst:ByteDst) : unit =
        match a with
        | B b ->
            ByteStream.writeByte (byte '[') dst
            writeProgram true b dst
            ByteStream.writeByte (byte ']') dst
        | W w ->
            ByteStream.writeBytes w dst
        | N n -> 
            ByteStream.writeBytes (BS.fromString (string n)) dst
        | T t ->
            ByteStream.writeByte (byte '"') dst
            ByteStream.writeBytes t dst
            ByteStream.writeByte (byte '"') dst
        | Anno w ->
            ByteStream.writeByte (byte '(') dst
            ByteStream.writeBytes w dst
            ByteStream.writeByte (byte ')') dst
        | LPut lbl ->
            ByteStream.writeByte (byte ':') dst
            ByteStream.writeBytes lbl dst
        | LGet lbl ->
            ByteStream.writeByte (byte '.') dst
            ByteStream.writeBytes lbl dst
        | Bin h ->
            ByteStream.writeByte (byte '%') dst
            ByteStream.writeBytes h dst
        | Ext h ->
            ByteStream.writeByte (byte '$') dst
            ByteStream.writeBytes h dst
        | Hier (a,d) ->
            writeAction a dst
            ByteStream.writeByte (byte '@') dst
            ByteStream.writeBytes d dst

    /// Write a Program to a ByteString.
    let inline write (p:Program) : ByteString = 
        ByteStream.write (writeProgram p)






