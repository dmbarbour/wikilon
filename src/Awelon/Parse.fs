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
//   Word = [a-z][a-z0-9-]* 
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

    /// A Word is a ByteString with regex `[a-z][a-z0-9-]*`. 
    type Word = ByteString

    /// A Natural number is `0 | [1-9][0-9]*`. 
    /// Not processed into a number type by this module.
    type NatTok = ByteString

    /// Embedded Text in Awelon code.
    type Text = ByteString

    /// A RscHash is from Stowage
    ///
    /// NOTE: We only keep weak references at this parser layer. The
    /// client may need special actions to keep references alive.
    type RscHash = Stowage.RscHash

    /// Tokens are just typed ByteString fragments. We'll skip the 
    /// annotation parentheses, text quotes, or prefix characters. 
    type TT =
        | Word=0    // word
        | Anno=1    // (anno)
        | Nat=2     // 0 42 128
        | Text=3    // "text"
        | LblPut=4  // :label
        | LblGet=5  // .label
        | BinRef=6  // %secureHash
        | CodeRef=7 // $secureHash
    type Token = (struct(TT * ByteString))
    let inline tok tt s = struct(tt,s)
    let inline tokWord w = tok TT.Word w
    let inline tokAnno w = tok TT.Anno w
    let inline tokNat  n = tok TT.Nat n
    let inline tokText t = tok TT.Text t
    let inline tokLblPut w = tok TT.LblPut w
    let inline tokLblGet w = tok TT.LblGet w
    let inline tokBinRef h = tok TT.BinRef h
    let inline tokCodeRef h = tok TT.CodeRef h

    type Action =
        | Block of Program      // [Program]
        | NS of Word * Action   // ns/Action
        | Atom of Token         // simple action
    and Program = Action list   // Action*
        // TODO: consider a vector (immutable array) type

    /// view each token in the program
    let rec seqTokens (p:Program) : seq<Token> =
        seq {
            match p with
            | (a :: p') -> 
                yield! seqActionTokens a
                yield! seqTokens p'
            | [] -> ()
        }
    and private seqActionTokens (a:Action) : seq<Token> =
        match a with
        | Block b -> seqTokens b
        | NS (_,a') -> seqActionTokens a'
        | Atom t -> Seq.singleton t

    /// view each word in the program, in order of appearance.
    let seqWords (p:Program) : seq<Word> =
        seq {
            for (struct(tt,w)) in seqTokens p do
                match tt with
                | TT.Word -> yield w
                | _ -> ()
        }

    /// view each RscHash in the program, in order of appearance.
    let seqRefs (p:Program) : seq<RscHash> =
        seq {
            for (struct(tt,h)) in seqTokens p do
                match tt with
                | TT.CodeRef | TT.BinRef -> yield h
                | _ -> ()
        }

    /// Awelon uses characters in ASCII minus C0 and DEL. Most
    /// of these are permitted only within embedded texts. For
    /// embedding Unicode, you might try binary resoures.
    let inline isAwelonChar c = (126uy >= c) && (c >= 32uy)

    let inline isWordStart c = (byte 'z' >= c) && (c >= byte 'a')
    let inline isNumChar c = (byte '9' >= c) && (c >= byte '0')
    let inline isWordChar c = isWordStart c || isNumChar c || (byte '-' = c)

    let isValidWord w =
        if (BS.isEmpty w) then false else
        isWordStart (BS.unsafeHead w) 
            && BS.forall isWordChar (BS.unsafeTail w)

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
        | TT.Word | TT.Anno | TT.LblPut | TT.LblGet -> isValidWord s
        | TT.Text -> isValidText s
        | TT.Nat -> isValidNatTok s
        | TT.BinRef | TT.CodeRef -> RscHash.isValidHash s
        | _ -> false

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

    /// On Parse Error, we'll return the parser's full state.
    /// The lists in this case represent stacks, e.g. `p` is
    /// reverse order relative to the program it represents.
    type ParseState =
        { cx    : (struct(Word list * Action list)) list // block context
          ns    : Word list     // pending operation's namespace
          p     : Action list   // current open block
          s     : ByteString    // remaining bytes
        }

    type ParseResult =
        | ParseOK of Program
        | ParseFail of ParseState
    
    let private finiParse cx ns p s =
        let ok = BS.isEmpty s && List.isEmpty cx && List.isEmpty ns
        if ok then ParseOK (List.rev p) else
        ParseFail { cx = cx; ns = ns; p = p; s = s }

    let rec private wrapNS (ns : Word list) (a:Action) : Action =
        match ns with
        | [] -> a
        | (w::ns') -> wrapNS ns' (NS(w,a))

    let inline private matchChar c s = 
        startsWith ((=) (byte c)) s

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
            Some (struct(tokWord w, s'))
        else if isNumChar c0 then
            match tryParseNatTok s with
            | Some (struct(n,s')) -> Some (struct(tokNat n, s'))
            | _ -> None
        else if(byte '"' = c0) then // embedded texts
            let struct(txt,s') = BS.span isTextChar (BS.unsafeTail s)
            if (matchChar '"' s') 
                then Some (struct(tokText txt, BS.unsafeTail s'))
                else None
        else if(byte '(' = c0) then // annotation
            match tryParseWord (BS.unsafeTail s) with
            | Some (struct(w,s')) when matchChar ')' s' ->
                Some (struct(tokAnno w, BS.unsafeTail s'))
            | _ -> None
        else if(byte ':' = c0) then // record label 
            match tryParseWord (BS.unsafeTail s) with
            | Some (struct(w,s')) -> Some (struct(tokLblPut w, s'))
            | _ -> None
        else if(byte '.' = c0) then // record label access
            match tryParseWord (BS.unsafeTail s) with
            | Some (struct(w,s')) -> Some (struct(tokLblGet w, s'))
            | _ -> None
        else if(byte '%' = c0) then // binary resource
            match tryParseHash (BS.unsafeTail s) with
            | Some (struct(h,s')) -> Some (struct(tokBinRef h, s'))
            | _ -> None
        else if(byte '$' = c0) then // stowage resource
            match tryParseHash (BS.unsafeTail s) with
            | Some (struct(h,s')) -> Some (struct(tokCodeRef h, s'))
            | _ -> None
        else None

    // parse given full parse state 
    let rec parse' cx ns p s =
        if BS.isEmpty s then finiParse cx ns p s else
        let c0 = BS.unsafeHead s
        if ((byte ' ' = c0) && (List.isEmpty ns)) then
            // whitespace is permitted between actions
            // (but not between a namespace and action)
            parse' cx [] p (BS.unsafeTail s)
        else if(byte '[' = c0) then
            // save namespace for ']', save context to addend block
            parse' (struct(ns,p)::cx) [] [] (BS.unsafeTail s)
        else if((byte ']' = c0) && (List.isEmpty ns)) then
            // require empty ns to forbid `foo/bar/]` nonsense.
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
                let a = wrapNS ns (Atom (tokWord w))
                parse' cx [] (a :: p) s'
        else 
            match tryParseToken s with
            | Some (struct(a,s')) ->
                parse' cx [] ((wrapNS ns (Atom a)) :: p) s'
            | None -> finiParse cx ns p s

    /// Parse from a ByteString.
    ///
    /// I assume Awelon programs are relatively small, up to a few dozen
    /// kilobytes due to editable views. If we want to model streaming
    /// code, do so at the dictionary layer to simplify caching, forking,
    /// versioning, review, undo, and editing of the stream. Individual
    /// definitions should still be small.
    let inline parse (s:ByteString) : ParseResult = parse' [] [] [] s

    /// Parse from modified parse state.
    let inline parseST (st:ParseState) : ParseResult =
        parse' (st.cx) (st.ns) (st.p) (st.s)


    /// Write a single token. The token bytestring excludes the
    /// surrounding punctuation, so we'll add that back in. This
    /// assumes a valid token.
    let writeToken (struct(tt,s)) dst =
        match tt with
        | TT.Word -> 
            ByteStream.writeBytes s dst
        | TT.Anno ->
            ByteStream.writeByte (byte '(') dst
            ByteStream.writeBytes s dst
            ByteStream.writeByte (byte ')') dst
        | TT.Nat -> 
            ByteStream.writeBytes s dst
        | TT.Text ->
            ByteStream.writeByte (byte '"') dst
            ByteStream.writeBytes s dst
            ByteStream.writeByte (byte '"') dst
        | TT.LblPut ->
            ByteStream.writeByte (byte ':') dst
            ByteStream.writeBytes s dst
        | TT.LblGet ->
            ByteStream.writeByte (byte '.') dst
            ByteStream.writeBytes s dst
        | TT.BinRef ->
            ByteStream.writeByte (byte '%') dst
            ByteStream.writeBytes s dst
        | TT.CodeRef ->
            ByteStream.writeByte (byte '$') dst
            ByteStream.writeBytes s dst
        | _ -> invalidArg "tt" "unrecognized token type"

    let inline wsp p dst =
        if not (List.isEmpty p)
            then ByteStream.writeByte (byte ' ') dst

    let rec private wloop cx p dst =
        match p with
        | (a :: p') ->
            match a with
            | Block b -> 
                ByteStream.writeByte (byte '[') dst
                wloop (p' :: cx) b dst
            | NS (w,a') ->
                ByteStream.writeBytes w dst
                ByteStream.writeByte (byte '/') dst
                wloop cx (a' :: p') dst
            | Atom t ->
                writeToken t dst
                wsp p' dst
                wloop cx p' dst
        | [] ->
            match cx with
            | (p' :: cx') ->
                ByteStream.writeByte (byte ']') dst
                wsp p' dst
                wloop cx' p' dst
            | [] -> () // done!
 
    /// Write a program to a byte stream.
    ///
    /// Note: This writer normalizes spaces, injecting SP between
    /// adjacent actions. `1(nat)[4]b` becomes `1 (nat) [4] b`. I
    /// believe this should have negligible impact on performance.
    let write' p dst = wloop [] p dst

    /// Write to byte string. (Trivially wraps write'.)
    let inline write (p:Program) : ByteString = 
        ByteStream.write (write' p) 


