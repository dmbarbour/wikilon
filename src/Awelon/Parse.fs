namespace Awelon
open Data.ByteString
open System.Collections.Immutable
open Stowage

// Awelon is encoded in ASCII (minus C0 and DEL), and is syntactically
// simple, similar to Forth. A program is a sequence of named operations
// with first-class blocks, which further contain programs. 
//
//   Program = (Action)*
//   Action = Block | Atom
//   Block = '[' Program ']'
//   Atom = Word | Annotation | Nat | Text 
//
//   Word = [a-z][a-z0-9-]* 
//   Annotation = '(' Word ')'
//   Nat = '0' | [1-9][0-9]*
//   Text = '"' (not '"')* '"'
//
// An Awelon dictionary may also include ad-hoc binary resources, but
// we won't even attempt to parse those. Texts are limited binaries.
//
// TODO: separate lexer and parser operation? It would be convenient
// for stuff like finding all word dependencies or renaming a word.
module Parser =

    /// A Word is a ByteString with regex `[a-z][a-z0-9-]*`. 
    type Word = ByteString

    /// A positive Natural number is `[1-9][0-9]*`. 
    /// Not processed into a number type by this module.
    type NatTok = ByteString

    /// Embedded Text in Awelon code.
    type Text = ByteString

    /// Tokens are just typed ByteString fragments. We'll skip the 
    /// annotation parentheses, text quotes, or prefix characters. 
    type TT =
        | Word=0    // word
        | Anno=1    // (anno)
        | Nat=2     // 0 42 128
        | Text=3    // "text"
    type Token = (struct(TT * ByteString))
    let inline tok tt s = struct(tt,s)
    let inline tokWord w = tok TT.Word w
    let inline tokAnno w = tok TT.Anno w
    let inline tokNat  n = tok TT.Nat n
    let inline tokText t = tok TT.Text t

    /// At this Parser layer, a Program is simply a legal parse. There
    /// is no association with the dictionary or Stowage context.
    type Program = Action list
    and Action =
        | Atom of Token         // simple action
        | Block of Program      // [Program]

    // tokenization cursor
    type private TC = (struct(Program list * Program))
    let rec private tokStep (struct(u,r) : TC) : (Token * TC) option =
        match r with
        | (op::r') -> 
            match op with
            | Atom tok -> Some (tok, struct(u,r'))   // report token
            | Block b -> tokStep (struct((r'::u),b)) // step into block
        | [] -> 
            match u with
            | (r'::u') -> tokStep (struct(u',r'))    // step out of block
            | [] -> None                             // done

    /// Extract sequence of Tokens from program, ignoring block structure.    
    let tokenize (p:Program) : seq<Token> =
        Seq.unfold tokStep (struct([],p))

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

    let inline private isSepChar c = 
        (byte ' ' = c) || (byte '[' = c) || (byte ']' = c)
    let inline private hasSep r = (BS.isEmpty r) || (isSepChar (BS.unsafeHead r)) 

    let tryParseWord (r:ByteString) : (struct(Word * ByteString)) option =
        let noWordStart = BS.isEmpty r 
                       || not (isWordStart (BS.unsafeHead r))
        if noWordStart then None else Some (BS.span isWordChar r)

    let inline private hasWordSep r = 
        (BS.isEmpty r) || (isSepChar (BS.unsafeHead r))

    /// Test match for regex `0 | [1-9][0-9]*`.
    let isValidNatTok s =
        if BS.isEmpty s then false 
        else if ((byte '0') = (BS.unsafeHead s)) then (1 = BS.length s) 
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
        | TT.Word | TT.Anno -> isValidWord s
        | TT.Text -> isValidText s
        | TT.Nat -> isValidNatTok s
        | _ -> false

    let isValidProgram (expr:Program) : bool =
        Seq.forall isValidToken (tokenize expr)

    /// On Parse Error, we'll return a parser stack (a list
    /// of reverse-ordered programs, representing a partial
    /// parse) and the remaining input. Otherwise, we'll 
    /// return a properly structured program.
    type ParseResult =
        | ParseOK of Program
        | ParseFail of ((Action list) list * ByteString)

    let private finiParse p b s =
        let ok = BS.isEmpty s && List.isEmpty p
        if ok then ParseOK (List.rev b) else
        ParseFail (b::p,s)

    let inline private matchChar c s = 
        startsWith ((=) (byte c)) s

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
        else None

    let rec private parse' p b s : ParseResult =
        if BS.isEmpty s then finiParse p b s else
        let c0 = BS.unsafeHead s
        if (byte ' ' = c0) then
            parse' p b (BS.unsafeTail s)
        else if(byte '[' = c0) then
            parse' (b::p) [] (BS.unsafeTail s)
        else if(byte ']' = c0) then
            match p with
            | (b'::p') -> 
                let op = Block (List.rev b)
                parse' p' (op::b') (BS.unsafeTail s)
            | [] -> finiParse p b s // imbalance of ']'
        else 
            match tryParseToken s with
            | Some (struct(tok,s')) -> parse' p ((Atom tok)::b) s'
            | None -> finiParse p b s

    /// Parse from a ByteString.
    ///
    /// I assume Awelon programs are relatively small, up to a few dozen
    /// kilobytes due to editable views. So this isn't heavily optimized.
    let inline parse (s:ByteString) : ParseResult = parse' [] [] s

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
        | _ -> invalidArg "tt" "unrecognized token type"

    let inline wsp p dst =
        if not (List.isEmpty p)
            then ByteStream.writeByte (byte ' ') dst

    let rec private wloop cx p dst =
        match p with
        | (op :: p') -> 
            match op with
            | Block b ->
                ByteStream.writeByte (byte '[') dst
                wloop (p'::cx) b dst
            | Atom tok ->
                writeToken tok dst
                wsp p' dst
                wloop cx p' dst
        | [] -> 
            match cx with
            | (p'::cx') ->
                ByteStream.writeByte (byte ']') dst
                wsp p' dst
                wloop cx' p' dst
            | [] -> () // done
 
    /// Write a program to a byte stream.
    ///
    /// Note: This writer normalizes spaces, injecting SP between
    /// adjacent actions. `1(nat)[4]b` becomes `1 (nat) [4] b`. I
    /// believe this should have negligible impact on performance.
    let write' p dst = wloop [] p dst

    /// Write to byte string. (Trivially wraps write'.)
    let inline write (p:Program) : ByteString = 
        ByteStream.write (write' p)


