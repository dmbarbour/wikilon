namespace Awelon
open Data.ByteString
open System.Numerics
open Stowage

// Awelon is syntactically relatively simple, like Forth. But it has
// some complexity due to annotations and various extensions.
//
// The basic concept is just this:
//
//  Program = Action*
//  Action = Atom | Block 
//  Block = '[' Program ']'
//  Atom = Word | Wordlike
//  Word = [a-z][a-z0-9_]*
//
// The `Wordlike` atoms exist for performance or convenience. 
//  
//  Wordlike = Anno | Label | Txt | Nat | Rsc
//  Anno = '(' Word ')'
//  Label = [:.]Word
//  Txt = '"' [\32\33\35-\126]* '"'
//  Nat = '0' | [1-9][0-9]*
//  Rsc = [$%]Hash
//  Hash = cf. module Stowage.RscHash
//
// Labeled data is still experimental in Awelon, but supports basic
// row-polymorphic records and variants.
//
// For hierarchical dictionaries, we modify Action slightly:
//
//  Action = Atom | Block | Action '@' Word
//  
// Secure hash resources should bind to Stowage, while Words
// bind to a Dictionary. However, this module does not deal
// with bindings due to various issues of efficient caching.
//
// The parser's intermediate representation is not suitable
// for efficient evaluation. 
//
// A secondary goal is to provide reasonable feedback when we cannot
// parse a program completely.

module AST =

    /// A Word is a ByteString with regex `[a-z][a-z0-9_]*`
    type Word = ByteString

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

    /// A Natural number is currently represented via BigInteger.
    /// Naturally, only non-negative values are valid.
    type Nat = BigInteger

    /// Assumes valid input string, regex `0 | [1-9][0-9]*`. 
    let parseNat (s : ByteString) : Nat =
        if (BS.length s > 18) then BigInteger.Parse(BS.toString s) else
        let accum a b = (10L * a) + int64 b - int64 '0'
        let i64 = BS.fold accum 0L s
        new BigInteger(i64)

    let inline hasNatStart r =
        not (BS.isEmpty r) && (isNumChar (BS.unsafeHead r))

    /// This accepts `0 | [1-9][0-9]*`
    let tryParseNat (r : ByteString) : (struct(Nat * ByteString)) option =
        let struct(ns,r') = BS.span isNumChar r
        let accept = not (BS.isEmpty ns) 
                  && ((byte '0' <> BS.unsafeHead ns) || (1 = BS.length ns))
        if accept then Some (struct(parseNat ns, r')) else None

    /// Texts embedded in Awelon code are constrained rather severely.
    /// They may only contain ASCII characters minus C0, DEL, and `"`.
    type Text = ByteString
    let inline isTextChar c = (126uy >= c) && (c >= 32uy) && (c <> 34uy)

    type Action =
        | B of Program      // [subprogram]
        | W of Word         // hello_world
        | N of Nat          // 0, 1, 42
        | T of Text         // "text"
        | Anno of Word      // (word)
        | LPut of Word      // :word
        | LGet of Word      // .word
        | Bin of RscHash    // %secureHash
        | Ext of RscHash    // $secureHash
        | Hier of Action * Word // Action @ Word
    and Program = Action list

    /// An incomplete program may have several open blocks. I record
    /// this information in a reverse-ordered list for fast addend.
    ///
    ///   foo [bar baz [qux   =>    [qux; baz bar; foo]
    ///
    /// Additionally, parse may fail before it reaches the end of the
    /// input, so we may need to return the unparsed input. Looking at
    /// the first character of the input can provide some hints about
    /// the error.
    type IncompleteProg = (struct(Program list * ByteString))

    /// Parse result is either a valid program or an incomplete program
    /// with remaining bytes. 
    type ParseResult =
        | ParseOK of Program
        | ParseErr of IncompleteProg

    let inline private matchChar c s = 
        startsWith ((=) (byte c)) s

    let rec parseHier a s =
        if not (matchChar '@' s) then struct(a,s) else
        match tryParseWord (BS.unsafeTail s) with
        | Some (struct(w,s')) -> parseHier (Hier(a,w)) s'
        | None -> struct(a,s)

    let inline private tryParseHash (s:ByteString) : (struct(RscHash * ByteString)) option =
        let struct(h,s') = BS.span (RscHash.isHashByte) s
        if(RscHash.size <> BS.length h) then None else
        Some (struct(h,s')) 

    let tryParseAction (s:ByteString) : (struct(Action * ByteString)) option =
        if BS.isEmpty s then None else
        let c0 = BS.unsafeHead s // looking at one character is sufficient
        if isWordStart c0 then // common words
            match tryParseWord s with
            | Some (struct(w,s')) -> Some (struct(W w,s'))
            | _ -> None
        else if(isNumChar c0) then // natural numbers
            match tryParseNat s with
            | Some (struct(n,s')) when not (startsWith isWordChar s') ->
                // filtering out stuff like `42zed`. Require separation.
                Some (struct(N n, s'))
            | _ -> None
        else if(byte '"' = c0) then // embedded texts
            let struct(txt,s') = BS.span isTextChar (BS.unsafeTail s)
            if (matchChar '"' s') 
                then Some (struct(T txt, BS.unsafeTail s'))
                else None
        else if(byte '(' = c0) then // annotation
            match tryParseWord (BS.unsafeTail s) with
            | Some (struct(w,s')) when matchChar ')' s' ->
                Some (struct(Anno w, BS.unsafeTail s'))
            | _ -> None
        else if(byte ':' = c0) then // record label 
            match tryParseWord (BS.unsafeTail s) with
            | Some (struct(w,s')) -> Some (struct(LPut w, s'))
            | _ -> None
        else if(byte '.' = c0) then // record label access
            match tryParseWord (BS.unsafeTail s) with
            | Some (struct(w,s')) -> Some (struct(LGet w, s'))
            | _ -> None
        else if(byte '%' = c0) then // binary resource
            match tryParseHash (BS.unsafeTail s) with
            | Some (struct(h,s')) -> Some (struct(Bin h, s'))
            | _ -> None
        else if(byte '$' = c0) then // stowage resource
            match tryParseHash (BS.unsafeTail s) with
            | Some (struct(h,s')) -> Some (struct(Ext h, s'))
            | _ -> None
        else None

    let private parseHalt cx p s =
        if (BS.isEmpty s) && (List.isEmpty cx) 
           then ParseOK (List.rev p)
           else ParseErr (struct(p::cx, s))

    let rec parse' cx p s = 
        if (BS.isEmpty s) then parseHalt cx p s else
        let c0 = BS.unsafeHead s
        if (byte ' ' = c0) then
            parse' cx p (BS.unsafeTail s)
        else if (byte '[' = c0) then
            parse' (p :: cx) (List.empty) (BS.unsafeTail s)
        else if (byte ']' = c0) then
            match cx with
            | (pp :: cx') ->
                let a = B (List.rev p)
                let struct(a',s') = parseHier a (BS.unsafeTail s)
                parse' cx' (a' :: pp) s'
            | [] -> parseHalt cx p s
        else 
            match tryParseAction s with
            | Some (struct(a,sa)) ->
                let struct(a',s') = parseHier a sa
                parse' cx (a' :: p) s'
            | None -> parseHalt cx p s

    /// Parse from a ByteString.
    ///
    /// I assume Awelon programs are relatively small, up to a few
    /// dozen kilobytes (given editable views), so stream processing
    /// isn't essential. Larger programs should be broken into small
    /// programs as needed.
    let inline parse (s:ByteString) : ParseResult = 
        parse' (List.empty) (List.empty) s


    // Write program to byte string. Currently this will perform
    // a naive recursive write, so there may be stack issues if
    // the input program is very large or deep.
    //
    // TODO: consider eliminating unnecessary whitespace.
    let rec writeProgram (p:Program) (dst:ByteDst) : unit =
        match p with
        | (a :: p') -> 
            writeAction a dst
            if List.isEmpty p' then () else
            ByteStream.writeByte (byte ' ') dst
            writeProgram p' dst
        | [] -> ()
    and writeAction (a:Action) (dst:ByteDst) : unit =
        match a with
        | B b ->
            ByteStream.writeByte (byte '[') dst
            writeProgram b dst
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






