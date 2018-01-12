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

    let inline hasWordStart r =
        (not (BS.isEmpty r)) && (isWordStart (BS.unsafeHead r))

    let inline hasWordSep r =
        (BS.isEmpty r) || not (isWordChar (BS.unsafeHead r))

    let tryParseWord (r:ByteString) : (struct(Word * ByteString)) option =
        if not (hasWordStart r) then None else
        Some (BS.span isWordChar r)

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

    /// This accepts `0 | [1-9][0-9]*` but rejects if not separated from
    /// valid word characters.
    let tryParseNat (r : ByteString) : (struct(Nat * ByteString)) option =
        let struct(ns,r') = BS.span isNumChar r
        let accept = not (BS.isEmpty ns) && hasWordSep r'
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
        | LW of Word        // :word
        | LR of Word        // .word
        | Anno of Word      // (word)
        | Bin of RscHash    // %secureHash
        | Ext of RscHash    // $secureHash
        | Hier of Action * Word // Action @ Word
    and Program = Action list

    // Parser Notes:
    //
    // - most programs should be relatively short
    // - parse from bytestring, no need for streaming
    // 
    // The current parser allocates more than I'd prefer, which will
    // hurt performance somewhat. But it shouldn't be a huge problem.
    // It's something I can try to resolve later.

    /// An incomplete program is represented by a stack of reversed programs.
    /// E.g. `foo [bar baz [qux` should produce `[qux; baz bar; foo]`.
    type IncompleteProg = Program list

    /// Parse result is either a valid program or a partial program
    /// and remaining bytes.
    type ParseResult =
        | ParseOK of Program
        | ParseErr of IncompleteProg * ByteString

    let private parseHalt cx p s =
        if (BS.isEmpty s) && (List.isEmpty cx) 
           then ParseOK (List.rev p)
           else ParseErr (p::cx, s)

    let rec parseHier a s =
        if (BS.isEmpty s) || (byte '@' <> BS.unsafeHead s) then struct(a,s) else
        match tryParseWord (BS.unsafeTail s) with
        | Some (struct(w,s')) -> parseHier (Hier(a,w)) s'
        | None -> struct(a,s)

    let tryParseAction (s:ByteString) : (struct(Action * ByteString)) option =
        raise (System.NotImplementedException "todo: parse actions")

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

    let inline parse (s:ByteString) : ParseResult = 
        parse' (List.empty) (List.empty) s





