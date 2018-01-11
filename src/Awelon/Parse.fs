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
        isAlpha (BS.unsafeHead w) && BS.forall isWC (BS.unsafeTail w)

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

    /// This accepts `0 | [1-9][0-9]*` followed by a word separator.
    let tryParseNat (r : ByteString) : (struct(Nat * ByteString)) option =
        let struct(ns,r') = BS.span isNumChar r
        let accept = not (BS.isEmpty ns) && hasWordSep r'
                  && ((byte '0' <> BS.unsafeHead ns) || (1 = BS.length ns))
        if accept then Some (parseNat ns) else None

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

    // NOTE: Improving the 'list' type used here could potentially
    // offer significant efficiency benefits. But it is not a high
    // priority at this time.

    let annoError = Anno (BS.fromString "error")

    // Parser Notes:
    //
    // - most programs should be relatively short
    // - parse from bytestring, streaming is rare
    // 
    // The current parser allocates more than I'd prefer, which will
    // hurt performance somewhat. But it shouldn't be a huge problem.
    // It's something I can try to resolve later.

    /// A ParseState is returned for partial parsing.
    [<Struct>]
    type ParseState = 
        val program : Program // reverse ordered, current block
        val context : Program list // stack of open blocks
        val remainder : ByteString // unprocessed bytes
        val wsep : bool // parsed a clean word separator (SP or '[' or ']')
        new(p,cx,r,ws) = { program = p; context = cx; remainder = r; wsep = ws }

    let parseInit (s:ByteString) : ParseState = 
        new ParseState(List.empty,List.empty,s,true)
    let parseAddend (ps:ParseState) (s:ByteString) : ParseState =
        new ParseState(ps.program, ps.context, BS.concat (ps.remainder) s, ps.wsep)

    /// Returns true iff the parse is in a valid final state.
    /// This means: no unparsed data, all blocks are closed. 
    let inline parseOK (ps:ParseState) : bool =
        BS.isEmpty (ps.remainder) && List.isEmpty (ps.context) 

    /// Return a program from a parse state. This will always
    /// succeed (assuming sufficient memory), but an invalid
    /// program is truncated and silently addends `(error)`.
    let parsedProg (ps:ParseState) : Program =
        if parseOK ps then List.rev (ps.program) else
        let rec truncateProg p s =
            match s with
            | (pp::s') -> truncateProg (B (List.rev p) :: pp) s'
            | [] -> List.rev (annoError :: p)
        truncateProg (ps.program) (ps.context)

    /// Parse hierarchical dictionary route.
    let rec parseHier (a:Action) (r:ByteString) : struct(Action * ByteString) =
        
        if (BS.isEmpty r) || (byte '@' <> BS.unsafeHead r) then struct(a,r) else
        match tryParseWord
        
        
        

    let rec private parse' p cx r =
        if BS.isEmpty rem then ParseState(p,cx,r) else
        let c0 = BS.unsafeHead r
        if (byte ' ' = c0) then
            parse' p cx (BS.unsafeTail r)
        else if (byte '[' = c0) then
            parse' (List.empty) (p::cx) (BS.unsafeTail r)
        else if (byte ']' = c0) then
            match cx with
            | (pp::cx') -> 
                let a = B (List.rev p)
                let struct(a',r') = parseHier a (BS.unsafeTail r)
                parse' (a' :: pp) cx' r'
            | [] -> ParseState(p,cx,r)
        else 
            match tryParseAtom r with
            | Some (struct(a,ra)) -> 
                let struct(a',r') = parseHier a ra
                


parseH a p cx r'
            | None -> ParseState(p,cx,r) // invalid code
    
    

if isWordStart c0 then
            let rwe = skipWordEnd tl
            let wlen = BS.length r - BS.length r'
            parseH (W (BS.take wlen r)) p cx r'
            let w = BS.take wlen r
            let struct(a,r') = parseH (W w) rwe
            let w = BS.take (BS.length r - BS.length r') r
            let wlen = 1 + BS.length (BS.takeWhile isWordChar tl)
            let w = BS.take wlen r
            let r' = BS.drop wlen r
            
        else if (isNumChar c0) then
            let natLen = 1 + BS.length (BS.takeWhile isNumChar (BS.unsafe

if (isWordStart c0) then
        


    /// Parse operation.
    ///
    /// This evaluates/rewrites a ParseState, parsing as much as
    /// possible. Use `parseOK` and `parsedProg` to observe the
    /// result.
    let inline parse (ps:ParseState) : ParseState =
        parse' (ps.context) (ps.program) (ps.remainder)
    

        
   

