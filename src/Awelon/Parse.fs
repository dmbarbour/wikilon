namespace Awelon
open Data.ByteString
open System.Collections.Immutable
open Stowage

// Awelon is encoded in ASCII (minus C0 and DEL), and is syntactically
// simple, similar to Forth. A program is a sequence of named operations
// with first-class blocks (which contain programs). For large scale
// Awelon systems, I also permit qualified namespaces, although they
// should only rarely see use.
//
//   Program = (NS Action)*
//   Action = Block | Atom
//   Block = '[' Program ']'
//   NS = (Word '/')* 
//   Atom = Word | Annotation | Nat | Text 
//
//   Word = [a-z][a-z0-9-]* 
//   Annotation = '(' Word ')'
//   Nat = '0' | [1-9][0-9]*
//   Text = '"' (not '"')* '"'
//
// Besides Awelon code, a dictionary may define some symbols with a
// binary reference (via secure hash). In this case, the binary acts
// as a text, but without restrictions on byte use. 
module Parser =

    /// A Word is a ByteString with regex `[a-z][a-z0-9-]*`. 
    type Word = ByteString

    /// A positive Natural number is `[1-9][0-9]*`. 
    /// Not processed into a number type by this module.
    type NatTok = ByteString

    /// Embedded Text in Awelon code.
    type Text = ByteString

    /// A RscHash is from Stowage
    ///
    /// NOTE: We only keep weak references at this parser layer. The
    /// client may need special actions to keep references alive.
    type RscHash = Stowage.RscHash

    /// A Namespace qualifier is a string such as `foo/bar/baz/`.
    /// Usually, it's the empty string. 
    /// In this case, we'll record it as a simple bytestring. The
    /// empty string is the most common.
    type NS = ByteString

    /// Tokens are just typed ByteString fragments. We'll skip the 
    /// annotation parentheses, text quotes, or prefix characters. 
    type TT =
        | Word=0    // word
        | Anno=1    // (anno)
        | Nat=2     // 0 42 128
        | Text=3    // "text"
        | BinRef=4  // %secureHash
        | CodeRef=5 // $secureHash
        //| LblPut=6  // :label
        //| LblGet=7  // .label
    type Token = (struct(TT * ByteString))
    let inline tok tt s = struct(tt,s)
    let inline tokWord w = tok TT.Word w
    let inline tokAnno w = tok TT.Anno w
    let inline tokNat  n = tok TT.Nat n
    let inline tokText t = tok TT.Text t
    let inline tokBinRef h = tok TT.BinRef h
    let inline tokCodeRef h = tok TT.CodeRef h

    /// At this Parser layer, a Program is simply a legal parse. There
    /// is no association with the dictionary or Stowage context.
    type Program = NSAction list
    and NSAction = (struct(NS * Action))
    and Action =
        | Atom of Token         // simple action
        | Block of Program      // [Program]

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
        | TT.Word | TT.Anno -> isValidWord s
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

    /// Parser state, mostly tracks block and namespace context. 
    [<Struct>]
    type ParseState =
        { cx    : ParseState option // parent block parse context
          ns    : Word list     // pending operation's namespace
          p     : Action list   // reverse-ordered list of actions in block
        }
    let inline makeParseState cx ns p = 
        { cx = cx; ns = ns; p = p }
    let inline parsedOp st op =
        let ns_op = wrapNS (st.ns) op // add pending ns to operation
        makeParseState (st.cx) [] (ns_op::(st.p))
    let inline parsedTok st tok = parsedOp st (Atom tok)
    let inline parsedNS st w = makeParseState (st.cx) (w::st.ns) (st.p)

    /// On Parse Error, we'll return the parser's full state.
    /// The lists in this case represent stacks, e.g. `p` is
    /// reverse order relative to the program it represents.
    type ParseResult =
        | ParseOK of Program
        | ParseFail of (ParseState * ByteString)
    
    let private finiParse st s =
        let ok = BS.isEmpty s && Option.isNone (st.cx) && List.isEmpty (st.ns)
        if ok then ParseOK (List.rev (st.p)) else
        ParseFail (st,s)

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
        else if(byte '%' = c0) then // binary resource
            match tryParseHash (BS.unsafeTail s) with
            | Some (struct(h,s')) -> Some (struct(tokBinRef h, s'))
            | _ -> None
        else if(byte '$' = c0) then // stowage resource
            match tryParseHash (BS.unsafeTail s) with
            | Some (struct(h,s')) -> Some (struct(tokCodeRef h, s'))
            | _ -> None
        else None

    /// parse given a full parse state and remaining data. 
    let rec parse' (st:ParseState) (s:ByteString) : ParseResult =
        if BS.isEmpty s then finiParse st s else
        let c0 = BS.unsafeHead s
        if ((byte ' ' = c0) && (List.isEmpty (st.ns))) then
            // whitespace is permitted between actions
            //   but not between namespace and action
            parse' st (BS.unsafeTail s)
        else if(byte '[' = c0) then
            let st' = makeParseState (Some st) [] []
            parse' st' (BS.unsafeTail s)
        else if((byte ']' = c0) && (List.isEmpty (st.ns))) then
            // require empty ns to forbid `foo/bar/]` nonsense.
            match st.cx with
            | Some bcx ->
                let op = Block (List.rev (st.p))
                parse' (parsedOp op bcx) (BS.unsafeTail s)
            | None -> finiParse st s
        else if isWordStart c0 then
            let struct(w,s') = BS.span isWordChar s
            if matchChar '/' s' 
                then parse' (parsedNS st w) (BS.unsafeTail s')
                else parse' (parsedTok st (tokWord w)) s'
        else 
            match tryParseToken s with
            | Some (struct(tok,s')) -> parse' (parsedTok st tok) s'
            | None -> finiParse st s

    /// Parse from a ByteString.
    ///
    /// I assume Awelon programs are relatively small, up to a few dozen
    /// kilobytes due to editable views. If we want to model streaming
    /// code, do so at the dictionary layer to simplify caching, forking,
    /// versioning, review, undo, and editing of the stream. Individual
    /// definitions should still be small.
    let inline parse (s:ByteString) : ParseResult =
        let st0 = makeParseState None [] []
        parse' st0 s

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
        | (op :: p') -> wloopOp cx p' op dst
        | [] -> wloopCx cx dst
    and private wloopOp cx p op dst =
        match op with
        | Block b ->
            ByteStream.writeByte (byte '[') dst
            wloop (p :: cx) b dst
        | NS (struct(w,op')) ->
            ByteStream.writeBytes w dst
            ByteStream.writeByte (byte '/') dst
            wloopOp cx p op' dst
        | Atom tok ->
            writeToken tok dst
            wsp p dst
            wloop cx p dst
    and private wloopCx cx dst =
        match cx with
        | (p' :: cx') ->
            ByteStream.writeByte (byte ']') dst
            wsp p' dst
            wloop cx' p' dst
        | [] -> () // done writing
 
    /// Write a program to a byte stream.
    ///
    /// Note: This writer normalizes spaces, injecting SP between
    /// adjacent actions. `1(nat)[4]b` becomes `1 (nat) [4] b`. I
    /// believe this should have negligible impact on performance.
    let write' p dst = wloop [] p dst

    /// Write to byte string. (Trivially wraps write'.)
    let inline write (p:Program) : ByteString = 
        ByteStream.write (write' p) 


    /// A cursor represents a location within an "open" program. This
    /// is convenient for rewriting a program, or for addending one,
    /// while controlling use of the .Net stack. Entering a block is
    /// recorded with a continuation - an action to perform on exit.
    ///
    /// This cursor is placed between actions, potentially deep in a 
    /// block. It does not track location within a text, word, or the
    /// namespace part.
    [<Struct>]
    type Cursor =
        { bcc : (Program -> Cursor) option // block continuation context
          toL : Action list // reverse-ordered
          toR : Action list // program order
        }

    module Cursor =
        /// place cursor at start of program
        let fromProgram (p:Program) : Cursor =
            { bcc = None; toL = List.empty; toR = p }

        let inline hasLeft (crs:Cursor) : bool =
            not (List.isEmpty (crs.toL))

        let inline hasRight (crs:Cursor) : bool =
            not (List.isEmpty (crs.toR))

        let inline inBlock (crs:Cursor) : bool =
            Option.isSome (crs.bcc)

        let inline putLeft (op:Action) (crs:Cursor) : Cursor =
            { crs with toL = (op::crs.toL) }

        let inline putRight (op:Action) (crs:Cursor) : Cursor =
            { crs with toR = (op::crs.toR) }

        // same as List.append (List.rev xs) dst
        let rec private appendRev xs dst =
            match xs with
            | (x::xs') -> appendRev xs' (x::dst)
            | _ -> dst

        let skipToBlockStart (crs:Cursor) : Cursor =
            { crs with toL = List.empty; toR = appendRev (crs.toL) (crs.toR) }

        let skipToBlockEnd (crs:Cursor) : Cursor =
            { crs with toR = List.empty; toL = appendRev (crs.toR) (crs.toL) }

        let rec private openBlock' ns op =
            match op with
            | Block b -> struct(ns,b)
            | NS(w,op') -> openBlock' (w::ns) op'
            | Atom _ -> failwith "not a block"
        let inline private openBlock op = openBlock' (List.empty) op

        let private blockCC (ns:Word list) (cc:Action -> Cursor) (b:Program) : Cursor =
            cc (wrapNS ns (Block b))
            
        /// enter block with an explicit exit continuation.
        let enterBlockCC (blockOp:Action) (cc:Action -> Cursor) : Cursor =
            let struct(ns,b) = openBlock blockOp
            { bcc = Some (blockCC ns cc)
              toL = List.empty
              toR = b
            }

        /// enters block, on exit will place block to left of cursor.
        let enterBlockExitRight (b:Action) (crs:Cursor) : Cursor =
            enterBlockCC b (fun b' -> putLeft b' crs)

        /// exit block will apply the bcc continuation, handling the
        /// modified block.
        let exitBlock (crs:Cursor) : Cursor =
            match crs.bcc with
            | Some fn -> fn ((skipToBlockStart crs).toR)
            | None -> failwith "not in a block"

        let rec toProgram (crs:Cursor) : Program =
            if inBlock crs then toProgram (exitBlock crs) else
            (skipToBlockStart crs).toR



