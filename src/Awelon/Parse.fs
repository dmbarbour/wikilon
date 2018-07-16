namespace Awelon
open Data.ByteString
open System.Collections.Generic
open System.Collections.ObjectModel
open Stowage

// Awelon is encoded in ASCII (minus C0 and DEL), and is syntactically
// simple, similar to Forth. A program is a sequence of named operations
// with first-class blocks (which contain programs) and namespaces. We 
// also enable a hierarchical dictionary namespace.
//
//   Program = Action*
//   Action = Block | NS '/' Action | Atom
//   Block = '[' Program ']'
//   NS = Word
//   Atom = Word | Annotation | Nat | Text | Resource
//
//   Word = [a-z][a-z0-9-]* 
//   Annotation = '(' Word ')'
//   Nat = '0' | [1-9][0-9]*
//   Text = '"' (not '"')* '"'
//   Resource = BinRef | CodeRef
//   BinRef = '%' RscHash
//   CodeRef = '$' RscHash
//   RscHash = (see Stowage.RscHash)
// 
// This module implements a hand-written parser for Awelon code. 
module Parser =

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
    type Program = Action list  // Action*
        // TODO: consider the ImmutableArray<Action> type, instead.
        // That should be more efficient in many use cases. But the
        // list is simpler to work with from F#.
    and Action =
        | Atom of Token         // simple action
        | Block of Program      // [Program]
        | NS of NSAction        // ns/Action
    and NSAction = (struct(Word * Action))

    /// Test whether an Action is a Block, possibly with namespace.
    let rec isBlock op =
        match op with
        | Block _ -> true
        | NS (ns,op') -> isBlock op'
        | Atom _ -> false

    let rec private wrapNS ns op =
        match ns with
        | (w::ns') -> wrapNS ns' (NS(struct(w,op)))
        | _ -> op

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
    /// while controlling use of the .Net stack.
    ///
    /// This cursor is placed between actions, potentially deep in a 
    /// block. It does not track location within a text, word, or the
    /// namespace part.
    [<Struct>]
    type Cursor =
        { bcx : (struct(Word list * Cursor)) option
          toL : Action list // reverse-ordered
          toR : Action list // program order
        }

    module Cursor =
        let inline hasLeft (crs:Cursor) : bool =
            not (List.isEmpty (crs.toL))

        let inline hasRight (crs:Cursor) : bool =
            not (List.isEmpty (crs.toR))

        let inline inBlock (crs:Cursor) : bool =
            Option.isSome (crs.bcx)

        let inline putLeft (op:Action) (crs:Cursor) : Cursor =
            { crs with toL = (op::crs.toL) }

        let inline putRight (op:Action) (crs:Cursor) : Cursor =
            { crs with toR = (op::crs.toR) }

        let rec private openBlock' ns op =
            match op with
            | Block b -> struct(ns,b)
            | NS(w,op') -> openBlock' (w::ns) op'
            | Atom _ -> failwith "not a block"

        let inline private openBlock op = openBlock' (List.empty) op

        let inline enterBlock' (ns:Word list) (b:Program) (crs:Cursor) : Cursor =
            { bcx = Some(ns,crs)
              toL = List.empty
              toR = b
            }

        let enterBlock (blockOp:Action) (crs:Cursor) : Cursor =
            let struct(ns,b) = openBlock blockOp
            enterBlock' ns b crs

        /// same as List.append (List.rev xs) dst
        let rec appendRev xs dst =
            match xs with
            | (x::xs') -> appendRev xs' (x::dst)
            | _ -> dst

        /// we'll exit to the right of our block
        let exitBlock (crs:Cursor) : Cursor =
            match crs.bcx with
            | Some(ns,crs') ->
                let b = appendRev (crs.toL) (crs.toR)
                putLeft (wrapNS ns (Block b)) crs'
            | None -> failwith "not in a block"

        let skipToBlockStart (crs:Cursor) : Cursor =
            { crs with toL = List.empty; toR = appendRev (crs.toL) (crs.toR) }

        let skipToBlockEnd (crs:Cursor) : Cursor =
            { crs with toR = List.empty; toL = appendRev (crs.toR) (crs.toL) }

        let rec private riseToProgRoot (crs:Cursor) : Cursor =
            if not (inBlock crs) then crs else
            riseToProgRoot (exitBlock crs)

        let skipToProgStart (crs:Cursor) : Cursor = 
            riseToProgRoot crs |> skipToBlockStart

        let skipToProgEnd (crs:Cursor) : Cursor = 
            riseToProgRoot crs |> skipToBlockEnd

        let fromProgram (p:Program) : Cursor =
            { bcx = None; toL = List.empty; toR = p }

        let toProgram (crs:Cursor) : Program =
            (skipToProgStart crs).toR


