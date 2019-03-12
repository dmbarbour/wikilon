namespace Awelon
open Data.ByteString
open Stowage

// Awelon is encoded in printable ASCII (excluding C0 and DEL). Awelon is
// syntactically very simple, similar to Forth. A program is represented 
// by a sequence of atoms and blocks which contain first-class programs.
// Each atom and program represents a function in Awelon. The meaning 
// depends on a contextual dictionary.
//
// Program = (Block | Token)*
// Block = '[' Program ']'
// Token = Word | Annotation | Nat | Text
// Word = Frag('-'Frag)*
// Frag = [a-z]+(Nat)?
// Annotation = '(' Word ')'
// Nat = '0' | [1-9][0-9]*
// Text = '"' (not '"')* '"'
//
// Words in Awelon are lower-case, with internal hyphens, and may have
// natural numbers at the end of each word fragment. This is designed
// to be relatively friendly for URLs, managing volumes of a dictionary,
// and unambiguous in projections that might introduce some punctuation.
// (But Awelon is sacrificing '-' for subtraction.)
//
// Tokens can be separated by spaces (SP) as needed to disambiguate.
// Whitespace effectively has identity semantics; it may be ignored.
// Formatting is not part of Awelon definitions, and is usually not
// preserved within the saved dictionary. Instead, it's left to our
// projectional editing environments to format and present code. 
//
// Texts do not use C0, DEL, or escape characters. Developers can
// work around this with projections, or use binary large objects
// via the dictionary which are not parsed at all.
//
// TODO: This parser inefficient. Might be worth developing a more
// optimal parser, perhaps using vectors instead of lists.
module Parser =

    /// A Word is represented by a ByteString but should have some
    /// internal structure according to regex: 
    ///
    ///    Word = Frag('-'Frag)*
    ///    Frag = [a-z]+(Nat)?  
    ///    Nat  = '0' | [1-9][0-9]*
    ///
    /// That is, a word can be some text, optionally followed by a
    /// number, optionally followed by a hyphenated word. (We might
    /// make the hyphens optional, but for  
    /// has some lower-case characters followed by an optional natural
    /// number.
    type Word = ByteString

    /// A Natural number is a token of form `'0' | [1-9][0-9]*`. 
    /// This parser just keeps numbers in token form.
    type NatTok = ByteString

    /// Embedded Text in Awelon code.
    type Text = ByteString

    /// Tokens are just typed ByteString fragments. We'll skip the 
    /// annotation parentheses or text quotes. 
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

    let inline private isDigit c = (byte '9' >= c) && (c >= byte '0')
    let inline private isLowerAlpha c = (byte 'z' >= c) && (c >= byte 'a')
    let isWordChar c = (isLowerAlpha c) || (isDigit c) || ((byte '-') = c)
    let inline private isTextChar c = (126uy >= c) && (c >= 32uy) && (c <> (byte '"'))

    /// Return count of bytes that parse as natural number. ('0' | [1-9][0-9]*).
    let parseNatLen s = 
        if (BS.isEmpty s) then 0 else
        let c0 = BS.unsafeHead s
        if ((byte '0') = c0) then 1 else
        BS.length (BS.takeWhile isDigit s)

    /// Validate an alleged natural number token.
    let inline isValidNat (n:NatTok) : bool =
        (not (BS.isEmpty n)) && ((BS.length n) = (parseNatLen n))

    /// Return count of bytes in a word fragment [a-z]+(Nat)?
    let parseWordFragLen s =
        let struct(az,rem) = BS.span isLowerAlpha s
        if BS.isEmpty az then 0 else
        (BS.length az) + (parseNatLen rem)

    let inline private hasWordExt s =
        ((BS.length s) >= 2) 
            && ((byte '-') = (s.[0])) 
            && (isLowerAlpha (s.[1]))

    // tail-recursive loop for word length
    let rec private parseWordLenLoop acc s =
        let wfLen = parseWordFragLen s
        if (0 = wfLen) then acc else
        let acc' = wfLen + acc
        let s' = BS.drop wfLen s
        if (not (hasWordExt s')) then acc' else
        parseWordLenLoop (1 + acc') (BS.unsafeTail s')

    /// Return count of bytes that match a valid word. 
    let parseWordLen s =  parseWordLenLoop 0 s 
    
    let rec private parseWordLenLoop acc s =
        let struct(az,rem) = BS.span isLowerAlpha s
        if BS.isEmpty az then acc else
        let nlen = parseNatLen rem
        let s' = BS.drop nlen rem
        let acc' = acc + (BS.length az) + nlen
        if not (hasWordExt s') then acc' else
        parseWordLenLoop (1 + acc') (BS.unsafeTail s')

    let parseWordLen s = parseWordLenLoop 0 s

    /// Validate an alleged word token.
    let inline isValidWord (w:Word) : bool =
        (not (BS.isEmpty w)) && ((BS.length w) = (parseWordLen w))

    /// Return number of valid embedded-text bytes within a string.
    /// (Texts allow ASCII modulo C0, DEL, and double quotes (34).)
    let parseTextLen s = BS.length (BS.takeWhile isTextChar s)

    /// Validate an alleged text token. Empty is permitted.
    let inline isValidText s = ((parseTextLen s) = (BS.length s))

    /// Validate an arbitrary token. 
    let isValidToken (struct(tt,s) : Token) : bool =
        match tt with
        | TT.Word | TT.Anno -> isValidWord s
        | TT.Text -> isValidText s
        | TT.Nat -> isValidNat s
        | _ -> invalidArg "tt" "unrecognized token type"

    let inline private startsWith c s =
        (not (BS.isEmpty s)) && (c = (BS.unsafeHead s))
    let inline private splitAt ix s = struct(BS.take ix s, BS.drop ix s)
    let inline private splitFn fn s = splitAt (fn s) s
    let inline private hasWordSep s = (BS.isEmpty s) || (not (isWordChar (BS.unsafeHead s)))

    /// Attempt to parse a token from start of bytestring input.
    /// We only lookahead one byte at most to determine token type.
    /// We will reject words and numbers that are directly adjacent. 
    let tryParseToken (s:ByteString) : (struct(Token * ByteString)) option =
        if (BS.isEmpty s) then None else
        let c0 = BS.unsafeHead s
        if isLowerAlpha c0 then
            let struct(w,rem) = splitFn parseWordLen s
            let ok = hasWordSep rem
            if not ok then None else
            Some (struct(tokWord w, rem))
        else if isDigit c0 then
            let struct(n,rem) = splitFn parseNatLen s
            let ok = hasWordSep rem
            if not ok then None else
            Some (struct(tokNat n, rem))
        else if (c0 = (byte '"')) then
            let struct(t,xrem) = splitFn parseTextLen (BS.unsafeTail s)
            let ok = startsWith (byte '"') xrem 
            if not ok then None else
            Some (struct(tokText t, BS.unsafeTail xrem))
        else if (c0 = (byte '(')) then
            let struct(w,xrem) = splitFn parseWordLen (BS.unsafeTail s)
            let ok = (startsWith (byte ')') xrem) && (not (BS.isEmpty w))
            if not ok then None else
            Some (struct(tokAnno w, BS.unsafeTail xrem))
        else None
    
    /// On a successful parse, we'll return the program. If there is
    /// an error, however, we'll return a parse context as a stack of
    /// open subprograms in reverse order, and remaining input.
    type ParseResult =
        | ParseOK of Program
        | ParseFail of ((Action list) list * ByteString)

    let private finiParse p b s =
        let ok = BS.isEmpty s && List.isEmpty p
        if ok then ParseOK (List.rev b) else
        ParseFail (b::p,s)

    let rec private parseLoop p b s : ParseResult =
        if BS.isEmpty s then finiParse p b s else
        let c0 = BS.unsafeHead s
        if (byte ' ' = c0) then // ignore whitespace
            parseLoop p b (BS.unsafeTail s)
        else if(byte '[' = c0) then // push block context
            parseLoop (b::p) [] (BS.unsafeTail s)
        else if(byte ']' = c0) then // pop block context
            match p with
            | (bp::p') -> 
                let b' = ((Block (List.rev b))::bp)
                parseLoop p' b' (BS.unsafeTail s)
            | [] -> finiParse p b s // imbalance of ']'
        else // parse an atomic token
            match tryParseToken s with
            | Some (struct(tok,s')) -> parseLoop p ((Atom tok)::b) s'
            | None -> finiParse p b s

    /// Parse from a ByteString.
    ///
    /// This parser isn't spectacularly efficient, mostly due to the
    /// intermediate allocation and destruction of F# lists. But it
    /// requires no backtracking, and parsing is unlikely to become 
    /// the performance bottleneck in an Awelon system.
    let parse (s:ByteString) : ParseResult = parseLoop [] [] s

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

    let inline private wsp p dst =
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
    /// This writer blindly introduces spaces between elements of the
    /// program. This can be considered a normalization of whitespace.
    let write' p dst = wloop [] p dst

    /// Write to byte string. (Trivially wraps write'.)
    let inline write (p:Program) : ByteString = 
        ByteStream.write (write' p)


    module private Tokenizer = 
        type TC = (struct(Program list * Program))
        let rec step (struct(u,r) : TC) : (Token * TC) option =
            match r with
            | (op::r') -> 
                match op with
                | Atom tok -> Some (tok, struct(u,r'))  // report token
                | Block b -> step (struct((r'::u),b))   // step into block
            | [] -> 
                match u with
                | (r'::u') -> step (struct(u',r'))      // step out of block
                | [] -> None                            // done

    /// Extract sequence of Tokens from a parsed program, ignoring block
    /// structure. In practice, this is only used for indexing programs,
    /// and a few projections. 
    let tokenize (p:Program) : seq<Token> =
        Seq.unfold (Tokenizer.step) (struct([],p))

    /// Validate a program. Block structure is valid via the F# Program
    /// type, but this will validate the individual tokens.
    let isValidProgram (expr:Program) : bool =
        Seq.forall isValidToken (tokenize expr)

    module private RewriteTokens =
        type RW = Token -> Program
        type U = Program -> Program
        type L = Program // reverse-ordered
        type R = Program

        // Thoughts: F# uses a small stack, so I'm using CPS (via type U)
        // to avoid the stack when processing recursive block structure.
        let rec step (rw:RW) (u:U) (l:L) (r:R) : Program =
            match l with
            | (op::l') -> 
                match op with
                | (Atom tok) -> 
                    step rw u l' (List.append (rw tok) r)
                | (Block b) -> 
                    let ub b' = step rw u l' ((Block b') :: r)
                    stepIni rw ub b
            | [] -> r
        and stepIni rw u p = step rw u (List.rev p) []

    /// Rewrite every token within a program. Each token may be independently
    /// expanded or erased based on a given context-independent rewrite rule.
    /// This has limited potential utility for cases such as:
    ///
    ///  * renaming a word or set of words
    ///  * inlining a word's definitions
    ///  * erasing undesirable annotations
    /// 
    /// Mostly, it's intended for renaming words.
    let tokenRewrite (rw:(Token -> Program)) (p:Program) : Program =
        RewriteTokens.stepIni rw id p 

