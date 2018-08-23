namespace Awelon
open Data.ByteString

// Awelon systems generally require indexed dictionaries.
//
// We start with two basic indices:
//
//  reverse lookup index:   symbol → client
//  behavior version index: word → version
//
// The behavior version index gives us a unique version ID for the
// deep definition of a word, including all of its dependencies.
// This version ID can then be used to cache computations on the
// definition's behavior such as type and evaluation. We might 
// further compute a behavior version for evaluated behaviors.
// 
// The reverse lookup index is necessary to compute the version
// index, but also is independently valuable for providing links
// when browsing or editing a dictionary. Some special attention
// is required for indexing our secure-hash resources, and perhaps
// for tracking resources that weren't immediately available.
//
// Ideally, indexing is real-time and occurs in the background,
// hence features such as laziness and incremental computation
// are desirable. To this end, our behavior version index will
// support a queue for elements we still need to index. 
//
// NOTE: 
//
// I intend to use a Dict to represent the index. This would enable
// embedding and sharing of indices, where appropriate. Also, use of 
// prefix sharing should ameliorate costs for words with many clients.
module DictX =

    /// A qualified word is basically a word together with a qualified
    /// namespace. We'll index fully qualified words, e.g. for the 
    /// program `d/[foo bar]` we must record dependencies on `d/foo`
    /// and `d/bar`. Each namespace component should also be a valid
    /// word. A qualified word will not contain SP or LF.
    ///
    /// Besides Awelon words, we might include a few more symbols to
    /// help track special dependencies such as secure hash resources.
    type QW = ByteString

    /// Our namespace is simply the fragment of a qualified word up
    /// to and including the final `/`.
    type NS = ByteString

    let cNSSep = byte '/'
    let private notNSSep c = (c <> cNSSep)
    let wordNS (qw:QW) : NS = BS.dropWhileEnd notNSSep qw
    let parentNS (ns:NS) : NS = wordNS (BS.dropLast 1 ns)
    let childNS (ns:NS) (w:Parser.Word) : NS = BS.append3 ns w (BS.singleton cNSSep)

    /// implicit dependencies for natural numbers and texts:
    /// zero, succ, null, cons
    let word_zero : Parser.Word = BS.fromString "zero"
    let word_succ : Parser.Word = BS.fromString "succ"
    let word_null : Parser.Word = BS.fromString "null"
    let word_cons : Parser.Word = BS.fromString "cons"

    /// Unqualified dependencies for a natural number in Awelon.
    let natDeps (n:Parser.NatTok) : QW list =
        let isZero = (1 = BS.length n) && (byte '0' = n.[0])
        if isZero then [word_zero] else [word_zero; word_succ]

    let private wrapAnno (w:Parser.Word) =
        BS.append3 (BS.singleton (byte '(')) w (BS.singleton (byte ')'))

    /// Unqualified dependencies for an annotation. We'll simply use
    /// the parenthetical annotation, in this case. 
    let annoDeps (anno:Parser.Word) : QW list = [wrapAnno anno]

    /// Unqualified dependencies for an embedded text in Awelon.
    let litDeps (txt:Parser.Text) : QW list =
        // empty string is just the null dependency
        if BS.isEmpty txt then [word_null] else
        let baseDeps = [word_null;word_cons;word_zero;word_succ] 
        // consider adding tokens for full-text search!
        baseDeps

    // TODO: Resource dependencies. (How shall we handle deep dependencies?)
    // 


    /// Given a text string and namespace, compute a list of word-like

    module private StepTok =
        type Tok = (struct(NS * Parser.Token))
        type CX = (struct(NS * Parser.Program)) list
        let rec stepCX (cx:CX) : (Tok * CX) option =
            match cx with
            | (struct(ns,p)::cx') -> stepP ns p cx'
            | [] -> None
        and stepP ns p cx =
            match p with
            | (op::p') -> stepOp ns op (struct(ns,p')::cx)
            | [] -> stepCX cx
        and stepOp ns op cx =
            match op with
            | Parser.Block b -> stepP ns b cx
            | Parser.NS (w,op') -> stepOp (childNS ns w) op' cx
            | Parser.Atom tok -> Some(struct(ns,tok),cx)

    let private tokDeps (ns:NS) (tok:Parser.Token) : QW list =
        failwith "not implemented"

    let rec private stepQW (struct(lst,cx)) =
        match lst with
        | (qw::lst') -> Some(qw, struct(lst',cx))
        | [] ->
            match StepTok.stepCX cx with
            | Some (struct(ns,tok),cx') -> 
                let deps = tokDeps ns tok
                stepQW (struct(deps,cx'))
            | None -> None

//
// Basically, this is represented using a meta dictionary with one
// :word.client entry for each client of a word. This allows for 
// `extractPrefix` to find all clients of a word.
//
// To deal with the hierarchical dictionaries, it's sufficient to 
// include the entries at the appropriate hierarchical layer. Like:
//
// - RLU/word.client        word has client in same dictionary
// - dict/RLU/word.client   dict internally has word as client
// - RLU/dict/word.client   dict/word has an external client
//
// To find all clients of foo/bar/baz, we look at three prefixes:
// 
// - foo/bar/RLU/baz.       internal clients of baz
// - foo/RLU/bar/baz.       clients of bar/baz within foo
// - RLU/foo/bar/baz.       clients of foo/bar/baz
//
// For implementations, we could either try to maintain our RLU index
// within our primary Awelon dictionary, or separately as a companion
// dictionary. The former option is convenient for sharing, but also
// introduces risk that there are errors in the index, and hinders
// some extensions of the reverse lookup.
//
// The `RLU/` could be represented as another `.` without ambiguity.
//
// Besides words, we can index annotations like (par), natural numbers,
// text fragments or words within texts, and so on. We'll also track any
// words whose definitions fail to parse. Secure hash resources are also
// tracked as dependencies, but are not locally indexed as clients.
//
// Note: I should table this for now - priority is relatively low. And
// it might be necessary to support lazy indexing.
        
        


    /// Reverse Dependency Lookup Index.
    ///
    /// Represented as a dictionary with special structure. Assume
    /// a scenario such as:
    ///
    ///   :foo bar d/baz
    ///
    /// In this case, we want to find `foo` given a reference to `bar`
    /// or `d/baz`. So we'll add two entries to our reverse lookup:
    ///
    ///   :^bar.foo
    ///   :^d/baz.foo
    ///
    /// The carrot `^` indicates a shared hierarchical namespace for
    /// client and dependency, to improve structure sharing. If we 
    /// have `d/^bar.foo`, that means `d/bar` depends on `d/foo`.
    ///
    /// To keep it simple, this reverse lookup index is focused on
    /// Awelon words.
    /// index for something like full-text search (one that has more
    /// support for multi-dictionary searches). 
    type RLU = Dict

    type DictX =
        { raw : Dict    // word → definition
          rlu : Dict    // symbol → clients
          ver : Dict    // 
        }


