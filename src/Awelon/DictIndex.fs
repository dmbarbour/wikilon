namespace Awelon
open Data.ByteString
open Parser

// Awelon systems generally require indexed dictionaries.
//
// I can start with two basic indices:
//
//  reverse lookup index:   symbol → client
//  behavior version index: word → version
//
// The reverse lookup index will also track annotations. 
// of a word (e.g. `foo bar` is a continuation of `foo`), but 
// that's probably less critical. We can leverage reverse lookup
// for manual keywords and categories, too.
//
// The version index can track each word together with transitive
// dependencies, i.e. a minimal subtree of the larger dictionary
// to define the word. Basically a tree-hash. Unfortunately, this
// cannot be computed in real-time! So I might wish to compute it
// using some form that tracks intermediate states and allows for
// incremental computation.
//
// Besides these two indices, I also want to support fuzzy-find
// and type-based lookups like Hoogle. Features that would help
// with interactive development. But I suspect this will require
// more specialized work, and might benefit from probabilistic 
// models. I can address it later, as needed.

// DictRLU combines a dictionary with a reverse-lookup index.
module DictRLU =
    open Parser

    /// implicit deps for natural numbers, texts, and binaries.
    let private w_zero : Word = BS.fromString "zero"
    let private w_succ : Word = BS.fromString "succ"
    let private w_null : Word = BS.fromString "null"
    let private w_cons : Word = BS.fromString "cons"

    // we won't look at binaries, just assume they use the full range.
    let private binDeps : Set<Word> =
        Set.ofList [w_null; w_cons; w_zero; w_succ]

    /// we'll simply treat `(anno)` as a word for reverse lookup
    /// dependencies, but we need to add parentheses again.
    let wrapParens (w:Word) : Word =
        let lP = BS.singleton (byte '(')
        let rP = BS.singleton (byte ')')
        BS.append3 lP w rP

    let private addTokDeps (s:Set<Word>) (struct(tt,w) : Token) : Set<Word> =
        match tt with
        | TT.Word -> Set.add w s
        | TT.Anno -> Set.add (annoDep w) s
        | TT.Nat -> 
            let bZero = (1 = BS.length w) && (byte '0' = w.[0])
            if bZero then s |> Set.add w_zero else
            s |> Set.add w_zero |> Set.add w_succ
        | TT.Text -> 
            if BS.isEmpty w then s |> Set.add w_null else
            s |> Set.add w_null |> Set.add w_cons 
              |> Set.add w_zero |> Set.add w_succ
    
    /// compute dependencies for a program.
    let progDeps (p:Program) : Set<Word> =
        Seq.fold addTokDeps (Set.empty) (tokenize p)

    /// parse a program and compute dependencies.
    /// (A program that does not parse has no dependencies.)
    let parseProgDeps (pStr:ByteString) : Set<Word> =
        match parse pStr with
        | ParseOK p -> progDeps p
        | ParseFail _ -> Set.empty

    /// Our reverse-lookup index can be represented using a dictionary
    /// for convenient export. We'll only use the keys from the trie.
    /// A key of form `word@client` means that client references word.
    ///
    /// Annotations are represented in the RLU using `(anno)@client`.
    /// We do not track full texts or exact natural numbers. 
    type RLU = Dict

    module RLU =

        /// Find direct clients with a specific prefix of a word. This is
        /// useful for some application models where we might render a word
        /// as a forum root with "replies" in a threaded model.
        let wordClientsWithPrefix (w:Word) (p:Word) (rlu:RLU) : seq<Word> =
            let wp = BS.append3 w (BS.singleton (byte '@')) p
            let rlu' = Dict.extractPrefix wp rlu
            let onKV ((k,v)) = BS.append p k
            Seq.map onKV (Dict.toSeq rlu')

        /// Find all direct clients of a word.
        let wordClients (w:Word) (rlu:RLU) : seq<Word> =
            wordClientsWithPrefix w (BS.empty) rlu

        /// Find all direct clients of an annotation word.
        /// (Trivially, wraps word in parentheses then calls wordClients.)
        let annoClients (w:Word) (rlu:RLU) : seq<Word> =
            wordClients (wrapParens w) rlu

        
    
    /// A DictRLU is a *consistent* pair of Dict and RLU. Well, we 
    /// assume consistency. Maintaining consistency requires use of
    /// the DictRLU update interface, in general.
    ///
    /// Fortunately, a DictRLU can be maintained incrementally.

(*

//
// NOTE: 
//
// I intend to use a Dict to represent the index. This would enable
// embedding and sharing of indices, where appropriate. Also, use of 
// prefix sharing should ameliorate costs for words with many clients.
module DictX =


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

*)
