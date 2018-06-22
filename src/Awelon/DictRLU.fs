namespace Awelon
open Data.ByteString

// The DictRLU module augments a dictionary with a reverse lookup
// index, such that we can find the clients for each word in the 
// dictionary. This does require parsing every definition.
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
module DictRLU =

    let parseErrorDep = BS.fromString "ERROR"
    let natPrefix = BS.fromString "NAT-"
    let litPrefix = BS.fromString "LIT-"

    type private NS = Parser.Word list
    type private TokDep = (struct(NS * Parser.Token))

    // utility functions to unfold dependencies in Program
    let rec private stepTokDep nsp =
        match nsp with
        | (struct(ns,p)::nsp') -> stepTokDepP ns p nsp'
        | [] -> None
    and private stepTokDepP ns p nsp' =
        match p with
        | (a::p') -> stepTokDepA ns a (struct(ns,p')::nsp')
        | [] -> stepTokDep nsp'
    and private stepTokDepA ns a nsp' =
        match a with
        | Parser.Block b -> stepTokDepP ns b nsp'
        | Parser.NS (w,a') -> stepTokDepA (w::ns) a' nsp'
        | Parser.Atom tok -> Some (struct(ns,tok),nsp')
    let private seqTokDeps (p:Parser.Program) : seq<TokDep> =
        Seq.unfold stepTokDep (struct([],p)::[])


    



