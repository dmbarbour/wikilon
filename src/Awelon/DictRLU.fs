namespace Awelon
open Data.ByteString

// The DictRLU module augments a dictionary with a reverse lookup
// index, such that we can find the clients for each word in the 
// dictionary. This does require parsing every definition.
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


    



