namespace Awelon
open Data.ByteString

// This module will implement a bi-directional program rewrite:
//
//     EXPR == X T(X,EXPR) for value X
//
//     T(X,E) | E does not contain X      => d E
//     T(X,X)                             => 
//     T(X,[E])                           => [T(X,E)] b
//     T(X,F G)                            
//        | only F contains X             => T(X,F) G
//        | only G contains X             => [F] a T(X,G)
//        | F and G contain X             => c [T(X,F)] a T(X,G)
//
module LocalVar =
    open Parser

    /// For simplicity, local variables are a subset of words.
    ///
    /// We may restrict to a common prefix like `var-*` to avoid
    /// naming conflicts and simplify projectional editing with
    /// variables. However, this is not required.
    type Var = Word

    module private Ops = 
        let inline wOp w = Atom (tokWord w)
        let inline cOp c = wOp (BS.singleton (byte c))
        let opApply = cOp 'a'
        let opBind  = cOp 'b'
        let opCopy  = cOp 'c' 
        let opDrop  = cOp 'd'

        let (|CharOp|_|) op =
            match op with
            | Atom (struct(TT.Word,w)) when (1 = BS.length w) -> Some (char (w.[0]))
            | _ -> None

    open Ops

    let rec private appendRev xs dst =
        match xs with
        | (x::xs') -> appendRev xs' (x::dst)
        | _ -> dst

    // add a variable with cursor context (l u r ~ left up right)
    let rec private pushVar (var:Var) (l:Program) (u:(Program -> Program)) (r:Program) : Program =
        match r with
        | ((Block b) :: (CharOp 'a') :: r') ->
            // block skips over variable, bind remainder
            pushVar var (appendRev b l) u r
        | ((Block b) :: (CharOp 'b') :: r') ->
            // bind variable into the current block, don't touch remainder
            let u' b' = u (appendRev l ((Block b') :: r'))
            pushVar var [] u' b
        | ((CharOp 'c') :: r') ->
            // bind variable twice into remaining program
            pushVar var [] (pushVar var l u) r'
        | ((CharOp 'd') :: r') -> u (appendRev l r') // drop variable
        | _ -> u (appendRev l ((wOp var)::r)) // inject variable
            
    /// Rewrite a program to inject a variable at head.
    let injectVar (var:Var) (expr:Program) : Program =
        pushVar var [] id expr

    let private matchVarTok (var:Var) (struct(tt,w):Token) : bool =
        (tt = TT.Word) && (w = var)

    /// Test whether a variable is contained within a program.
    let containsVar (var:Var) (expr:Program) : bool =
        Seq.exists (matchVarTok var) (tokenize expr)

    // extract a variable from a program.
    let rec private pullVar (var:Var) (l:Program) (u:(Program->Program)) (r:Program) : Program =
        // l: to skip (reversed), u: continuation, r: to right
        match r with
        | ((Atom (TT.Word,w)) :: r') when (var = w) ->
            pullVarOps var l u [] r'
        | ((Block b) :: r') when (containsVar var b) ->
            let u' b' = pullVarOps var l u [Block b'; tokWord opBind] r'
            pullVar var [] u' b
        | (op::r') -> pullVar var (op::l) u r'
        | [] -> u ((tokWord opDrop) :: (List.rev l))
    and private pullVarOps var l u vOps r =
        let u' = if List.isEmpty l then u else
                 let b = List.rev l
                 // add `[F] a` to skip variable
                 (fun p -> u ((Block b) :: (tokWord opApply) :: p))
        if containsVar var r then
            let uCopy r' = u' ((tokWord opCopy) :: r') // var copy operation
            pullVar var (List.rev vOps) uCopy r // var ops skip one var copy
        else u' (List.append vOps r) // done, elements to left added by u'.

    let extractVar (var:Var) (expr:Program) : Program =
        pullVar var [] id expr

    // TODO: check for 'safety' of variable injection and extraction.
    //
    // Safety means there is no shadowing: local variables are unique
    // within a scope, and do not overlap with Awelon words used in
    // the same function. Further, we might validate separately for 
    // injection vs. extraction.

    // TODO: annotation-driven extraction and injection of multiple vars.
    // Perhaps use (local-varname) vs. (lambda-varname). 

