namespace Awelon
open Data.ByteString

// This module implements a bi-directional program rewrite to support
// local variables and lambdas. I'll want to keep this consistent and
// widely usable, so we'll use the following scheme:
//
// The canonical program will include `(local-x)` annotations for each
// variable. For our view, we rewrite this to `(lambda-x)` then inject
// `x` into the program as a word. After edit, we can restore our
// initial program by finding `(lambda-x)`, rewriting to `(local-x)`
// then extracting the word `x`.
//
// This design allows local vars to be phase-compatible with other views,
// because the input and output are both represented as plain old Awelon
// code. But `(lambda-x)` is not a valid annotation. Properly, we should
// render it in a specialized manner, such as `\x`. This is left to higher
// layer views.
//
// The local variable view involves a simple set of rewrite rules:
//
//     (lambda-x) EXPR == (local-x) T(x,EXPR) 
//       assuming `x` represents a value
//
//     T(x,E) | E does not contain x      => d E
//     T(x,x)                             => 
//     T(x,[E])                           => [T(x,E)] b
//     T(x,F G)                            
//        | only F contains x             => T(x,F) G
//        | only G contains x             => [F] a T(x,G)
//        | F and G contain x             => c [T(x,F)] a T(x,G)
//
// The benefit of local variables is automatic data shuffling. But it
// can result some inefficient code for conditional behaviors. This 
// version will not optimize those, leaving it to our users.
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

