namespace Awelon
open Data.ByteString

// We can support local variables - lets and lambdas of conventional
// languages - using a projection. This LocalVars module provides a
// preliminary implementation, albeit without any specialization for
// conditional behaviors or pattern matching. 
//
// Long term, most projections over Awelon should be represented in
// Awelon code. Any specializations can be supported at that layer.
// This implementation is intended as short-term scaffolding, or as
// a debugging fallback. 
module LocalVar =
    open Parser

    module private Impl =
        let ppop = BS.fromString "pop-"
        let plet = BS.fromString "let-"

        let inline matchPrefix p w = 
            (p = (BS.take (BS.length p) w))
        let inline extractPrefix p w =
            if not (matchPrefix p w) then None else
            Some (BS.drop (BS.length p) w)
        let inline matchPopTok v (struct(tt,w)) =
            (TT.Anno = tt)
                && (v = (BS.drop (BS.length ppop) w))
                && (matchPrefix ppop w) 
        let inline matchWordTok tgt (struct(tt,w)) =
            (TT.Word = tt) && (tgt = w)

        // using `cc` to limit stack depth
        let rec cwLoop cc w p =
            match p with
            | ((Atom tok) :: p') ->
                if matchWordTok w tok then true else // word found
                if matchPopTok w tok then cwLoopCC cc w else // word shadowed in p'
                cwLoop cc w p'
            | ((Block b) :: p') -> cwLoop (p'::cc) w b
            | [] -> cwLoopCC cc w
        and cwLoopCC cc w =
            match cc with
            | (p :: cc') -> cwLoop cc' w p
            | [] -> false

        // Test whether a word is contained within a program, albeit
        // in a manner sensitive to name shadowing behind (pop-*)
        // annotations for idempotence.
        let containsWord w prog = cwLoop [] w prog

        type P = Program // a program
        type L = P       // a reverse-ordered program
        type U = P -> P // ad-hoc continuation

        // Test for `(let-x)` patterns, extracting 'x'.
        let inline extractAtomSuffix ttReq p op =
            match op with
            | Atom (struct(tt,w)) when (ttReq = tt) && (matchPrefix p w) ->
                Some (BS.drop (BS.length p) w)
            | _ -> None

        let (|Let|_|) op = extractAtomSuffix (TT.Anno) plet op
        let (|Pop|_|) op = extractAtomSuffix (TT.Anno) ppop op

        let inline isPrimOpC c = ((byte 'd') >= c) && (c >= (byte 'a'))
        let inline isPrimOp w = (1 = BS.length w) && (isPrimOpC (w.[0]))
        let inline acceptLet x p = (isValidWord x) && not ((isPrimOp x) || (containsWord x p))

        let rec appendRev xs dst =
            match xs with
            | (x::xs') -> appendRev xs' (x::dst)
            | [] -> dst

        // For partial evaluation pattern matching, we must easily
        // recognize our `a b c d` operators without allocations.
        let (|CharOp|NotCharOp|) (c:char) op =
            match op with
            | Atom (struct(TT.Word,w)) when (1 = BS.length w) && ((byte c) = w.[0])) -> CharOp
            | _ -> NotCharOp

        // This specifically recognizes the `[[]] a a d` sequence, then
        // returns the suffix if matched.
        let (|InlineP|_|) p =
            match p with
            | (Block [Block []] :: CharOp 'a' :: CharOp 'a' :: CharOp 'd' :: p') -> Some p'
            | _ -> None

        // Inject a variable word into a program. In this case, the `x`
        // variable word corresponds to a block `[x]`. This is performed
        // immediately, so variables are processed in a single pass. 
        // 
        // Inlining a block `[[]] a a d` is recognized directly, so we
        // don't use a full evaluator for this.
        let rec pushVar (x:Word) (u:U) (l:L) (r:P) : P =
            match r with
            | (InlineP r') ->
                // inject `x` inline then return
                u (appendRev l (Atom (tokWord x) :: r'))
            | ((Block ops) :: (CharOp 'a') :: r') -> 
                // apply `[ops]` over `[x]`, continue pushing `[x]`
                pushVar x u (appendRev ops l) r'
            | ((Block ops) :: (CharOp 'b') :: r') ->
                // bind variable into the current block, keep remainder
                let u' ops' = u (appendRev l ((Block ops') :: r'))
                pushVar x u' [] ops
            | ((CharOp 'c') :: r') ->
                // simply push variable twice into remaining program
                pushVar x (pushVar x u l) [] r'
            | ((CharOp 'd') :: r') ->
                // drop the variable and return
                u (appendRev l r') 
            | _ ->
                // inject the variable as `[x]`
                let xOp = Block [Atom (tokWord x)]
                u (appendRev l (xOp::r))

        // Rewrite (let-*) entries to (pop-*) from left to right
        let rec pushVars (u:U) (l:L) (r:P) : P =
            match r with
            | ((Var x) :: r') when acceptVar x r' ->
                // push `x` into `r'`, then continue from there.
                pushVar x (pushVars u ((mkPop x) :: l)) [] r'
            | ((Block b) :: r') -> // deep rewrite within blocks
                let ub b' = pushVars u ((Block b')::l) r'
                pushVars ub [] b
            | (op :: r') -> pushVars u (op::l) r' // skip most tokens
            | [] -> u (List.rev l) // all done!

        let inline private charOp c = 
            c |> byte |> BS.singleton |> Parser.tokWord |> Atom
        let op_a = charOp 'a'
        let op_b = charOp 'b'
        let op_c = charOp 'c'
        let op_d = charOp 'd'

        // add `[F] a` operations if needed to include
        // operators in `s` that do not include variables in output, 
        let private uSkip (u:U) (s:L) : U =
            if List.isEmpty s then u else
            let b = List.rev s
            (fun p -> (Block b :: op_a :: p))

        // extract a singular variable `x` from a program, replacing it
        // with primitive data plumbing to move value into place of `x`.
        //
        // favors `[op1 op2 ...] a` over `[op1] a [op2] a ...`. This has
        // some cost to complexity, which is handled in `u` continuations.
        let rec pullVar (x:Var) (u:U) (s:L) (r:P) : P =
            match r with
            | ((Atom tok) :: r') when matchWordTok x tok ->
                pullVarOps x (uSkip u s) [] r'
            | ((Block b) :: r') when (containsWord x b) ->
                let u' b' = pullVarOps x (uSkip u s) [Block b'; op_b] r'
                pullVar x u' [] b
            | (op::r') -> pullVar x u (op::s) r' // op does not use x
            | [] -> u (op_d :: (List.rev s)) // unused variable
        and pullVarOps x u varOps r =
            if containsWord x r then
                let uCopy r' = u (op_c :: r')
                pullVar x uCopy (List.rev varOps) r
            else u (List.append varOps r)

        // Extract (pop-*) entries from right to left.
        let rec pullVars (u:U) (l:P) (r:P) : P =
            match r with
            | ((Pop x) :: r') when validVar x -> 
                // extract `x` from r', prepend `(let-x)`
                let u' pf = u (appendRev l ((mkLet x) :: pf))
                pullVars (pullVar x u' []) [] r'
            | ((Block b) :: r') -> // deep rewrite within blocks
                let ub b' = pullVars u ((Block b')::l) r'
                pullVars ub [] b
            | (op::r') -> pullVars u (op::l) r' // ignore most tokens
            | [] -> u (List.rev l) // all done!


    /// The input is code containing `(pop-x)` tokens, which serve as
    /// placeholders for assigning a variable. The output replaces 
    /// `(pop-x) EXPR` by `(let-x) T(x,EXPR)` with transform `T` such
    /// that `[x] T(x,EXPR) == EXPR` independent of dictionary.
    ///
    ///     T(x,[x])                           => 
    ///     T(x,x)                             => [[]] a a d
    ///     T(x,E) | E does not contain x      => d E
    ///     T(x,[E])                           => [T(x,E)] b
    ///     T(x,F G)                            
    ///        | only F contains x             => T(x,F) G
    ///        | only G contains x             => [F] a T(x,G)
    ///        | F and G contain x             => c [T(x,F)] a T(x,G)
    ///
    /// We use Awelon primitive words `a b c d` to ensure independence
    /// from the dictionary. This transform is not the most concise for
    /// the `T(x,x)` case, but it's simple, robust, and consistent.
    ///
    /// See `viewLocalVars` for the other direction.
    let hideLocalVars (p:Program) : Program = Impl.pullVars id [] p

    /// Given source code containing `(let-x)` annotations, we rewrite
    /// to `(pop-x) [x]` then partially evaluate `[x]` using Awelon's 
    /// primitive `a b c d` operations. This is much weaker than a full
    /// evaluator, and only needs to reverse hideLocalVars. 
    /// 
    /// For safety, we omit the rewrite if `x` would be ambiguous.
    ///
    /// I assume the `(pop-x)` annotations will be further projected
    /// for concision and aesthetics, perhaps as `\x`. This supports
    /// lightweight lets and lambdas:
    ///
    ///     [X] \x EXPR         let x = X in EXPR
    ///     [\x EXPR]           (lambda x -> EXPR)
    ///
    /// Local variables can simplify data plumbing, especially when 
    /// deep closures become involved. However, it should be used
    /// carefully around conditional behaviors to avoid copying the
    /// variable into each conditional path.
    let viewLocalVars (p:Program) : Program = Impl.pushVars id [] p


