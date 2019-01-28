namespace Awelon
open Data.ByteString

// We can support local variables - lets and lambdas of conventional
// languages - using a projection. This LocalVars module provides a
// preliminary and reference implementation, so we can use this view
// early in development.
//
// However, my intention is to define views within Awelon dictionaries,
// where users can modify and extend them. So this shouldn't be in use
// too much. 
module LocalVar =
    open Parser

    module private Impl =
        type Var = Word
        let ppop = BS.fromString "pop-"
        let pvar = BS.fromString "var-"
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
        // annotations.
        let containsWord w prog = cwLoop [] w prog

        let rec appendRev xs dst =
            match xs with
            | (x::xs') -> appendRev xs' (x::dst)
            | [] -> dst

        // for convenient pattern matching of Awelon keywords
        // todo: figure out how to make this parameterized to
        // avoid allocation on matching
        let (|CharOp|_|) op =
            match op with
            | Atom (struct(TT.Word,w)) when (1 = BS.length w)  -> Some (char (w.[0]))
            | _ -> None

        let inline extractAtomSuffix ttReq p op =
            match op with
            | Atom (struct(tt,w)) when (ttReq = tt) && (matchPrefix p w) ->
                Some (BS.drop (BS.length p) w)
            | _ -> None

        // Test for `(var-x)` patterns, extracting 'x'.
        let (|Var|_|) op = extractAtomSuffix (TT.Anno) pvar op

        // Test for `(pop-x)` patterns, extracting 'x'.
        let (|Pop|_|) op = extractAtomSuffix (TT.Anno) ppop op

        let inline isPrimOpC c = ((byte 'd') >= c) && (c >= (byte 'a'))
        let inline isPrimOp w = (1 = BS.length w) && (isPrimOpC (w.[0]))

        // Not all symbols we can represent in `(var-*)` are valid words.
        // And even some valid words might be rejected, if they match our
        // primitive words or are already used within our program.
        let validVar w = (Parser.isValidWord w) && (not (isPrimOp w))
        let acceptVar w p = (validVar w) && (not (containsWord w p))

        type P = Program // a program
        type L = P       // a reverse-ordered program
        type U = P -> P // ad-hoc continuation

        // Inject a singular stack-value variable into a program, 
        // by evaluating the Awelon primitive operators in context.
        //
        // In this case, I keep a reverse-ordered list of operations
        // to the left. This allows efficient processing of `[F] a`
        // block inlining. 
        let rec pushVar (x:Var) (u:U) (l:L) (r:P) : P =
            match r with
            | ((Block b) :: (CharOp 'a') :: r') -> 
                // block will skip over variable + inline content
                pushVar x u (appendRev b l) r'
            | ((Block b) :: (CharOp 'b') :: r') ->
                // bind variable into the current block, keep remainder
                let u' b' = u (appendRev l ((Block b') :: r'))
                pushVar x u' [] b
            | ((CharOp 'c') :: r') ->
                // push variable twice into remaining program
                pushVar x (pushVar x u l) []  r'
            | ((CharOp 'd') :: r') ->
                // drop the variable 
                u (appendRev l r') 
            | _ ->
                // inject the variable
                let xOp = Atom (struct(TT.Word,x))
                u (appendRev l (xOp::r))

        // Rewrite (var-*) entries to (pop-*) from left to right
        let rec pushVars (u:U) (l:L) (r:P) : P =
            match r with
            | ((Var x) :: r') when acceptVar x r' ->
                // push `x` into `r'`, then continue from there.
                let pop_x = Atom (tokAnno (BS.append ppop x))
                pushVar x (pushVars u (pop_x :: l)) [] r'
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
                // extract `x` from r', prepend `(var-x)`
                let var_x = Atom (tokAnno (BS.append pvar x))
                let u' pf = u (appendRev l (var_x :: pf))
                pullVars (pullVar x u' []) [] r'
            | ((Block b) :: r') -> // deep rewrite within blocks
                let ub b' = pullVars u ((Block b')::l) r'
                pullVars ub [] b
            | (op::r') -> pullVars u (op::l) r' // ignore most tokens
            | [] -> u (List.rev l) // all done!


    /// The input is code containing `(pop-x)` tokens, which serve as
    /// placeholders for assigning a variable (not valid annotations).
    /// The output replaces `(pop-x) EXPR` by `(var-x) T(x,EXPR)` with
    /// the following definition of T:
    ///
    ///     T(x,E) | E does not contain x      => d E
    ///     T(x,x)                             => 
    ///     T(x,[E])                           => [T(x,E)] b
    ///     T(x,F G)                            
    ///        | only F contains x             => T(x,F) G
    ///        | only G contains x             => [F] a T(x,G)
    ///        | F and G contain x             => c [T(x,F)] a T(x,G)
    ///
    /// Effectively, this supports local variables within our projection,
    /// but 'compiles' them leaving behind `(var-x)` annotations as naming
    /// hints for later decompilation.
    let hideLocalVars (p:Program) : Program = Impl.pullVars id [] p


    /// Our LocalVars projection is computed by rewriting `(var-x)` 
    /// annotations to `(pop-x) x` then propagating the `x` through
    /// our computation via Awelon's primitive `a b c d` operators.
    /// If `x` is not a valid variable name, we reject the rewrite.
    ///
    /// A higher projection can render `(pop-x)` tokens as `\x` or
    /// another concise and convenient syntax. This would support
    /// lightweight lets and lambdas:
    ///
    ///     [X] \x EXPR         let x = [X] in EXPR         
    ///     [\x EXPR]           (lambda x . EXPR)
    ///
    /// Granted, this doesn't provide an infix notation, and it is
    /// not optimal for conditional behaviors. But use of locals is
    /// more convenient than explicit data shuffling in many cases.
    ///
    /// Note: Developers should be careful about shadowing words used
    /// implicitly in higher projections. 
    let viewLocalVars (p:Program) : Program = Impl.pushVars id [] p



