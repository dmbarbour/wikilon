namespace Awelon
open Data.ByteString

// We can support local variables - lets and lambdas of conventional
// languages - using a projection. 
//
// A simple proposal: introduce `\x` tokens that will "pop" a value 
// from the stack into a local variable `x` for the remainder of a
// finite program (a definition or block). Then `[X] \x EXPR` serves
// equivalent to `let x = [X] in EXPR`, and `[\x EXPR]` corresponds 
// to conventional `(lambda x . EXPR)`. 
//
// This LocalVars module provides a reference implementation for 
// this convenient feature. To ensure compatibility with higher
// views, we use the Awelon syntax and write `(lambda-x)` instead
// of `\x`. I assume (lambda-x) is specific to this view, not a
// valid annotation. Higher layers may render as `\x` if desired.
//
// I assume we have `(local-x)` annotations in Awelon definitions.
// This roughly means: for the remainder of this program, give the
// top stack element the name `x`. Might be useful for debugging.
// But it's mostly intended for local variables: to view locals, we
// rewrite `(local-x)` to `(lambda-x) x` then propagate `x` over 
// the fundamental Awelon operators  `a b c d`. Conversely, we can
// extract `(lambda-x) EXPR` by injecting some Awelon operators.
//
// The extraction rules are simple enough:
//
//     (pop-x) EXPR == (local-x) T(x,EXPR) 
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
// For the multi-variable case, we must extract in order of rightmost
// lambda to leftmost to ensure we don't mangle our variable scopes.
//      
//      (lambda-x) (lambda-y) x y           
//      (lambda-x) (local-y) [x] a
//      (local-x) [(local-y)] a [] b a
//
// This leaves an awkward `[(local-y)]`. Viewed locally, this is the
// trivial `[(lambda-y) y]`, but we'll normally want to propagate the
// `y` back into the parent scope. Thus, processing of `(local-*)` 
// annotations to compute the locals view should be left-to-right.
//
// Complicated data plumbing code can be hidden from view of the user.
// This might introduce performance issues in some cases, unnecessary
// closures. We might extend this view with some specializations like
// `T(x,[F][T]if) => [T(x,F)][T(x,T)]if` to avoid closing conditional
// expressions over the stack. Or we might rely on sufficiently smart
// optimizers to remove closures via static escape analysis.
//
// Thoughts: Performance of this view could be improved if I were to
// index the programs so I know which subprograms contain which words.
// Not sure it's worth the effort, though.
//
module LocalVar =
    open Parser

    /// Variables in this view are a represented as words.
    type Var = Word

    module private Impl =
        let plambda = BS.fromString "lambda-"
        let plocal = BS.fromString "local-"
        let inline matchPrefix p w = 
            (p = (BS.take (BS.length p) w))
        let inline extractPrefix p w =
            if not (matchPrefix p w) then None else
            Some (BS.drop (BS.length p) w)
        let inline matchLambdaTok v (struct(tt,w)) =
            (TT.Anno = tt)
                && (v = (BS.drop (BS.length plambda) w))
                && (matchPrefix plambda w) 
        let inline matchWordTok tgt (struct(tt,w)) =
            (TT.Word = tt) && (tgt = w)

        // using `cc` to limit stack depth
        let rec cwLoop cc w p =
            match p with
            | ((Atom tok) :: p') ->
                if matchWordTok w tok then true else // word found
                if matchLambdaTok w tok then cwLoopCC cc w else // word shadowed in p'
                cwLoop cc w p'
            | ((Block b) :: p') -> cwLoop (p'::cc) w b
            | [] -> cwLoopCC cc w
        and cwLoopCC cc w =
            match cc with
            | (p :: cc') -> cwLoop cc' w p
            | [] -> false

        // Test whether a word is contained within a program, albeit
        // in a manner sensitive to name shadowing behind (lambda-*)
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

        // Test for `(local-x)` patterns, extracting 'x'.
        let (|Local|_|) op = extractAtomSuffix (TT.Anno) plocal op

        // Test for `(lambda-x)` patterns, extracting 'x'.
        let (|Lambda|_|) op = extractAtomSuffix (TT.Anno) plambda op

        let inline isPrimOpC c = ((byte 'd') >= c) && (c >= (byte 'a'))
        let inline isPrimOp w = (1 = BS.length w) && (isPrimOpC (w.[0]))

        // not all lambda and local variables syntactically accepted by
        // Awelon should be accepted by this view. But we shouldn't fail
        // in practice, since that means we have dubious input.
        let acceptVar w = (Parser.isValidWord w) && (not (isPrimOp w))
        let acceptLocal w p = (acceptVar w) && (not (containsWord w p))

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

        // Rewrite (local-*) entries to (lambda-*) from left to right
        let rec pushVars (u:U) (l:L) (r:P) : P =
            match r with
            | ((Local x) :: r') when acceptLocal x r' ->
                // push `x` into `r'`, then continue from there.
                let lambda_x = Atom (tokAnno (BS.append plambda x))
                pushVar x (pushVars u (lambda_x :: l)) [] r'
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

        // Extract (lambda-*) entries from right to left. 
        let rec pullVars (u:U) (l:P) (r:P) : P =
            match r with
            | ((Lambda x) :: r') when acceptVar x -> 
                // remove lambdas from r', extract `x`, prepend `(local-x)`
                let local_x = Atom (tokAnno (BS.append plocal x))
                let u' pf = u (appendRev l (local_x :: pf))
                pullVars (pullVar x u' []) [] r'
            | ((Block b) :: r') -> // deep rewrite within blocks
                let ub b' = pullVars u ((Block b')::l) r'
                pullVars ub [] b
            | (op::r') -> pullVars u (op::l) r' // ignore most tokens
            | [] -> u (List.rev l) // all done!

    /// A LocalVars view is computed by rewriting `(local-x)`
    /// annotations to `\x x` then propagating the `x` as a
    /// value across Awelon's basic `a b c d` operators.
    ///
    /// However, for compatibility with Awelon parsers and other
    /// views, we use `(lambda-x)` tokens place of `\x`. Lambdas
    /// lack identity semantics, so aren't true annotations, but
    /// it serves as a placeholder for our views. A higher view
    /// might then render `(lambda-x)` as `\x`.
    ///
    /// This simulates local variable lets and lambdas:
    ///
    ///     [X] \x EXPR         let x = [X] in EXPR         
    ///     [\x EXPR]           (lambda x . EXPR)
    ///
    /// In problematic cases, e.g. if `x` is already in use, we
    /// may skip the rewrite, leaving a `(local-x)` annotation.
    let viewLocalVars (p:Program) : Program = Impl.pushVars id [] p

    /// Given a program containing `(lambda-*)` tokens - which are
    /// not valid annotations - we can rewrite to eliminate lambdas
    /// and local variables before we store to a dictionary. The
    /// algorithm simply injects appropriate Awelon primitives:
    ///
    ///     (lambda-x) EXPR == (local-x) T(x,EXPR) 
    ///       assuming `x` represents a value
    ///
    ///     T(x,E) | E does not contain x      => d E
    ///     T(x,x)                             => 
    ///     T(x,[E])                           => [T(x,E)] b
    ///     T(x,F G)                            
    ///        | only F contains x             => T(x,F) G
    ///        | only G contains x             => [F] a T(x,G)
    ///        | F and G contain x             => c [T(x,F)] a T(x,G)
    ///
    /// Effectively, our LocalVars view is a lightweight compiler and
    /// decompiler that leaves `(local-varname)` hints to recover the
    /// human-meaningful symbols.
    let hideLocalVars (p:Program) : Program = Impl.pullVars id [] p

    // Further options: erase (local-*) annotations (and resulting `[]a`
    // sequences) to simplify a debug view of the final code. Alternatively,
    // leave `(var-x)` annotations as debug placeholders when extracting 
    // `(lambda-x)`.



