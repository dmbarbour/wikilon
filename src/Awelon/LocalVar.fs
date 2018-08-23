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

    /// A variable is a placeholder for a value. In the Awelon program,
    /// it should take place of a word token, but it must not be a word
    /// actually used within the program.
    type Var = Word

    let opApply : Word = BS.singleton (byte 'a')
    let opBind  : Word = BS.singleton (byte 'b')
    let opCopy  : Word = BS.singleton (byte 'c')
    let opDrop  : Word = BS.singleton (byte 'd')

    let (|CharOp|_|) op =
        match op with
        | Atom(TT.Word,w) when (1 = BS.length w) -> Some (char (w.[0]))
        | _ -> None

    let rec private appendRev xs dst =
        match xs with
        | (x::xs') -> appendRev xs' (x::dst)
        | _ -> dst

    /// Inject a local variable into a small program, evaluating
    /// basic data plumbing of `Var Program` involving the given
    /// variable. Relies on recognition of Awelon primitive ops.
    let rec injectVar (var:Var) (expr:Program) : Program =
        pushVar var (Cursor.fromProgram expr) |> Cursor.toProgram
    and pushVar (var:Var) (crs:Cursor) : Cursor =
        match crs.toR with
        | ((Block b) :: (CharOp 'a') :: more) ->
            let toL' = appendRev b (crs.toL)
            let crs' = { crs with toL = toL'; toR = more }
            pushVar var crs'
        | (b :: (CharOp 'b') :: more) when isBlock b ->
            let crsRem = { crs with toR = more }
            let crs' = Cursor.enterBlockExitRight b crsRem
            pushVar var crs'
        | ((CharOp 'c') :: more) ->
            match more with
            | ((Block b) :: (CharOp 'a') :: toR') ->
                let cc b' = // continuation when done with block
                    let toL' = appendRev b' (crs.toL)
                    let crsRem = { crs with toL = toL'; toR = toR' }
                    pushVar var crsRem
                let crsBlock = { bcx = Some cc; toL = []; toR = b }
                pushVar var crsBlock
            | _ ->
                let toL' = (tokWord var) :: (crs.toL)
                let crs' = { crs with toL = toL'; toR = more }
                pushVar var crs'
        | ((CharOp 'd') :: more) -> { crs with toR = more }
        | _ -> Cursor.putRight (tokWord var) crs

    /// Test for containment of a variable or word within a program.
    let rec containsVar (var:Var) (expr:Program) : bool =
        // TODO: rewrite for constant stack space
        match expr with
        | (op:expr') -> (containsVarOp var op) || (containsVar var expr')
        | [] -> false
    and containsVarOp var op =
        match op with
        | Block b -> containsVar var b
        | NS (ns,op') -> containsVarOp var op // ignore namespace!
        | Atom (struct(tt,w)) -> (TT.Word = tt) && (w = var)

    // drop n elements from a list
    let rec private listDrop (n:int) xs =
        if (0 = n) then xs else
        listDrop (n-1) (List.tail xs)

    // add an `[ops] a` segment to left, but optimize `[] a`
    let inline private applyL b toL =
        if List.isEmpty b then toL else
        (tokWord opApply) :: (Block b) :: toL

    /// Extract a variable from a program. The variable is assumed to
    /// be a value word. This extracts the variable such that we may 
    /// easily reinject it using injectVar.
    let rec extractVar (var:Var) (expr:Program) : Program = 
        pullVar var (Cursor.fromProgram expr) |> Cursor.toProgram
    and pullVar var crs = // does not assume entry exists.
        match List.tryFindIndex (containsVarOp var) (crs.toR) with
        | None -> { crs with toR = (tokWord opDrop) :: (crs.toR) }
        | Some ix ->
            let toL' = applyL (List.truncate ix expr) (crs.toL)
            let toR' = listDrop ix expr
            let crs' = { crs with toL = toL'; toR = toR' }
            pullVarHd var crs'
    and private pullVarOpCC var crs varOps =
        // continuation after conversion of op -> varOps. This
        // mostly needs to handle copying of the variable.
        match List.tryFindIndex (containsVarOp var) (crs.toR) with
        | None -> { crs with toR = List.append varOps (crs.toR) }
        | Some ix ->
            let toR' = listDrop ix (crs.toR)
            let bApp = List.append varOps (List.truncate ix (crs.toR))
            let toL' = applyL bApp ((tokWord opCopy)::(crs.toL))
            let crs' = { crs with toL = toL'; toR = toR' }
            pullVarHd var crs'
    and private pullVarOp var op crs =
        match op with
        | Atom(TT.Word,w) when (w = var) -> 
            // variable entry is simply removed
            pullVarOpCC var crs []
        | b when isBlock b ->
            // need continuation
            let cc b' = pullVarOpCC var crs [b'; tokWord opBind]
            let crs' = Cursor.enterBlockCC b cc
            pullVar
    and private pullVarHd var crs =
        match crs.toR with
        | (op::more) ->
            let crsRem = { crs with toR = more }
            let varOps =
                match op with
                | Atom(TT.Word,w) when (w = var) -> List.empty
                | _ when isBlock op ->
                
            let crsRem = { crs with toR = more }
            let cc = pullVarOpsCC var crsRem
            let crsOp = { bcc = Some cc; toL = []; toR = [] }
    and private pullVarOps var crs varOps =
        match List.tryFindIndex (containsVarOp var) (crs.toR) with
        | None -> { crs with toR = (tokWord opDrop) :: (crs.toR) }
        | Some ix ->    
            let toL' = applyL (List.truncate ix (crs.toR)) (crs.toL)
            let toR' = listDrop ix (crs.toR)
            let crs' = { crs with toL = toL'; toR = toR' }
            pullVarHd var crs'
    and private pullVarOps var crs varOps =
        
    and private pullVarHd var crs =
        // assume var is found in first op
        match crs.toR with
        | (op::more) ->
            let cc varOps = // handle the rewritten op
                match List.tryFindIndex (containsVarOp var) more with
                | None -> // done with variable extraction
                    { crs with toR = List.append (varOps) more }
                | Some ix ->
                    let bBind = List.append varOps (List.truncate ix more)
                    let toL' = applyL bBind ((tokWord opCopy) :: crs.toL)
                    let toR' = listDrop ix more
                    
                        if List.isEmpty bBind then crs.toL else
                        (tokWord opBind) :: (Block bBind) :: (crs.toL)
            match List.tryFindIndex
        | _ -> failwith "invalid cursor state"

            
        match List.tryFindIndex (containsVarOp var) expr with
        | None -> (tokWord opDrop) :: expr // done!
        | Some ix -> 
            let crs = // skip to first variable entry
                if (0 = ix) then Cursor.fromProgram expr else
                let bSkip = List.truncate ix expr
                let toL = (tokWord opApply) :: (Block bSkip) :: []
                { bcc = None; toL = toL; toR = listDrop ix expr }
            (pullVarHd var crs) |> Cursor.toProgram
    and private pullVarHd var crs =
        match crs.toR with
        | (op::more) -> pullVarOp var op { crs with toR = more }
        | _ -> failwith "invalid 

        let cc varOps = 
        let varOps = pullVarOps var (List.head (crs.toR))
        let rem = List.tail (crs.toR)
        match List.tryFindIndex (containsVarOp var) rem with
        | None -> List.append varOps rem
        | Some ix -> // keep blocks large 
            let b = List.append varOps (List.truncate ix rem)
            let more = extractVarHd var (listDrop ix rem)
            (tokWord opCopy) :: List.append (applyBlock b) more

    and pullVarOps

        let 
        match crs.toR with
        | (
        | (op::more) ->
            match List.tryFindIndex (containsVarOp var) more with
            | None -> // Final?
                match op with
                | Atom (TT.Word,w) when (w = var) -> 
                | _ when isBlock op ->
                    // DONE!
{ crs with toR = more }
                | 
            | Some ix -> // need to copy our 
        | _ -> failwith "invalid cursor state" // should not occur
                            
            let crs = if (0 = ix) then Cursor.fromProgram expr else
            let bSkip = List.truncate ix expr
            let bRem = listDrop ix expr
            

            if 0 = ix then pullVar var crs else
            let b = List.truncate ix (crs.toR)
            

pullVar var ix (Cursor.fromProgram expr) |> Cursor.toProgram
    and private pullVar var ix crs =
        if 0 = ix then pullVarHd var crs else
        let bSkip = List.truncate ix (crs.toR)
        let toL' = (tokWord opApply) :: (Block bSkip) :: (crs.toL)
        let toR' = listDrop ix (crs.toR)
        let crs' = { crs with toL = toL'; toR = toR' }
        pullVarHd var crs' 


        match List.tryFindIndex (containsVarOp var) expr with
        | Some ix -> 
            let skip = List.truncate ix expr
            let rem = listDrop ix expr
            List.append (applyBlock skip) (extractVarHd var rem)
        | None -> (tokWord opDrop) :: expr
    and private extractVarHd (var:Var) (expr:Program) : Program =
    and private extractVarOp (var:Var) (op:Action) : Program =
        match op with
        | Atom (TT.Word,w) when (w = var) -> []
        | _ when isBlockOp op -> [extractVarB var op; tokWord opBind]
        | _ -> assert(false); [tokWord opDrop; op]





