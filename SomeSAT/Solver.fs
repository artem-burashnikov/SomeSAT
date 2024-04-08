module SomeSAT.Solver

open CNF

type TypedLiteral =
    | PurePositive
    | PureNegative
    | Mixed
    | Unresolved

let neg =
    function
    | Positive l -> Negative l
    | Negative l -> Positive l

let tryFindUnitLiteral cnfData =
    let maybeClause = List.tryFind (fun (clause: Set<_>) -> clause.Count = 1) cnfData

    match maybeClause with
    | Some clause -> Seq.head clause |> Some
    | None -> None

let tryFindPureLiteral cnfData cnfVariables =
    let _markLiterals cnfData cnfVariables =
        let innerFolder =
            fun (state: array<TypedLiteral>) l ->
                match l with
                | Positive n ->
                    match state[n - 1] with
                    | PurePositive
                    | Unresolved -> state[n - 1] <- PurePositive
                    | PureNegative
                    | Mixed -> state[n - 1] <- Mixed
                | Negative n ->
                    match state[n - 1] with
                    | PureNegative
                    | Unresolved -> state[n - 1] <- PureNegative
                    | PurePositive
                    | Mixed -> state[n - 1] <- Mixed

                state

        let typedLiteralsArr = Array.create cnfVariables Unresolved
        List.fold (Set.fold innerFolder) typedLiteralsArr cnfData

    let typedLiteralsArr = _markLiterals cnfData cnfVariables

    let maybeIndex =
        Array.tryFindIndex (fun elem -> elem = PurePositive || elem = PureNegative) typedLiteralsArr

    match maybeIndex with
    | Some id ->
        if typedLiteralsArr[id] = PurePositive then
            Some(Positive(id + 1))
        else
            Some(Negative(id + 1))
    | None -> None

let propagate cnfData model =
    let rec inner cnfData model =
        let maybeUnitLiteral = tryFindUnitLiteral cnfData

        match maybeUnitLiteral with
        | Some unitLiteral ->
            let _go =
                fun state (clause: Set<_>) ->
                    let negUnitLiteral = neg unitLiteral

                    if clause.Contains(unitLiteral) then
                        state
                    else
                        clause.Remove(negUnitLiteral) :: state

            let cnfData' = List.fold _go List.empty cnfData
            inner cnfData' (unitLiteral :: model)
        | None -> cnfData, model

    inner cnfData model

let purify cnfVariables cnfData model =
    let rec inner cnfData model =
        let maybePureLiteral = tryFindPureLiteral cnfData cnfVariables

        match maybePureLiteral with
        | Some pureLiteral ->
            let _go =
                fun state (clause: Set<_>) ->
                    if clause.Contains(pureLiteral) then
                        state
                    else
                        clause :: state

            let cnfData' = List.fold _go List.empty cnfData
            inner cnfData' (pureLiteral :: model)
        | None -> cnfData, model

    inner cnfData model

// Assume cnfData is not empty and does not contain an empty clause.
let decide cnfData = cnfData |> Seq.head |> Seq.head

let forceSetLiteral literal cnfData model =
    let folder =
        fun state (clause: Set<_>) ->
            let negLit = neg literal

            if clause.Contains(literal) then
                state
            else
                clause.Remove(negLit) :: state

    List.fold folder List.empty cnfData, literal :: model

let evaluate model cnfVariables =
    match model with
    | Some res ->
        let folder =
            fun (state: array<_>) elem ->
                match elem with
                | Positive n -> state[n - 1] <- n
                | Negative n -> state[n - 1] <- -n

                state

        let ans = [| 1..cnfVariables |]
        List.fold folder ans res |> Some
    | None -> None

let dpll (cnf: CNF) =
    let rec inner cnfData cnfVariables model =
        let cnfData', model' = propagate cnfData model
        let cnfData'', model'' = purify cnfVariables cnfData' model'

        match cnfData'' with
        | [] -> Some model''
        | xs when List.contains Set.empty xs -> None
        | _ ->
            let lit = decide cnfData''
            let negLit = neg lit
            let cnfData''', model''' = forceSetLiteral lit cnfData'' model''
            let maybeModel = inner cnfData''' cnfVariables model'''

            match maybeModel with
            | Some ans -> Some ans
            | None ->
                let cnfData'''', model'''' = forceSetLiteral negLit cnfData'' model''
                inner cnfData'''' cnfVariables model''''

    let model = inner cnf.Data cnf.Variables List.empty
    evaluate model cnf.Variables
