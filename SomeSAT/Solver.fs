module SomeSAT.Solver

open CNF

type TypedLiteral =
    | PurePositive
    | PureNegative
    | Mixed
    | Unresolved

let tryFindPureLiteral arr =
    let maybeIndex =
        Array.tryFindIndex (fun elem -> elem = PurePositive || elem = PureNegative) arr

    match maybeIndex with
    | Some i when arr[i] = PurePositive -> Some(Positive(i + 1))
    | Some i when arr[i] = PureNegative -> Some(Negative(i + 1))
    | _ -> None

let neg =
    function
    | Positive l -> Negative l
    | Negative l -> Positive l

let propagate unitLiteral cnfData =
    let negUnitLiteral = neg unitLiteral

    let folder =
        fun (state: Set<Set<_>>) (clause: Set<_>) ->
            if clause.Contains(unitLiteral) then
                state
            elif clause.Contains(negUnitLiteral) then
                state.Add(clause.Remove negUnitLiteral)
            else
                state.Add(clause)

    Set.fold folder Set.empty cnfData

let purify pureLiteral cnfData =
    let folder =
        fun (state: Set<Set<_>>) (clause: Set<_>) ->
            if clause.Contains(pureLiteral) then
                state
            else
                state.Add(clause)

    Set.fold folder Set.empty cnfData

let tryFindMap (cnf: CNF) =
    let innerFolder =
        fun (state: array<TypedLiteral>) (l: Literal) ->
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

    let outerFolder =
        fun (state: Option<Literal> * array<TypedLiteral>) (clause: Set<Literal>) ->
            let unitLiteral =
                match clause.Count with
                | cnt when cnt = 1 -> clause |> Set.toSeq |> Seq.tryHead
                | _ -> fst state

            unitLiteral, Set.fold innerFolder (snd state) clause

    let typedVarsArray: array<TypedLiteral> = Array.create cnf.Variables Unresolved
    Set.fold outerFolder (None, typedVarsArray) cnf.Data
