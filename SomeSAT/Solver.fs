module SomeSAT.Solver

open CNF

let tryFindUnitLiteral cnfData =
    Seq.tryPick
        (function
        | [ x ] -> Some x
        | _ -> None)
        cnfData

let isPure cnfData (literal: Literal) =
    if Seq.forall (Seq.forall ((<>) -literal)) cnfData then
        Some literal
    else
        None

let tryFindPureLiteral cnfData =
    Seq.concat cnfData |> Seq.distinct |> Seq.tryPick (isPure cnfData)

let resolveUnit cnfData literal =
    List.choose
        (fun clause ->
            if Seq.exists ((=) literal) clause then
                None
            else
                Some(List.filter ((<>) -literal) clause))
        cnfData

let propagate cnfData model =
    let rec inner cnfData model =
        match tryFindUnitLiteral cnfData with
        | Some literal ->
            let cnfData' = resolveUnit cnfData literal
            inner cnfData' (literal :: model)
        | None -> cnfData, model

    inner cnfData model

let resolvePure cnfData literal =
    List.choose
        (fun clause ->
            if Seq.exists ((=) literal) clause then
                None
            else
                Some clause)
        cnfData

let purify cnfData model =
    let rec inner cnfData model =
        match tryFindPureLiteral cnfData with
        | Some pureLiteral ->
            let cnfData' = resolvePure cnfData pureLiteral
            inner cnfData' (pureLiteral :: model)
        | None -> cnfData, model

    inner cnfData model

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
    let rec inner cnfData model =
        let cnfData', model' = propagate cnfData model
        let cnfData'', model'' = purify cnfData' model'

        match cnfData'' with
        | [] -> Some model''
        | [] :: tl -> None
        | (nextLiteral :: tl1) :: tl2 as f ->
            if List.exists List.isEmpty f then
                None
            else
                let cnfData''' = resolveUnit cnfData'' nextLiteral
                let maybeModel = inner cnfData''' (nextLiteral :: model'')

                match maybeModel with
                | Some ans -> Some ans
                | None ->
                    let cnfData'''' = resolveUnit cnfData'' -nextLiteral
                    inner cnfData'''' (-nextLiteral :: model'')

    let model = inner cnf.Data List.empty
    evaluate model cnf.Variables
