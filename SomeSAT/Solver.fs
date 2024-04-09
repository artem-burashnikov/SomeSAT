module SomeSAT.Solver

open System.IO

type Literal =
    | Positive of int
    | Negative of int

    static member (~-) literal =
        match literal with
        | Positive l -> Negative l
        | Negative l -> Positive l

type CNF(filePath: string) =
    let splitOptions =
        System.StringSplitOptions.RemoveEmptyEntries
        + System.StringSplitOptions.TrimEntries

    let headerDataLines =
        File.ReadLines filePath
        |> Seq.filter (fun (n: string) -> n[0] <> 'c' && n[0] <> '%')

    let header = (Seq.head headerDataLines).Split(' ', splitOptions)

    let cnf =
        let fMap (line: string) =
            line.Split(' ', splitOptions)
            |> Array.takeWhile (fun numStr -> numStr <> "0")
            |> Array.map (fun numStr ->
                let l = int numStr
                if l > 0 then Positive l else Negative -l)
            |> List.ofArray

        Seq.map fMap (Seq.tail headerDataLines) |> List.ofSeq

    member this.Variables = int header[2]
    member this.Clauses = int header[3]
    member this.Data = cnf

let tryFindUnitLiteral cnf =
    Seq.tryPick
        (function
        | [ x ] -> Some x
        | _ -> None)
        cnf

let isPure cnf (literal: Literal) =
    if Seq.forall (Seq.forall ((<>) -literal)) cnf then
        Some literal
    else
        None

let tryFindPureLiteral cnf =
    Seq.concat cnf |> Seq.distinct |> Seq.tryPick (isPure cnf)

let chooseUnit literal clause =
    if Seq.exists ((=) literal) clause then
        None
    else
        Some(List.filter ((<>) -literal) clause)

let choosePure literal clause =
    if Seq.exists ((=) literal) clause then
        None
    else
        Some(clause)

let resolve cnf fChoose literal = List.choose (fChoose literal) cnf

let propagate (cnf, model) =
    let rec inner cnf model =
        match tryFindUnitLiteral cnf with
        | Some literal -> inner (resolve cnf chooseUnit literal) (literal :: model)
        | None -> cnf, model

    inner cnf model

let purify (cnf, model) =
    let rec inner cnf model =
        match tryFindPureLiteral cnf with
        | Some literal -> inner (resolve cnf choosePure literal) (literal :: model)
        | None -> cnf, model

    inner cnf model

let evaluate model cnfVariables =
    match model with
    | Some res ->
        Some(
            List.fold
                (fun (state: array<_>) elem ->
                    match elem with
                    | Positive n -> state[n - 1] <- n
                    | Negative n -> state[n - 1] <- -n

                    state)
                [| 1..cnfVariables |]
                res
        )
    | None -> None

let solve (cnf: CNF) =
    let rec inner cnf model =
        let cnf', model' = (cnf, model) |> propagate |> purify

        match cnf' with
        | [] -> Some model'
        | [] :: _ -> None
        | (nextLiteral :: _) :: _ as cnf ->
            if List.exists List.isEmpty cnf then
                None
            else
                let maybeModel = inner (resolve cnf chooseUnit nextLiteral) (nextLiteral :: model')

                match maybeModel with
                | Some ans -> Some ans
                | None -> inner (resolve cnf chooseUnit -nextLiteral) (-nextLiteral :: model')

    let model = inner cnf.Data List.empty
    evaluate model cnf.Variables
