module SomeSAT.CNF

open DIMACSReader

type Literal =
    | Positive of int
    | Negative of int

    static member (~-) lit =
        match lit with
        | Positive l -> Negative l
        | Negative l -> Positive l

type CNF(file: DIMACSFile) =
    let splitOptions =
        System.StringSplitOptions.RemoveEmptyEntries
        + System.StringSplitOptions.TrimEntries

    let cnf =
        let mapping (line: string) =
            line.Split(' ', splitOptions)
            |> Array.takeWhile (fun numStr -> numStr <> "0")
            |> Array.map (fun numStr ->
                let l = int numStr
                if l > 0 then Positive l else Negative -l)
            |> List.ofArray

        Seq.map mapping file.Data |> List.ofSeq

    member this.Variables = file.Variables
    member this.Clauses = file.Clauses
    member this.Data = cnf
