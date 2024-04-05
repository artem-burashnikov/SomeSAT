module SomeSAT.CNF

open DIMACSReader

type Literal =
    | Positive of int
    | Negative of int

type CNF(file: DIMACSFile) =
    let cnf =
        let mapping (line: string) =
            line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.takeWhile (fun numStr -> numStr <> "0")
            |> Array.map (fun numStr ->
                let l = int numStr
                if l > 0 then Positive l else Negative -l)
            |> Set.ofArray

        Seq.map mapping file.Data |> Set.ofSeq

    member this.Variables = file.Variables
    member this.Clauses = file.Clauses
    member this.Data = cnf
