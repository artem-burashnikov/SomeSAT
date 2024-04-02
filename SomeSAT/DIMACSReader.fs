module SomeSAT.DIMACSReader

open System.IO

type Literal<'T> =
    | Positive of 'T
    | Negative of 'T

// DIMACS File reader.
let read filePath =
    let lineIsValidClause =
        fun (ch: string) ->
            (ch.Length > 0)
            && (ch[0] <> 'c')
            && (ch[0] <> 'p')
            && (ch[0] <> '%')
            && (ch[0] <> '0')

    // Skip all comments and header, store only data.
    let data = filePath |> File.ReadLines |> Seq.filter lineIsValidClause

    // Map to internal data structure.
    let mapping (line: string) =
        line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.takeWhile (fun numStr -> numStr <> "0")
        |> Array.map (fun numStr ->
            let l = int numStr
            if l > 0 then Positive l else Negative -l)
        |> Set.ofArray

    Seq.map mapping data |> Set.ofSeq
