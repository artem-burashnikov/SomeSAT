module SomeSAT.DIMACSReader

open System.IO

type DIMACSFile(filePath: string) =
    let splitOptions =
        System.StringSplitOptions.RemoveEmptyEntries
        + System.StringSplitOptions.TrimEntries

    let allLines = File.ReadLines filePath

    let noCommentsLines =
        Seq.filter (fun (n: string) -> n[0] <> 'c' && n[0] <> '%') allLines

    let header = (Seq.head noCommentsLines).Split(' ', splitOptions)

    let variables = int header[2]
    let clauses = int header[3]
    let data = Seq.tail noCommentsLines

    member this.Variables = variables
    member this.Clauses = clauses
    member this.Data = data
