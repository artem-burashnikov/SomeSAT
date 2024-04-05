module SomeSAT.DIMACSReader

open System.IO

type DIMACSFile(filePath: string) =
    let allLines = File.ReadLines filePath
    let noCommentsLines = Seq.skipWhile (fun (n: string) -> n[0] = 'c') allLines

    let header =
        (Seq.head noCommentsLines)
            .Split(' ', System.StringSplitOptions.RemoveEmptyEntries)

    let variables = int header[2]
    let clauses = int header[3]
    let data = Seq.tail noCommentsLines

    member this.Variables = variables
    member this.Clauses = clauses
    member this.Data = data
