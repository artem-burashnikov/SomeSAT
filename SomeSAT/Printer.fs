module SomeSAT.Printer

let printModel (model: array<int> option) =
    let maxLineLength = 25

    match model with
    | None -> printfn "s UNSATISFIABLE"
    | Some ans ->
        printfn "s SATISFIABLE"
        printf "v"
        printf " "

        for i in 0 .. ans.Length - 1 do
            printf $"%s{ans[i].ToString()}"

            if (i <> (ans.Length - 1)) && ((i + 1) % maxLineLength = 0) then
                printf "\n"
                printf "v"

            printf " "

        printfn "0"
