namespace SomeSAT

open Solver
open Printer

module Main =
    [<EntryPoint>]
    let main (argv: string array) =
        if argv.Length > 0 then
            let cnf = CNF argv[0]
            let model = solve cnf
            printModel model

        0
