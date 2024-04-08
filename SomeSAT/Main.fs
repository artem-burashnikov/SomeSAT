namespace SomeSAT

open System.Diagnostics
open DIMACSReader
open CNF
open Solver
open Printer

module Main =
    [<EntryPoint>]
    let main (argv: string array) =
        if argv.Length > 0 then
            let cnf = argv[0] |> DIMACSFile |> CNF
            let watch = Stopwatch()
            watch.Start()
            let model = dpll cnf
            watch.Stop()
            printModel model
            printfn $"Total elapsed time: %.3f{watch.Elapsed.TotalMilliseconds} ms"

        0
