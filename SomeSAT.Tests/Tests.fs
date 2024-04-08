module Tests

open SomeSAT.CNF
open SomeSAT.Solver
open Xunit

[<Fact>]
let ``Unit propagation`` () =
    let cnfData =
        [| [| Positive 1 |]
           [| Negative 1; Positive 2; Positive 3 |]
           [| Positive 3; Positive 4; Negative 1 |]
           [| Positive 5 |]
           [| Positive 5; Positive 6 |]
           [| Positive 1 |]
           [| Negative 1 |] |]
        |> Array.map Set.ofArray
        |> List.ofArray

    let result, _ = propagate cnfData List.empty

    let expected =
        [| [| Positive 2; Positive 3 |]; [| Positive 3; Positive 4 |]; [||] |]
        |> Array.map Set.ofArray
        |> Set.ofArray

    Assert.Equivalent(expected, result, strict = true)

[<Fact>]
let ``Pure literal elimination`` () =
    let cnfData =
        [| [| Negative 1; Positive 2; Positive 3 |]; [| Positive 3; Positive 4 |] |]
        |> Array.map Set.ofArray
        |> List.ofArray

    let result, _ = purify 4 cnfData List.empty

    let expected = List.empty

    Assert.Equivalent(expected, result, strict = true)

[<Fact>]
let ``Force set literal`` () =
    let cnfData =
        [| [| Negative 1; Positive 2; Positive 3 |]
           [| Positive 3; Positive 4 |]
           [| Positive 1; Negative 2 |] |]
        |> Array.map Set.ofArray
        |> List.ofArray

    let result, _ = forceSetLiteral (Negative 1) cnfData List.empty

    let expected =
        [| [| Positive 3; Positive 4 |]; [| Negative 2 |] |]
        |> Array.map Set.ofArray
        |> List.ofArray

    Assert.Equivalent(expected, result, strict = true)
