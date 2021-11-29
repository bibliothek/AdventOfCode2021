module Tests

open System
open AdventOfCode2021.Solver
open Xunit

let demoData = [|"1721";"979";"366";"299";"675";"1456"|]

[<Fact>]
let ``Day 0 part 1`` () =
    let solution = demoData |> Day0.solver1
    Assert.Equal ("514579", solution)

[<Fact>]
let ``Day 0 part 2`` () =
    let solution = demoData |> Day0.solver2
    Assert.Equal ("241861950", solution)
