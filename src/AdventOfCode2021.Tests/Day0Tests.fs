module Tests

open System
open AdventOfCode2021.Solver
open Xunit

[<Fact>]
let ``Day 0`` () =
    let solution = [|"line1"; "line2"; "line3"|] |> Day0.solver
    Assert.Equal ("line1", solution)
