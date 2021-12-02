module AdventOfCode2021.Tests.Day2Tests

open AdventOfCode2021.Solver
open Xunit



[<Fact>]
let ``Day 2 part 1`` () =
    let demoData = [|"forward 5";"down 5";"forward 8";"up 3";"down 8";"forward 2"|]
    let solution = Day2.solver1 demoData
    Assert.Equal ("150", solution)

