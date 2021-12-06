module AdventOfCode2021.Tests.Day6Tests

open AdventOfCode2021.Solver
open Xunit

type Day6Test() =
    let demoData =
        [| "3,4,3,1,2" |]

    [<Fact>]
    let ``Day 6`` () =
        let solution = Day6.solver1 demoData
        Assert.Equal("5934", solution)
