module AdventOfCode2021.Tests.Day9Tests

open AdventOfCode2021.Solver
open Xunit

type Day9Test() =
    let demoData =
        [| "2199943210"
           "3987894921"
           "9856789892"
           "8767896789"
           "9899965678" |]

    [<Fact>]
    let ``Day 9 part 1`` () =
        let solution = Day9.solver1 demoData
        Assert.Equal("15", solution)


    [<Fact>]
    let ``Day 9 part 2`` () =
        let solution = Day9.solver2 demoData
        Assert.Equal("", solution)
