module AdventOfCode2021.Tests.Day17Tests

open AdventOfCode2021.Solver
open Xunit

type Day17Test() =
    let demoData =
        [| "target area: x=20..30, y=-10..-5" |]

    [<Fact>]
    let ``Day 17 part 1`` () =
        let solution = Day17.solver1 demoData
        Assert.Equal("45", solution)


    [<Fact>]
    let ``Day 17 part 2`` () =
        let solution = Day17.solver2 demoData
        Assert.Equal("112", solution)
