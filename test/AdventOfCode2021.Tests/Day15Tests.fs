module AdventOfCode2021.Tests.Day15Tests

open AdventOfCode2021.Solver
open Xunit

type Day15Test() =
    let demoData =
        [|
           "1163751742"
           "1381373672"
           "2136511328"
           "3694931569"
           "7463417111"
           "1319128137"
           "1359912421"
           "3125421639"
           "1293138521"
           "2311944581" |]

    [<Fact>]
    let ``Day 15 part 1`` () =
        let solution = Day15.solver1 demoData
        Assert.Equal("40", solution)


    [<Fact>]
    let ``Day 15 part 2`` () =
        let solution = Day15.solver2 demoData
        Assert.Equal("315", solution)
