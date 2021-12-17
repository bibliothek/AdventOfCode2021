module AdventOfCode2021.Tests.Day12Tests

open AdventOfCode2021.Solver
open Xunit

type Day12Test() =
    let demoData =
        [| "start-A"
           "start-b"
           "A-c"
           "A-b"
           "b-d"
           "A-end"
           "b-end" |]

    [<Fact>]
    let ``Day 12 part 1`` () =
        let solution = Day12.solver1 demoData
        Assert.Equal("10", solution)


    [<Fact>]
    let ``Day 12 part 2`` () =
        let solution = Day12.solver2 demoData
        Assert.Equal("", solution)
