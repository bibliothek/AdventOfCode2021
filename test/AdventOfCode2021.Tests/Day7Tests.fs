module AdventOfCode2021.Tests.Day7Tests

open AdventOfCode2021.Solver
open Xunit

type Day7Test() =
    let demoData =
        [| "16,1,2,0,4,2,7,1,2,14" |]

    [<Fact>]
    let ``Day 7 part 1`` () =
        let solution = Day7.solver1 demoData
        Assert.Equal("37", solution)


    [<Fact>]
    let ``Day 7 part 2`` () =
        let solution = Day7.solver2 demoData
        Assert.Equal("168", solution)
