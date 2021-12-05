module AdventOfCode2021.Tests.Day5Tests

open AdventOfCode2021.Solver
open Xunit

type Day5Test() =
    let demoData =
        [| "0,9 -> 5,9"
           "8,0 -> 0,8"
           "9,4 -> 3,4"
           "2,2 -> 2,1"
           "7,0 -> 7,4"
           "6,4 -> 2,0"
           "0,9 -> 2,9"
           "3,4 -> 1,4"
           "0,0 -> 8,8"
           "5,5 -> 8,2" |]

    [<Fact>]
    let ``Day 5 part 1`` () =
        let solution = Day5.solver1 demoData
        Assert.Equal("5", solution)


    [<Fact>]
    let ``Day 5 part 2`` () =
        let solution = Day5.solver2 demoData
        Assert.Equal("", solution)
