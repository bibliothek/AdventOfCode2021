module AdventOfCode2021.Tests.Day3Tests

open AdventOfCode2021.Solver
open Xunit

type Day3Test() =
    let demoData =
        [|
            "00100"
            "11110"
            "10110"
            "10111"
            "10101"
            "01111"
            "00111"
            "11100"
            "10000"
            "11001"
            "00010"
            "01010"
            |]

    [<Fact>]
    let ``Day 3 part 1`` () =
        let solution = Day3.solver1 demoData
        Assert.Equal("198", solution)


    [<Fact>]
    let ``Day 3 part 2`` () =
        let solution = Day3.solver2 demoData
        Assert.Equal("", solution)
