module AdventOfCode2021.Tests.Day14Tests

open AdventOfCode2021.Solver
open Xunit

type Day14Test() =
    let demoData =
        [| "NNCB"
           ""
           "CH -> B"
           "HH -> N"
           "CB -> H"
           "NH -> C"
           "HB -> C"
           "HC -> B"
           "HN -> C"
           "NN -> C"
           "BH -> H"
           "NC -> B"
           "NB -> B"
           "BN -> B"
           "BB -> N"
           "BC -> B"
           "CC -> N"
           "CN -> C"
           |]

    [<Fact>]
    let ``Day 14 part 1`` () =
        let solution = Day14.solver1 demoData
        Assert.Equal("1588", solution)


    [<Fact>]
    let ``Day 14 part 2`` () =
        let solution = Day14.solver2 demoData
        Assert.Equal("", solution)
