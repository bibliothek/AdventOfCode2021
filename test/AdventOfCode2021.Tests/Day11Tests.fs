module AdventOfCode2021.Tests.Day11Tests

open AdventOfCode2021.Solver
open Xunit

type Day11Test() =
    let demoData =
        [| "5483143223"
           "2745854711"
           "5264556173"
           "6141336146"
           "6357385478"
           "4167524645"
           "2176841721"
           "6882881134"
           "4846848554"
           "5283751526" |]

    [<Fact>]
    let ``Day 11 part 1`` () =
        let solution = Day11.solver1 demoData
        Assert.Equal("1656", solution)


    [<Fact>]
    let ``Day 11 part 2`` () =
        let solution = Day11.solver2 demoData
        Assert.Equal("195", solution)
