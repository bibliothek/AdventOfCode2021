module AdventOfCode2021.Tests.Day2Tests

open AdventOfCode2021.Solver
open Xunit

type Day2Test() =
    let demoData =
        [| "forward 5"
           "down 5"
           "forward 8"
           "up 3"
           "down 8"
           "forward 2" |]

    [<Fact>]
    let ``Day 2 part 1`` () =
        let solution = Day2.solver1 demoData
        Assert.Equal("150", solution)


    [<Fact>]
    let ``Day 2 part 2`` () =
        let solution = Day2.solver2 demoData
        Assert.Equal("900", solution)
