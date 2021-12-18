module AdventOfCode2021.Tests.Day13Tests

open AdventOfCode2021.Solver
open Xunit

type Day13Test() =
    let demoData =
        [| "6,10"
           "0,14"
           "9,10"
           "0,3"
           "10,4"
           "4,11"
           "6,0"
           "6,12"
           "4,1"
           "0,13"
           "10,12"
           "3,4"
           "3,0"
           "8,4"
           "1,10"
           "2,14"
           "8,10"
           "9,0"
           ""
           "fold along y=7"
           "fold along x=5"

           |]

    [<Fact>]
    let ``Day 13 part 1`` () =
        let solution = Day13.solver1 demoData
        Assert.Equal("17", solution)


    [<Fact>]
    let ``Day 13 part 2`` () =
        let solution = Day13.solver2 demoData
        Assert.Equal("#####
#...#
#...#
#...#
#####
.....
.....
", solution)
