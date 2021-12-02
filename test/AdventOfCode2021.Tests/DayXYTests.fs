module AdventOfCode2021.Tests.DayXYTests

open AdventOfCode2021.Solver
open Xunit

type DayXYTest() =
    let demoData =
        [| ""
           "" |]

    [<Fact(Skip="Template")>]
    let ``Day XY part 1`` () =
        let solution = DayXY.solver1 demoData
        Assert.Equal("", solution)


    [<Fact(Skip="Template")>]
    let ``Day XY part 2`` () =
        let solution = DayXY.solver2 demoData
        Assert.Equal("", solution)
