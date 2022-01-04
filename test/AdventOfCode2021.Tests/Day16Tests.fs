module AdventOfCode2021.Tests.Day16Tests

open AdventOfCode2021.Solver
open Xunit
open Xunit
open Xunit

type Day16Test() =
    let demoData =
        [| ""
           "" |]

    [<Theory>]
    [<InlineData("8A004A801A8002F478","16")>]
    [<InlineData("620080001611562C8802118E34","12")>]
    [<InlineData("C0015000016115A2E0802F182340","23")>]
    [<InlineData("A0016C880162017C3686B18A3D4780","31")>]
    let ``Day 16 part 1`` (input: string, solution: string) =
        let result = Day16.solver1 [|input|]
        Assert.Equal(solution, result)

    [<Fact>]
    let ``Day 16 part 1 example 1`` () =
        let solution = Day16.solver1 [|"38006F45291200"|]
        solution |> ignore

    [<Fact>]
    let ``Day 16 part 1 example 2`` () =
        let solution = Day16.solver1 [|"EE00D40C823060"|]
        solution |> ignore




    [<Fact>]
    let ``Day 16 part 2`` () =
        let solution = Day16.solver2 demoData
        Assert.Equal("", solution)
