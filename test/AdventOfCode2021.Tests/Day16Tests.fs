module AdventOfCode2021.Tests.Day16Tests

open AdventOfCode2021.Solver
open Xunit
open Xunit
open Xunit

type Day16Test() =

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

    [<Theory>]
    [<InlineData("C200B40A82","3")>]
    [<InlineData("04005AC33890","54")>]
    [<InlineData("880086C3E88112","7")>]
    [<InlineData("CE00C43D881120","9")>]
    [<InlineData("D8005AC2A8F0","1")>]
    [<InlineData("F600BC2D8F","0")>]
    [<InlineData("9C005AC2F8F0","0")>]
    [<InlineData("9C0141080250320F1802104A08","1")>]
    let ``Day 16 part 2`` (input:string, solution:string) =
        let result = Day16.solver2 [|input|]
        Assert.Equal(solution, result)
