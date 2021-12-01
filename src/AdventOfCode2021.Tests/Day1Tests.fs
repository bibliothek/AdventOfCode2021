module AdventOfCode2021.Tests.Day1Tests

open AdventOfCode2021.Solver
open Xunit

[<Fact>]
let ``Day 1 part 1`` () =
    let demoData = [|"199";"200";"208";"210";"200";"207";"240";"269";"260";"263"|]
    let solution = Day1.solver1 demoData
    Assert.Equal ("7", solution)

[<Fact>]
let ``Day 1 part 2`` () =
    let demoData = [|"199";"200";"208";"210";"200";"207";"240";"269";"260";"263"|]
    let solution = Day1.solver2 demoData
    Assert.Equal ("5", solution)