module AdventOfCode2021.Tests.Day10Tests

open AdventOfCode2021.Solver
open Xunit

type Day10Test() =
    let demoData =
        [| "[({(<(())[]>[[{[]{<()<>>"
           "[(()[<>])]({[<{<<[]>>("
           "{([(<{}[<>[]}>{[]{[(<()>"
           "(((({<>}<{<{<>}{[]{[]{}"
           "[[<[([]))<([[{}[[()]]]"
           "[{[{({}]{}}([{[{{{}}([]"
           "{<[[]]>}<{[{[{[]{()[[[]"
           "[<(<(<(<{}))><([]([]()"
           "<{([([[(<>()){}]>(<<{{"
           "<{([{{}}[<[[[<>{}]]]>[]]" |]

    [<Fact>]
    let ``Day 10 part 1`` () =
        let solution = Day10.solver1 demoData
        Assert.Equal("26397", solution)


    [<Fact>]
    let ``Day 10 part 2`` () =
        let solution = Day10.solver2 demoData
        Assert.Equal("288957", solution)
