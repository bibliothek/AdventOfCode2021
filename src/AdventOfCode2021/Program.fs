open AdventOfCode2021.Solver
open AdventOfCode2021.Common

let getSolver (day, part): Solver =
    match (day, part) with
        | 1,1 -> Day1.solver1
        | 1,2 -> Day1.solver2
        | 2,1 -> Day2.solver1
        | 2,2 -> Day2.solver2
        | 3,1 -> Day3.solver1
        | 3,2 -> Day3.solver2
        | 4,1 -> Day4.solver1
        | 4,2 -> Day4.solver2
        | 5,1 -> Day5.solver1
        | 5,2 -> Day5.solver2
        | 6,1 -> Day6.solver1
        | 6,2 -> Day6.solver1
        | 7,1 -> Day7.solver1
        | 7,2 -> Day7.solver2
        | 8,1 -> Day8.solver1
        | 8,2 -> Day8.solver2
        | 9,1 -> Day9.solver1
        | 9,2 -> Day9.solver2
        | 10,1 -> Day10.solver1
        | 10,2 -> Day10.solver2
        | 11,1 -> Day11.solver1
        | 11,2 -> Day11.solver2
        | 12,1 -> Day12.solver1
        | 12,2 -> Day12.solver2
        | 13,1 -> Day13.solver1
        | 13,2 -> Day13.solver2
        | 14,1 -> Day14.solver1
        | 14,2 -> Day14.solver2
        | 15,1 -> Day15.solver1
        | 15,2 -> Day15.solver2
        | 16,1 -> Day16.solver1
        | 16,2 -> Day16.solver2
        | 17,1 -> Day17.solver1
        | 17,2 -> Day17.solver2
        | _ -> failwith $"Day {day} and Part {part} not implemented"

let getLines day = 
    System.IO.File.ReadAllLines $"Input/Day{day}.txt"

[<EntryPoint>]
let main args =
    let day = args.[0] |> int
    let part = args.[1] |> int
    printfn $"Solving for day %i{day} part %i{part}\n"
    let solution = getLines day |> getSolver (day, part)
    printfn $"{solution}"
    0
