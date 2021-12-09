﻿open AdventOfCode2021.Solver
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
