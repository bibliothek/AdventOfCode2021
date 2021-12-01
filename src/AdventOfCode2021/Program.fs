open AdventOfCode2021.Solver
open AdventOfCode2021.Common

let getSolver (day, part): Solver =
    match (day, part) with
        | 1,1 -> Day1.solver1
        | 1,2 -> Day1.solver2
        | _ -> failwith $"Day {day} and Part {part} not implemented"

let getLines day = 
    System.IO.File.ReadAllLines $"Input/Day{day}.txt"

[<EntryPoint>]
let main args =
    let day = args.[0] |> int
    let part = args.[1] |> int
    printfn $"Solving for day %i{day} part %i{part}"
    let solution = getLines day |> getSolver (day, part)
    printfn $"{solution}"
    0
