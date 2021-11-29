open System
open AdventOfCode2021.Solver

let getSolver (day, part) =
    match (day, part) with
        0,_ -> Day0.solver
        | _ -> failwith $"Day {day} not implemented"

let getLines day = 
    System.IO.File.ReadAllLines $"Input/Day{day}.txt"

[<EntryPoint>]
let main args =
    let day = args.[0] |> int
    let part = args.[1] |> int
    printfn $"Solving for day %i{day}"
    let solution = getLines day |> getSolver (day, part)
    Console.WriteLine solution
    0
