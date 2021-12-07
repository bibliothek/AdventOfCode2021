module AdventOfCode2021.Solver.Day7

open System

let rec getOptimalFuel (positions: int array) (idx:int) (maxPosition:int) (optimalFuel:int) =
    if idx > maxPosition then
        optimalFuel
    else
        let currentFuel = positions |> Array.map (fun x-> Math.Abs (x - idx)) |> Array.sum
        if currentFuel < optimalFuel then
            getOptimalFuel positions (idx + 1) maxPosition currentFuel
        else
            getOptimalFuel positions (idx + 1) maxPosition optimalFuel


let solver1 (lines: string array) =
    let positions = lines.[0].Split ',' |> Array.map int
    let maxPosition = positions |> Array.max
    let optimalFuel = getOptimalFuel positions 0 maxPosition Microsoft.FSharp.Core.int.MaxValue
    optimalFuel |> string

let solver2 (lines: string array) =
    failwith "error"