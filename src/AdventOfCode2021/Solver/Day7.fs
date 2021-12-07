module AdventOfCode2021.Solver.Day7

open System

let calculateConstantFuelConsumption (pos: int) (referencePos: int) = Math.Abs(pos - referencePos)

let rec getOptimalFuel
    (positions: int array)
    (idx: int)
    (maxPosition: int)
    (optimalFuel: int)
    calculateFuelConsumption
    =
    if idx > maxPosition then
        optimalFuel
    else
        let currentFuel =
            positions
            |> Array.map (fun x -> calculateFuelConsumption x idx)
            |> Array.sum

        if currentFuel < optimalFuel then
            getOptimalFuel positions (idx + 1) maxPosition currentFuel calculateFuelConsumption
        else
            getOptimalFuel positions (idx + 1) maxPosition optimalFuel calculateFuelConsumption


let solver1 (lines: string array) =
    let positions = lines.[0].Split ',' |> Array.map int
    let maxPosition = positions |> Array.max

    let optimalFuel =
        getOptimalFuel positions 0 maxPosition Microsoft.FSharp.Core.int.MaxValue calculateConstantFuelConsumption

    optimalFuel |> string

let rec getGrowingConsumptionFromDistance (distance: int) acc =
    if distance = 0 then
        acc
    else
        getGrowingConsumptionFromDistance (distance - 1) (acc + distance)

let calculateGrowingFuelConsumption (pos: int) (referencePos: int) =
    let distance = Math.Abs(pos - referencePos)
    getGrowingConsumptionFromDistance distance 0


let solver2 (lines: string array) =
    let positions = lines.[0].Split ',' |> Array.map int
    let maxPosition = positions |> Array.max

    let optimalFuel =
        getOptimalFuel positions 0 maxPosition Microsoft.FSharp.Core.int.MaxValue calculateGrowingFuelConsumption

    optimalFuel |> string
