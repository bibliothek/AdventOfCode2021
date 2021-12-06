module AdventOfCode2021.Solver.Day6

let rec tick (population: int64 list) idx n : int64 list =
    if idx = n then population
    else
        let procreatingFishCount = population.[0]
        let updatedPopulation = (population |> List.skip 1) @ [procreatingFishCount]
        let updatedFish6Count = updatedPopulation.[6] + procreatingFishCount
        let completeUpdatedPopulation = (updatedPopulation |> List.take 6) @ [updatedFish6Count] @ (updatedPopulation |> List.skip 7)
        tick completeUpdatedPopulation (idx + 1) n



let solver1 (lines: string array) =
    let fish = lines.[0].Split ',' |> Array.map int
    let population = [ for i in 0 .. 8 -> fish |> Array.filter (fun x-> x = i) |> Array.length |> int64 ]
    let updatedPopulation = tick population 0 80

    updatedPopulation |> List.sum |> string

let solver2 (lines: string array) =
    let fish = lines.[0].Split ',' |> Array.map int
    let population = [ for i in 0 .. 8 -> fish |> Array.filter (fun x-> x = i) |> Array.length |> int64 ]
    let updatedPopulation = tick population 0 256

    updatedPopulation |> List.sum |> string