module AdventOfCode2021.Solver.Day6

let updateFish (fish: int) =
    match fish with
    | 0 -> 6
    | i -> i - 1

let rec tick (fish: int[]) idx n =
    if idx = n then fish
    else
        printfn $"%i{idx}"
        let procreatingFishCount = fish |> Array.filter (fun x -> x = 0) |> Array.length
        let procreatingFish = [| for _ in 0 .. procreatingFishCount - 1 -> 8 |]
        let updatedFish = fish |> Array.map updateFish |> Array.append procreatingFish
        tick updatedFish (idx + 1) n



let solver1 (lines: string array) =
    let fish = lines.[0].Split ',' |> Array.map int
    let updatedFish = tick fish 0 80
    updatedFish.Length |> string

let solver2 (lines: string array) =
    let fish = lines.[0].Split ',' |> Array.map int
    let population = [| for i in 0 .. 8 -> fish |> Array.filter (fun x-> x = i) |> Array.length |> int64 |]

    let updatedFish = tick fish 0 256
    updatedFish.Length |> string