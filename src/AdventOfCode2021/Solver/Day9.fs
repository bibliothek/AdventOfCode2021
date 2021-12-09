module AdventOfCode2021.Solver.Day9

let parseInput (lines: string array) =
    lines |> Array.map (fun x -> x |> Seq.toArray |> Array.map (System.Char.GetNumericValue >> int))

let isTopSmaller (map: int[][]) i j =
    (i - 1) >= 0 && map.[i - 1].[j] <= map.[i].[j]

let isBottomSmaller (map: int[][]) i j =
    (i + 1) < map.Length && map.[i+1].[j] <= map.[i].[j]

let isLeftSmaller (map: int[][]) i j =
    (j-1) >= 0 && map.[i].[j-1] <= map.[i].[j]

let isRightSmaller (map: int[][]) i j =
    (j+1) < map.[0].Length && map.[i].[j+1] <= map.[i].[j]

let isLowPoint (map: int[][]) i j =
    not (isTopSmaller map i j || isBottomSmaller map i j || isRightSmaller map i j || isLeftSmaller map i j)


let rec findLowPoints (map: int [][]) i j acc =
    if i = map.Length then acc
    elif j = map.[0].Length then findLowPoints map (i+1) 0 acc
    elif isLowPoint map i j then
        findLowPoints map i (j+1) ((i,j) :: acc)
    else
        findLowPoints map i (j+1) acc


let solver1 (lines: string array) =
    let map = parseInput lines
    let lowPoints = findLowPoints map 0 0 []
    lowPoints |> List.map (fun (i,j) -> map.[i].[j] + 1) |> List.sum |> string

let isTopHigher (map: int[][]) (i, j) =
    (i - 1) >= 0 && map.[i - 1].[j] >= map.[i].[j]

let isBottomHigher (map: int[][]) (i, j) =
    (i + 1) < map.Length && map.[i+1].[j] >= map.[i].[j]

let isLeftHigher (map: int[][]) (i, j) =
    (j-1) >= 0 && map.[i].[j-1] >= map.[i].[j]

let isRightHigher (map: int[][]) (i, j) =
    (j+1) < map.[0].Length && map.[i].[j+1] >= map.[i].[j]

let isPointHeight9 (map: int array array) point =
    map.[fst point].[snd point] = 9

let rec getBasin map point (acc: Set<int*int>) =
    if acc.Contains point then acc
    elif isPointHeight9 map point then acc
    else
        let newAcc = acc |> Set.add point
        let left = if isLeftHigher map point then getBasin map (fst point, snd point - 1) newAcc else Set.empty
        let right = if isRightHigher map point then getBasin map (fst point, snd point + 1) newAcc else Set.empty
        let top = if isTopHigher map point then getBasin map (fst point - 1, snd point) newAcc else Set.empty
        let bottom = if isBottomHigher map point then getBasin map (fst point + 1, snd point) newAcc else Set.empty
        newAcc |> Set.union left |> Set.union right |> Set.union top |> Set.union bottom

let rec getBasins map lowPoints acc =
    match lowPoints with
    | [] -> acc
    | head :: tail -> (getBasin map head Set.empty) :: getBasins map tail acc

let solver2 (lines: string array) =
    let map = parseInput lines
    let lowPoints = findLowPoints map 0 0 []
    let basins = getBasins map lowPoints []
    let otherBasins = basins |> List.sortBy (fun x -> x.Count)
    let reducedBasins = basins |> List.map (fun x -> x.Count) |> List.sortDescending |> List.take 3
    reducedBasins |> List.fold (fun a b -> a * b) 1 |> string