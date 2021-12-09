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

let solver2 (lines: string array) =
    failwith "error"