module AdventOfCode2021.Solver.Day13

open System
open System.Text

let parseInput (lines: string array) =
    let emptyLineIdx =
        lines |> Array.findIndex (fun x -> x = "")

    let coords =
        lines
        |> Array.take emptyLineIdx
        |> Array.map
            (fun x ->
                let split = x.Split ','
                (split.[0] |> int, split.[1] |> int))

    let instructions =
        lines
        |> Array.skip (emptyLineIdx + 1)
        |> Array.map (fun x -> (x.Split ' ').[2])
        |> Array.toList

    let maxX =
        (coords |> Array.map fst |> Array.max) + 1

    let maxY =
        (coords |> Array.map snd |> Array.max) + 1

    let map = Array2D.create maxX maxY "."

    for (x, y) in coords do
        map.[x, y] <- "#"

    (map, instructions)


let foldHorizontal (map: string [,]) idx =
    let smallerMap = map.[*, 0..(idx - 1)]

    for x in 0 .. ((map |> Array2D.length1) - 1) do
        for y in idx .. ((map |> Array2D.length2) - 1) do
            if map.[x, y] = "#" then
                smallerMap.[x, ((map |> Array2D.length2) - 1 - y)] <- "#"

    smallerMap

let foldVertical (map: string [,]) idx =
    let smallerMap = map.[0..(idx - 1), *]

    for x in idx .. ((map |> Array2D.length1) - 1) do
        for y in 0 .. ((map |> Array2D.length2) - 1) do
            if map.[x, y] = "#" then
                smallerMap.[((map |> Array2D.length1) - 1 - x), y] <- "#"

    smallerMap

let foldMap (map) (instruction: string) =
    if instruction.[0] = 'y' then
        foldHorizontal map (instruction.Substring 2 |> int)
    elif instruction.[0] = 'x' then
        foldVertical map (instruction.Substring 2 |> int)
    else
        failwith "improper input"

let countDots (map: string [,]) =
    map
    |> Seq.cast<string>
    |> Seq.countBy id
    |> Seq.find (fun x -> fst x = "#")
    |> snd

let solver1 (lines: string array) =
    let map, instructions = parseInput lines
    let folded = instructions |> List.take 1 |> List.fold foldMap map
    countDots folded |> string

let printMap (map: string [,]) =
    let sb = StringBuilder()

    for y in 0 .. (map |> Array2D.length2) - 1 do
        for x in 0 .. (map |> Array2D.length1) - 1 do
            sb.Append map.[x, y] |> ignore

        sb.Append Environment.NewLine |> ignore

    sb.ToString()


let solver2 (lines: string array) =
    let map, instructions = parseInput lines
    let folded = instructions |> List.fold foldMap map
    printMap folded
