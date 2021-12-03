module AdventOfCode2021.Solver.Day3

open System

let rec getMostCommon (lines: string array) (idx: int) acc =
    if idx = lines.[0].Length then
        acc
    else
        let count =
            lines
            |> Array.map (fun x -> x.[idx] |> sprintf "%c" |> int)
            |> Array.sum

        if count > lines.Length / 2 then
            getMostCommon lines (idx + 1) acc @ [ "1" ]
        else
            getMostCommon lines (idx + 1) acc @ [ "0" ]



let solver1 (lines: string array) =
    let mostCommon = getMostCommon lines 0 [] |> List.rev

    let leastCommon =
        mostCommon
        |> List.map (fun x -> if x = "0" then "1" else "0")

    let mostCommonInt =
        Convert.ToInt32((String.Join("", mostCommon)), 2)

    let leastCommonInt =
        Convert.ToInt32((String.Join("", leastCommon)), 2)

    mostCommonInt * leastCommonInt |> string

let rec getLineByReduction (lines: string array) idx comparer =
    if lines.Length = 1 then
        lines.[0]
    else
        let threshold = (lines.Length |> float32) / 2.0f

        let count =
            lines
            |> Array.map (fun x -> x.[idx] |> sprintf "%c" |> int)
            |> Array.sum

        let comparerChar: char = comparer (count, threshold)

        let newLines =
            lines
            |> Array.filter (fun x -> x.[idx] = comparerChar)

        getLineByReduction newLines (idx + 1) comparer

let solver2 (lines: string array) =
    let mostCommonReducedLine =
        getLineByReduction
            lines
            0
            (fun (count, threshold) ->
                if count |> float32 >= threshold then
                    '1'
                else
                    '0')

    let leastCommonReducedLine =
        getLineByReduction
            lines
            0
            (fun (count, threshold) ->
                if count |> float32 >= threshold then
                    '0'
                else
                    '1')

    let mostCommonReducedInt =
        Convert.ToInt32((String.Join("", mostCommonReducedLine)), 2)

    let leastCommonReducedInt =
        Convert.ToInt32((String.Join("", leastCommonReducedLine)), 2)

    mostCommonReducedInt * leastCommonReducedInt
    |> string
