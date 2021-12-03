module AdventOfCode2021.Solver.Day3

open System

let solver1 (lines: string array) =
    let mutable mostCommon = []
    let threshold = lines.Length / 2
    for i in 0 .. (lines.[0].Length - 1) do
        let count = lines |> Array.map (fun x -> x.[i] |> sprintf "%c" |> int) |> Array.sum
        if count > threshold then mostCommon <- mostCommon @ ["1"] else mostCommon <- mostCommon @ ["0"]

    let leastCommon = mostCommon |> List.map (fun x -> if x = "0" then "1" else "0")
    let mostCommonInt = Convert.ToInt32 ((String.Join ("", mostCommon)), 2)
    let leastCommonInt = Convert.ToInt32 ((String.Join ("", leastCommon)), 2)
    mostCommonInt * leastCommonInt |> string

let solver2 (lines: string array) =
    failwith "error"