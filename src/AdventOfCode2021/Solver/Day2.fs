module AdventOfCode2021.Solver.Day2

open System

let negative x =
    x * -1

let solver1 (lines: string array) =
    let forward = lines |> Array.filter (fun x -> x.StartsWith "forward") |> Array.map (fun x -> x.Substring 7 |> int) |> Array.sum
    let depth = lines |> Array.filter (fun x -> not (x.StartsWith "forward")) |> Array.map(fun x -> if x.StartsWith "up" then x.Substring 3 |> int |> negative else x.Substring 5 |> int) |> Array.sum
    forward * depth |> string