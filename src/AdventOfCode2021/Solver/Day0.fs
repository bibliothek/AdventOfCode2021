module AdventOfCode2021.Solver.Day0

open System

let solver1 (lines: string array) =
    let numbers = lines |> Array.map int

    let mutable n1 = 0
    let mutable n2 = 0

    for number in numbers do
        Array.tryFind (fun x -> x + number = 2020) numbers |> Option.map (fun y -> (n1 <- number; n2 <- y)) |> ignore

    n1 * n2 |> string

let solver2 (lines: string array) =
    let numbers = lines |> Array.map int

    let mutable n1 = 0
    let mutable n2 = 0
    let mutable n3 = 0

    for i in numbers do
        for j in numbers do
            for k in numbers do
                if i + j + k = 2020 then n1 <- i; n2 <- j; n3 <- k

    n1 * n2 * n3 |> string


