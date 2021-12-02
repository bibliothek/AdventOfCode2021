module AdventOfCode2021.Solver.Day2

let solver1 (lines: string array) =
    let negative x = x * -1

    let forward =
        lines
        |> Array.filter (fun x -> x.StartsWith "forward")
        |> Array.map (fun x -> x.Substring 7 |> int)
        |> Array.sum

    let depth =
        lines
        |> Array.filter (fun x -> not (x.StartsWith "forward"))
        |> Array.map
            (fun x ->
                if x.StartsWith "up" then
                    x.Substring 3 |> int |> negative
                else
                    x.Substring 5 |> int)
        |> Array.sum

    forward * depth |> string

let calculateNewPosition (instruction: string) (aim, depth, forward) =
    if instruction.StartsWith "up" then
        ((aim - (instruction.Substring 3 |> int)), depth, forward)
    elif instruction.StartsWith "down" then
        ((aim + (instruction.Substring 5 |> int)), depth, forward)
    else
        (aim, depth + aim * (instruction.Substring 7 |> int), forward + (instruction.Substring 7 |> int))

let solver2 (lines: string array) =
    let rec calculatePosition (instructions: string list) (aim, depth, forward) =
        match instructions with
        | [] -> (aim, depth, forward)
        | head :: tail -> calculatePosition tail (calculateNewPosition head (aim, depth, forward))

    let _, depth, forward =
        calculatePosition (lines |> Array.toList) (0, 0, 0)

    depth * forward |> string
