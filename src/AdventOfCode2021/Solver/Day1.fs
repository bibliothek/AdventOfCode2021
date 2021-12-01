module AdventOfCode2021.Solver.Day1

let rec countIncreasing numbers acc =
    match numbers with
    | [] -> acc
    | [ _ ] -> acc
    | head :: tail -> countIncreasing tail (if head < tail.[0] then acc + 1 else acc)

let solver1 (lines: string array) =
    let ints = lines |> Array.map int |> Array.toList
    countIncreasing ints 0 |> string

let solver2 (lines: string array) =
    let ints = lines |> Array.map int |> Array.toList

    let rec getWindowedNumbers (numbers: int list) windowed =
        match numbers with
        | n1 :: n2 :: n3 :: tail -> getWindowedNumbers (n2 :: n3 :: tail) ((n1 + n2 + n3) :: windowed)
        | _ -> windowed

    countIncreasing (getWindowedNumbers ints [] |> List.rev) 0 |> string
