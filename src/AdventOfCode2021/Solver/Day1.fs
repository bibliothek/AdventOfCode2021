module AdventOfCode2021.Solver.Day1

let solver1 (lines: string array) =
    let ints = lines |> Array.map int |> Array.toList
    let rec countIncreasing numbers acc =
        match numbers with
            | [] -> acc
            | [_] -> acc
            | head :: tail -> countIncreasing tail (if head < tail.[0] then acc + 1 else acc)

    countIncreasing ints 0 |> string