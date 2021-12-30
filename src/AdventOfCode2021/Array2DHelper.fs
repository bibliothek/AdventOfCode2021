module AdventOfCode2021.Array2DHelper

open System
open System.Text


let printMap map =
    let sb = StringBuilder()

    for y in 0 .. (map |> Array2D.length2) - 1 do
        for x in 0 .. (map |> Array2D.length1) - 1 do
            sb.Append(map.[x, y] |> string) |> ignore

        sb.Append Environment.NewLine |> ignore

    sb.ToString()
