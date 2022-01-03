module AdventOfCode2021.Array2DHelper

open System
open System.Text


let getPrintableOverview map =
    let sb = StringBuilder()

    for y in 0 .. (map |> Array2D.length2) - 1 do
        for x in 0 .. (map |> Array2D.length1) - 1 do
            sb.Append(map.[x, y] |> string) |> ignore

        sb.Append Environment.NewLine |> ignore

    sb.ToString()

let getHorizontalAndVerticalNeighbours map pos =
    let x, y = pos

    [| (x + 1, y)
       (x - 1, y)
       (x, y + 1)
       (x, y - 1) |]
    |> Array.filter
        (fun (x, y) ->
            x >= 0
            && x < Array2D.length1 map
            && y >= 0
            && y < Array2D.length2 map)
