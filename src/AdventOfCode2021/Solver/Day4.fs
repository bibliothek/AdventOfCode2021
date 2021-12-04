module AdventOfCode2021.Solver.Day4

open System
open AdventOfCode2021.Solver

type bingoSheet = {
    Rows: int array array
    Columns: int array array
}

type game = {
    Sheets: bingoSheet array
    calledNumbers: int array
}

let isOneDimensionBingo (dimension: int array array) (calledNumbers: int array) =
    dimension |> Array.exists (fun x-> x |> Array.forall (fun y -> calledNumbers |> Array.contains y))

let isSheetBingo (sheet: bingoSheet) (calledNumbers: int array) : bool =
    isOneDimensionBingo sheet.Rows calledNumbers || isOneDimensionBingo sheet.Columns calledNumbers

let parseInput (lines: string array)  =
    let calledNumbers = lines.[0].Split ',' |> Array.map (fun x-> x |> int)
    let sheetLines = lines |> Array.skip 2 |> Array.chunkBySize 6
    let sheets = sheetLines |> Array.map (fun x ->
        let numberedRows = x |> Array.take 5
        let rows = numberedRows |> Array.map (fun y -> y.Split ' ' |> Array.filter (fun a -> not(String.IsNullOrWhiteSpace a)) |> Array.map (fun z -> z |> int))
        let columns = [| for i in 0 .. 4 -> [| for j in 0 .. 4 -> rows.[j].[i] |]|]
        { Rows = rows; Columns = columns })
    { Sheets = sheets; calledNumbers = calledNumbers }



let solver1 (lines: string array) =
    let game = parseInput lines
    failwith "error"

let solver2 (lines: string array) =
    failwith "error"