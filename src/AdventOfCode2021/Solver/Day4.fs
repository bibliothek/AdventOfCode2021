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

let rec findWinningSheet (game: game) idx =
    let numbersSubset = game.calledNumbers |> Array.take idx
    let winningSheet = game.Sheets |> Array.tryFind (fun x -> isSheetBingo x numbersSubset)
    match winningSheet with
    | Some sheet -> idx, sheet
    | None -> findWinningSheet game (idx + 1)

let calculateFinalAnswer game winningSheet idx =
    let unmarkedSum = winningSheet.Rows |> Array.fold Array.append Array.empty |> Array.except (game.calledNumbers |> Array.take idx) |> Array.sum
    unmarkedSum * game.calledNumbers.[idx - 1] |> string

let solver1 (lines: string array) =
    let game = parseInput lines
    let idx, winningSheet = findWinningSheet game 5
    calculateFinalAnswer game winningSheet idx

let rec findLastWinningSheet (game: game) (sheets: bingoSheet array) idx =
    let numbersSubset = game.calledNumbers |> Array.take idx
    let winningSheet = sheets |> Array.tryFind (fun x -> isSheetBingo x numbersSubset)
    match winningSheet with
    | Some sheet ->
        if sheets.Length = 1 then
            idx, sheets.[0]
        else
            findLastWinningSheet game (sheets |> Array.except [sheet]) idx
    | None -> findLastWinningSheet game sheets (idx + 1)

let solver2 (lines: string array) =
    let game = parseInput lines
    let idx, winningSheet = findLastWinningSheet game game.Sheets 5
    calculateFinalAnswer game winningSheet idx