module AdventOfCode2021.Solver.Day10

open System.ComponentModel.DataAnnotations

let getMatchingClosingBracket (c: char) =
    match c with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> failwith "unexpected bracket"

let isOpeningBracket (c: char) =
    match c with
    | '('
    | '['
    | '{'
    | '<' -> true
    | _ -> false

type lineState =
    | Valid
    | Incomplete of char list
    | Corrupted of char

let rec getLineState (line: char list) (stack: char list) =
    match line with
    | [] ->
        if stack.Length = 0 then
            Valid
        else
            Incomplete stack
    | head :: tail ->
        if head |> isOpeningBracket then
            getLineState tail (head :: stack)
        else if head = (getMatchingClosingBracket stack.Head) then
            getLineState tail stack.Tail
        else
            Corrupted head

let getCorruptionScore c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith "unexpected bracket"

let solver1 (lines: string array) =
    let lineStates =
        lines
        |> Array.map (fun x -> getLineState (x |> Seq.toList) [])

    let score =
        lineStates
        |> Array.choose
            (function
            | Corrupted c -> Some c
            | _ -> None)
        |> Array.map getCorruptionScore
        |> Array.sum

    score |> string

let getCompletionPoints c =
    match c with
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L
    | _ -> failwith "unexpected character"

let solver2 (lines: string array) =
    let lineStates =
        lines
        |> Array.map (fun x -> getLineState (x |> Seq.toList) [])

    let lineCompletionScores =
        lineStates
        |> Array.choose
            (function
            | Incomplete chars ->
                Some(
                    chars
                    |> List.map (getMatchingClosingBracket >> getCompletionPoints)
                    |> List.fold (fun score el -> score * 5L + el) 0L
                )
            | _ -> None)
        |> Array.sort

    lineCompletionScores.[(lineCompletionScores.Length / 2)]
    |> string
