module AdventOfCode2021.Solver.Day8

open System

type SignalEntry = {
    DigitPatterns: string array
    Outputs: string array
}

type SignalEntryWithMapping = {
    Outputs: string array
    Mapping: string option array
}

let sortString (s: string) =
    let charArray = s |> Seq.toArray
    let sorted = charArray |> Array.sort
    sorted |> System.String


let parseInput (line:string) : SignalEntry =
    let lineParts = line.Split '|'
    {
     DigitPatterns = lineParts.[0].Split ' ' |> Array.filter (String.IsNullOrWhiteSpace >> not) |> Array.map sortString
     Outputs = lineParts.[1].Split ' ' |> Array.filter (String.IsNullOrWhiteSpace >> not) |> Array.map sortString
     }

let createDigitMapping (entry: SignalEntry) =
    let mapping: string option array = Array.create 10 None
    mapping.[1] <- entry.DigitPatterns |> Array.tryFind (fun x -> x.Length = 2)
    mapping.[4] <- entry.DigitPatterns |> Array.tryFind (fun x -> x.Length = 4)
    mapping.[7] <- entry.DigitPatterns |> Array.tryFind (fun x -> x.Length = 3)
    mapping.[8] <- entry.DigitPatterns |> Array.tryFind (fun x -> x.Length = 7)
    {Outputs = entry.Outputs; Mapping = mapping}

let getKnownOutputsCount (entryWithMapping: SignalEntryWithMapping) =
    entryWithMapping.Outputs |> Array.filter (fun x -> entryWithMapping.Mapping |> Array.exists (fun y -> y = Some x)) |> Array.length


let solver1 (lines: string array) =
    let entries = lines |> Array.map parseInput
    let mappings = entries |> Array.map createDigitMapping
    mappings |> Array.map getKnownOutputsCount |> Array.sum |> string

let solver2 (lines: string array) =
    failwith "error"