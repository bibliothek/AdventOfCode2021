module AdventOfCode2021.Solver.Day8

open System

type SignalEntry =
    { DigitPatterns: Set<char> array
      Outputs: string array }

type SignalEntryWithMapping =
    { Outputs: string array
      Mapping: string array }

let parseInput (line: string) : SignalEntry =
    let lineParts = line.Split '|'
    { DigitPatterns =
          lineParts.[0].Split ' '
          |> Array.filter (String.IsNullOrWhiteSpace >> not)
          |> Array.map (Seq.toArray >> Set.ofArray)
      Outputs =
          lineParts.[1].Split ' '
          |> Array.filter (String.IsNullOrWhiteSpace >> not)
          |> Array.map (Seq.toArray >> Set.ofArray >> Set.toArray >> String) }

let find6 (digitPatterns: Set<char> array) (mapping: Set<char> array) =
    let patternsWith6Segments =
        digitPatterns
        |> Array.filter (fun x -> x.Count = 6)

    ((patternsWith6Segments
      |> Array.filter (fun x -> (Set.intersect x mapping.[1]).Count = 1))).[0]

let find2and3and5 (digitPatterns: Set<char> array) (mapping: Set<char> array) =
    let patternsWith5Segments =
        digitPatterns
        |> Array.filter (fun x -> x.Count = 5)

    let five =
        ((patternsWith5Segments
          |> Array.filter (fun x -> (Set.intersect x mapping.[6]).Count = 5))).[0]

    let two =
        patternsWith5Segments
        |> Array.filter (fun x -> (Set.intersect x five).Count = 3)

    let three =
        patternsWith5Segments
        |> Array.filter (fun x -> (Set.intersect x five).Count = 4)

    two.[0], three.[0], five

let find0and9 (digitPatterns: Set<char> array) (mapping: Set<char> array) =
    let patternsWith6SegmentsWithout6 =
        digitPatterns
        |> Array.filter (fun x -> x.Count = 6)
        |> Array.except [| mapping.[6] |]

    let nine =
        patternsWith6SegmentsWithout6
        |> Array.filter (fun x -> (Set.intersect x mapping.[5]).Count = 5)

    let zero =
        patternsWith6SegmentsWithout6
        |> Array.filter (fun x -> (Set.intersect x mapping.[5]).Count = 4)

    zero.[0], nine.[0]

let createDigitMapping (entry: SignalEntry) =
    let mapping: Set<char> array = Array.create 10 (Set.empty)

    mapping.[1] <-
        entry.DigitPatterns
        |> Array.find (fun x -> x.Count = 2)
        |> Seq.toArray
        |> Set.ofArray

    mapping.[4] <-
        entry.DigitPatterns
        |> Array.find (fun x -> x.Count = 4)
        |> Seq.toArray
        |> Set.ofArray

    mapping.[7] <-
        entry.DigitPatterns
        |> Array.find (fun x -> x.Count = 3)
        |> Seq.toArray
        |> Set.ofArray

    mapping.[8] <-
        entry.DigitPatterns
        |> Array.find (fun x -> x.Count = 7)
        |> Seq.toArray
        |> Set.ofArray

    mapping.[6] <- find6 entry.DigitPatterns mapping

    let two, three, five =
        find2and3and5 entry.DigitPatterns mapping

    mapping.[2] <- two
    mapping.[3] <- three
    mapping.[5] <- five
    let zero, nine = find0and9 entry.DigitPatterns mapping
    mapping.[0] <- zero
    mapping.[9] <- nine

    { Outputs = entry.Outputs
      Mapping = mapping |> Array.map (Set.toArray >> String) }


let solver1 (lines: string array) =
    let entries = lines |> Array.map parseInput

    entries
    |> Array.map
        (fun x ->
            x.Outputs
            |> Array.map (fun y -> y.Length)
            |> Array.filter (fun y -> y = 2 || y = 3 || y = 4 || y = 7)
            |> Array.length)
    |> Array.sum
    |> string

let getOutputValue (entry: SignalEntryWithMapping) =
    let outputThousands =
        (entry.Mapping
         |> Array.findIndex (fun x -> x = entry.Outputs.[0]))
        * 1000

    let outputHundreds =
        (entry.Mapping
         |> Array.findIndex (fun x -> x = entry.Outputs.[1]))
        * 100

    let outputTens =
        (entry.Mapping
         |> Array.findIndex (fun x -> x = entry.Outputs.[2]))
        * 10

    let outputSingles =
        (entry.Mapping
         |> Array.findIndex (fun x -> x = entry.Outputs.[3]))

    outputThousands
    + outputHundreds
    + outputTens
    + outputSingles

let solver2 (lines: string array) =
    let entries = lines |> Array.map parseInput
    let mappings = entries |> Array.map createDigitMapping
    let values = mappings |> Array.map getOutputValue
    values |> Array.sum |> string
