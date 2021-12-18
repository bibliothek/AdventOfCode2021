module AdventOfCode2021.Solver.Day14

open System.Collections.Generic

let parseInput (lines: string array) =
    let startState = Dictionary<string, int>()

    lines.[0]
    |> Seq.toArray
    |> Array.pairwise
    |> Array.iter (fun (a, b) ->
        let key = $"{a}{b}"
        if startState.ContainsKey key then
            startState.[key] <- startState.[key] + 1
        else
            startState.Add(key,1)
        )

    let substitutionMap =
        lines
        |> Array.skip 2
        |> Array.fold
            (fun map el ->
                map
                |> Map.add
                    (el.Substring(0, 2))
                    (el.Substring(0, 1) + (el.Substring 6), ((el.Substring 6) + el.Substring(1, 2)).Trim()))
            Map.empty<string, string * string>

    startState, substitutionMap

let updateState (state: Dictionary<string, int>) (substitutionMap: Map<string, string * string>) =
    let newState = Dictionary<string, int>()

    substitutionMap
    |> Map.keys
    |> Seq.iter (fun x -> newState.Add(x, 0))

    for kvp in state do
        newState.[fst substitutionMap.[kvp.Key]] <-
            newState.[fst substitutionMap.[kvp.Key]]
            + kvp.Value

        newState.[snd substitutionMap.[kvp.Key]] <-
            newState.[snd substitutionMap.[kvp.Key]]
            + kvp.Value

    newState

let rec tick (startState: Dictionary<string, int>) (substitutionMap: Map<string, string * string>) (i: int) (n: int) =
    if i = n then
        startState
    else
        tick (updateState startState substitutionMap) substitutionMap (i + 1) n

let getCharCount (state: Dictionary<string, int>) (initialLine: string) =
    let lastInitialChar = initialLine.[initialLine.Length - 1]

    state
    |> Seq.groupBy (fun kvp -> kvp.Key.[0])
    |> Seq.map
        (fun x ->
            (fst x,
             (snd x |> Seq.map (fun y -> y.Value) |> Seq.sum)
             + if fst x = lastInitialChar then 1 else 0))
    |> Seq.toArray


let solver1 (lines: string array) =
    let startState, substitutionMap = parseInput lines
    let endState = tick startState substitutionMap 0 10
    let endStateCounts = getCharCount endState lines.[0]

    let minOccur = endStateCounts |> Array.minBy snd |> snd

    let maxOccur  =endStateCounts |> Array.maxBy snd |> snd

    maxOccur - minOccur |> string

let solver2 (lines: string array) = failwith "error"
