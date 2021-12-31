module AdventOfCode2021.Solver.Day15

open System
open AdventOfCode2021

let parseInput (lines: string array) =
    Array2D.init lines.[0].Length lines.Length (fun x y -> Char.GetNumericValue lines.[y].[x] |> int)

type Pos = int * int

type State =
    { Distances: Map<Pos, int>
      Unvisited: Set<Pos>
      Map: int [,] }

let getNodeSet map =
    seq {
        for i in 0 .. (Array2D.length1 map - 1) do
            for j in 0 .. (Array2D.length2 map - 1) do
                yield (i, j)
    }
    |> Set.ofSeq

let initState nodes map =

    let distances =
        nodes
        |> Set.fold (fun map el -> map |> Map.add el Int32.MaxValue) Map.empty<Pos, int>
        |> Map.change (0, 0) (fun _ -> Some 0)

    { Distances = distances
      Map = map
      Unvisited = nodes }

let getNeighbours map pos =
    Set.empty
    |> Set.union (
        if fst pos > 0 then
            Set.empty.Add((fst pos) - 1, snd pos)
        else
            Set.empty
    )
    |> Set.union (
        if snd pos > 0 then
            Set.empty.Add(fst pos, (snd pos) - 1)
        else
            Set.empty
    )
    |> Set.union (
        if fst pos < (map |> Array2D.length1) - 1 then
            Set.empty.Add(fst pos + 1, snd pos)
        else
            Set.empty
    )
    |> Set.union (
        if snd pos < (map |> Array2D.length2) - 1 then
            Set.empty.Add(fst pos, (snd pos) + 1)
        else
            Set.empty
    )



let updateCost state pos currentCost =
    { state with
          Distances =
              state.Distances
              |> Map.change
                  pos
                  (fun x ->
                      let updatedCost =
                          currentCost + state.Map.[fst pos, snd pos]

                      match x with
                      | Some i ->
                          if i < updatedCost then
                              Some i
                          else
                              Some updatedCost
                      | None -> None) }


let calcState (state: State) (current: Pos) =
    if state.Unvisited.Count % 1000 = 0 then
        printfn $"{state.Unvisited.Count} / {state.Distances.Count}"

    let unvisitedNeighbours =
        getNeighbours state.Map current
        |> Set.intersect state.Unvisited

    let updatedState =
        unvisitedNeighbours
        |> Set.fold (fun s el -> updateCost s el state.Distances.[current]) state

    { updatedState with
          Unvisited = state.Unvisited |> Set.remove current }


let solver1 (lines: string array) =
    let map = parseInput lines
    let allNodes = getNodeSet map
    let initialState = initState allNodes map

    let endState =
        allNodes |> Set.fold calcState initialState

    endState.Distances.[Array2D.length1 map - 1, Array2D.length2 map - 1]
    |> string

let increaseMap (map: int [,]) =
    Array2D.init
        (Array2D.length1 map * 5)
        (Array2D.length2 map * 5)
        (fun x y ->
            let valueInSmallMap = map.[x % (Array2D.length1 map), y % (Array2D.length2 map)]
            let increasedValue = valueInSmallMap + 1 * (x/ (Array2D.length1 map)) + 1*(y/(Array2D.length2 map))
            if increasedValue > 9 then increasedValue - 9 else increasedValue)

let solver2 (lines: string array) =
    let smallMap = parseInput lines
    let map = increaseMap smallMap

    let allNodes = getNodeSet map
    let initialState = initState allNodes map

    let endState =
        allNodes |> Set.fold calcState initialState

    endState.Distances.[Array2D.length1 map - 1, Array2D.length2 map - 1]
    |> string
