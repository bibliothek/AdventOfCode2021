module AdventOfCode2021.Solver.Day15

open System
open AdventOfCode2021

let parseInput (lines: string array) =
    Array2D.init lines.[0].Length lines.Length (fun x y -> Char.GetNumericValue lines.[y].[x] |> int)

type Pos = int * int

type State =
    { Distances: Map<Pos, int>
      Unvisited: Set<int * Pos>
      Map: int [,]
      MaxX: int
      MaxY: int }

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
      Unvisited = Set.empty<int * Pos> |> Set.add (0, (0, 0))
      MaxX = Array2D.length1 map - 1
      MaxY = Array2D.length2 map - 1 }

let updateNode state pos currentCost =
    let updatedCost =
        currentCost + state.Map.[fst pos, snd pos]

    if updatedCost >= state.Distances.[pos] then
        state
    else
        { state with
              Distances =
                  state.Distances
                  |> Map.change
                      pos
                      (fun x ->
                          match x with
                          | Some i -> Some updatedCost
                          | None -> None)
              Unvisited = state.Unvisited |> Set.add (updatedCost, pos) }

let rec updateState (state: State) (currentPos: Pos) =
    if currentPos = (state.MaxX, state.MaxY) then
        state
    else
        let currentDistance = state.Distances.[currentPos]

        let updatedState =
            Array2DHelper.getHorizontalAndVerticalNeighbours state.Map currentPos
            |> Array.fold (fun s el -> updateNode s el currentDistance) state

        let _, nextPos = updatedState.Unvisited.MinimumElement

        updateState
            { updatedState with
                  Unvisited = updatedState.Unvisited.Remove(currentDistance, currentPos) }
            nextPos

let solver1 (lines: string array) =
    let map = parseInput lines
    let allNodes = getNodeSet map
    let initialState = initState allNodes map

    let endState = updateState initialState (0, 0)

    endState.Distances.[endState.MaxX, endState.MaxY]
    |> string

let increaseMap (map: int [,]) =
    Array2D.init
        (Array2D.length1 map * 5)
        (Array2D.length2 map * 5)
        (fun x y ->
            let valueInSmallMap =
                map.[x % (Array2D.length1 map), y % (Array2D.length2 map)]

            let increasedValue =
                valueInSmallMap
                + 1 * (x / (Array2D.length1 map))
                + 1 * (y / (Array2D.length2 map))

            if increasedValue > 9 then
                increasedValue - 9
            else
                increasedValue)

let solver2 (lines: string array) =
    let smallMap = parseInput lines
    let map = increaseMap smallMap

    let allNodes = getNodeSet map
    let initialState = initState allNodes map

    let endState = updateState initialState (0, 0)

    endState.Distances.[endState.MaxY, endState.MaxY]
    |> string
