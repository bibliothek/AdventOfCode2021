module AdventOfCode2021.Solver.Day17

open System

type Pos = int * int

type TargetArea =
    { xMin: int
      xMax: int
      yMin: int
      yMax: int
      allPoints: Set<Pos> }

let isPosInTargetArea (pos: Pos) (target: TargetArea) = target.allPoints |> Set.contains pos

let hasPosOvershotTargetArea (pos: Pos) (target: TargetArea) =
    fst pos > target.xMax || snd pos < target.yMin

let getTargetArea (line: string) =
    let xBounds =
        ((line.Substring(line.IndexOf "x=" + 2)).Split ',').[0]
            .Split ".."

    let yBounds =
        (line.Substring(line.IndexOf "y=" + 2)).Split ".."

    let xMin, xMax = xBounds.[0] |> int, xBounds.[1] |> int
    let yMin, yMax = yBounds.[0] |> int, yBounds.[1] |> int

    let allPoints =
        seq {
            for x in xMin .. xMax do
                for y in yMin .. yMax do
                    yield (x, y)
        }
        |> Set.ofSeq

    { xMin = xMin
      xMax = xMax
      yMin = yMin
      yMax = yMax
      allPoints = allPoints }

let getPossibleVelocities (target: TargetArea) : Set<int * int> =
    seq {
        for x in 0 .. target.xMax do
            for y in target.yMin .. target.xMax do
                yield (x, y)
    }
    |> Set.ofSeq


let rec getMaxYPosition (velocity: int * int) (target: TargetArea) (currentPos: Pos) acc =
    if isPosInTargetArea currentPos target then
        Some acc
    elif hasPosOvershotTargetArea currentPos target then
        None
    else
        let xPos = fst currentPos + fst velocity
        let yPos = snd currentPos + snd velocity

        let xVel =
            match fst velocity with
            | i when i < 0 -> i + 1
            | i when i > 0 -> i - 1
            | _ -> 0

        let yVel = snd velocity - 1
        getMaxYPosition (xVel, yVel) target (xPos, yPos) (if yPos > acc then yPos else acc)

let solver1 (lines: string array) =
    let targetArea = getTargetArea lines.[0]
    let possibleVelocities = getPossibleVelocities targetArea

    let maxYPositions =
        possibleVelocities
        |> Set.fold
            (fun s el ->
                match (getMaxYPosition el targetArea (0, 0) Int32.MinValue) with
                | Some i -> Set.add i s
                | None -> s)
            Set.empty<int>

    maxYPositions |> Set.maxElement |> string

let solver2 (lines: string array) =
    let targetArea = getTargetArea lines.[0]
    let possibleVelocities = getPossibleVelocities targetArea

    let velocitiesThatHitTarget =
        possibleVelocities
        |> Set.filter
            (fun x ->
                match (getMaxYPosition x targetArea (0, 0) Int32.MinValue) with
                | Some _ -> true
                | None -> false)

    velocitiesThatHitTarget |> Set.count |> string
