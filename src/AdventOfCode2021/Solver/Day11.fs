module AdventOfCode2021.Solver.Day11

type Octopus =
    { alreadyFired: bool
      energyLevel: int }

let getOctopusMap (lines: string array) =
    Array2D.init
        lines.[0].Length
        lines.Length
        (fun i j ->
            { alreadyFired = false
              energyLevel =
                  lines.[j] |> Seq.toArray |> Array.get <| i
                  |> System.Char.GetNumericValue
                  |> int })

let getOctopiAboutToFire (map: Octopus [,]) =
    seq {
        for i in 0 .. (Array2D.length1 map - 1) do
            for j in 0 .. (Array2D.length2 map - 1) do
                if map.[i, j].energyLevel > 9 then
                    yield (i, j)
    }
    |> Set.ofSeq

let isInBounds (map: Octopus [,]) (pos: int * int) =
    let x, y = pos

    x >= 0
    && y >= 0
    && x < (map |> Array2D.length1)
    && y < (map |> Array2D.length2)

let increaseIfInBoundsAndNotAlreadyFired (map: Octopus [,]) (pos: int * int) =
    if isInBounds map pos
       && not map.[fst pos, snd pos].alreadyFired then
        let octopus = map.[fst pos, snd pos]

        map.[fst pos, snd pos] <-
            { octopus with
                  energyLevel = octopus.energyLevel + 1 }

        Set.empty.Add pos
    else
        Set.empty

let increaseSurroundingOctopi (map: Octopus [,]) (pos: int * int) =
    let top =
        increaseIfInBoundsAndNotAlreadyFired map (fst pos, snd pos - 1)

    let topRight =
        increaseIfInBoundsAndNotAlreadyFired map (fst pos + 1, snd pos - 1)

    let right =
        increaseIfInBoundsAndNotAlreadyFired map (fst pos + 1, snd pos)

    let bottomRight =
        increaseIfInBoundsAndNotAlreadyFired map (fst pos + 1, snd pos + 1)

    let bottom =
        increaseIfInBoundsAndNotAlreadyFired map (fst pos, snd pos + 1)

    let bottomLeft =
        increaseIfInBoundsAndNotAlreadyFired map (fst pos - 1, snd pos + 1)

    let left =
        increaseIfInBoundsAndNotAlreadyFired map (fst pos - 1, snd pos)

    let topLeft =
        increaseIfInBoundsAndNotAlreadyFired map (fst pos - 1, snd pos - 1)

    top
    |> Set.union topRight
    |> Set.union right
    |> Set.union bottomRight
    |> Set.union bottom
    |> Set.union bottomLeft
    |> Set.union left
    |> Set.union topLeft


let rec fireOctopi (map: Octopus [,]) (firingOctopi: Set<(int * int)>) =
    if firingOctopi.Count = 0 then
        map
    else
        let head = firingOctopi |> Set.minElement
        let tail = firingOctopi |> Set.remove head
        let octopus = map.[fst head, snd head]

        if octopus.alreadyFired || octopus.energyLevel <= 9 then
            fireOctopi map tail
        else
            map.[fst head, snd head] <- { alreadyFired = true; energyLevel = 0 }
            let surroundingOctopi = increaseSurroundingOctopi map head
            fireOctopi map (tail |> Set.union surroundingOctopi)

let calculateOneIteration (map: Octopus [,]) =
    let initialIncrease =
        map
        |> Array2D.map
            (fun x ->
                { alreadyFired = false
                  energyLevel = x.energyLevel + 1 })

    let firingOctopi = getOctopiAboutToFire initialIncrease

    initialIncrease |> fireOctopi <| firingOctopi

let countFlashes (map: Octopus [,]) =
    seq {
        for i in 0 .. Array2D.length1 map - 1 do
            for j in 0 .. Array2D.length2 map - 1 do
                yield map.[i, j]
    }
    |> Seq.fold
        (fun count octopus ->
            if octopus.energyLevel = 0 then
                count + 1
            else
                count)
        0

let rec getFlashes (map: Octopus [,]) (idx: int) (iterNum: int) (numFlashes: int) =
    if idx = iterNum then
        numFlashes + countFlashes map
    else
        getFlashes (calculateOneIteration map) (idx + 1) iterNum (numFlashes + countFlashes map)

let solver1 (lines: string array) =
    let map = getOctopusMap lines
    let flashes = getFlashes map 0 100 0
    flashes |> string

let rec getSynchronizedFlash (map: Octopus [,]) (idx: int) (maxFlashes: int) =
    let flashes = countFlashes map

    if flashes = maxFlashes then
        idx
    else
        getSynchronizedFlash (calculateOneIteration map) (idx + 1) maxFlashes

let solver2 (lines: string array) =
    let map = getOctopusMap lines

    let flashes =
        getSynchronizedFlash map 0 (Array2D.length1 map * Array2D.length2 map)

    flashes |> string
