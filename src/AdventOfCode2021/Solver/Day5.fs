module AdventOfCode2021.Solver.Day5

type Direction =
    | Vertical
    | Horizontal
    | Diagonal

type LineDefinition = { Start: int * int; End: int * int }

let getLineDefinition (token: string) =
    let elements = token.Split ','
    (elements.[0] |> int, elements.[1] |> int)

let getLineDefinitions (lines: string array) =
    lines
    |> Array.map
        (fun x ->
            let lineTokens = x.Split ' '

            { Start = getLineDefinition lineTokens.[0]
              End = getLineDefinition lineTokens.[2] })

let getDirection (line: LineDefinition) =
    if fst line.Start = fst line.End then
        Vertical
    elif snd line.Start = snd line.End then
        Horizontal
    else
        Diagonal

let isHorizontalOrVertical (line: LineDefinition) =
    match getDirection line with
    | Horizontal
    | Vertical -> true
    | Diagonal -> false

let visualizeMap (map: int [,]) =
    for i in 0 .. ((Array2D.length1 map) - 1) do
        for j in 0 .. ((Array2D.length2 map) - 1) do
            printf $"%i{map.[i, j]} "

        printf "\n"

let incrementAtPos (map: int [,]) (i: int, j: int) = map.[i, j] <- map.[i, j] + 1

let getMinMaxPos lineDef dimension =
    if dimension lineDef.Start < dimension lineDef.End then
        (dimension lineDef.Start, dimension lineDef.End)
    else
        (dimension lineDef.End, dimension lineDef.Start)


let calculateMap lineDefs =
    let xMax = 1000
    let yMax = 1000
    let map = Array2D.init xMax yMax (fun _ _ -> 0)

    for lineDef in lineDefs do
        let dir = getDirection lineDef

        match dir with
        | Horizontal ->
            let xMin, xMax = getMinMaxPos lineDef fst
            for i in xMin .. xMax do
                incrementAtPos map (i, snd lineDef.Start)
        | Vertical ->
            let yMin, yMax = getMinMaxPos lineDef snd
            for i in yMin .. yMax do
                incrementAtPos map (fst lineDef.Start, i)
        | Diagonal -> failwith "not yet"
    map

let flattenArray2D (a2d:int[,]) =
    seq {for i in 0 .. Array2D.length1 a2d - 1 do for j in 0 .. Array2D.length2 a2d - 1 do yield a2d.[i,j]}

let solver1 (lines: string array) =
    let lineDefs =
        getLineDefinitions lines
        |> Array.filter isHorizontalOrVertical

    let map = calculateMap lineDefs
    map |> flattenArray2D |> Seq.filter (fun x-> x > 1) |> Seq.length |> string


let solver2 (lines: string array) = failwith "error"
