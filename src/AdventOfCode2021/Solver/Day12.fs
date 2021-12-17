module AdventOfCode2021.Solver.Day12

open System


let getNodeDict (lines: string array) =
    let edges =
        lines
        |> Array.map
            (fun x ->
                let parts = x.Split '-'
                (parts.[0], parts.[1]))

    edges
    |> Array.fold
        (fun m el ->
            m
            |> Map.add
                (fst el)
                (m
                 |> Map.tryFind (fst el)
                 |> function
                     | Some x -> x @ [ (snd el) ]
                     | None -> [ (snd el) ])
            |> Map.add
                (snd el)
                (m
                 |> Map.tryFind (snd el)
                 |> function
                     | Some x -> x @ [ (fst el) ]
                     | None -> [ (fst el) ]))
        Map.empty

let rec walkSmallOnce
    (nodeDict: Map<string, string list>)
    (step: string)
    (currentPath: string list)
    (completePaths: string list list)
    =
    if step = "end" then
        (currentPath @ [ step ]) :: completePaths
    elif step.[0] |> Char.IsLower
         && List.contains step currentPath then
        completePaths
    else
        nodeDict.Item step
        |> List.map (fun x -> walkSmallOnce nodeDict x (currentPath @ [ step ]) completePaths)
        |> List.fold List.append List.empty

let containsAlreadyTwiceAndStep (step: string) (currentPath: string list) =
    let groupedLowerCase =
        currentPath
        |> List.filter (fun x -> x.[0] |> Char.IsLower && not (x = "start"))
        |> List.groupBy id

    let alreadyTwice =
        groupedLowerCase
        |> List.exists (fun x -> (snd x).Length > 1)

    alreadyTwice && List.contains step currentPath


let rec walkSmallMaybeTwice
    (nodeDict: Map<string, string list>)
    (step: string)
    (currentPath: string list)
    (completePaths: string list list)
    =
    if step = "end" then
        (currentPath @ [ step ]) :: completePaths
    elif step = "start" && not currentPath.IsEmpty then
        completePaths
    elif step.[0] |> Char.IsLower
         && containsAlreadyTwiceAndStep step currentPath then
        completePaths
    else
        nodeDict.Item step
        |> List.map (fun x -> walkSmallMaybeTwice nodeDict x (currentPath @ [ step ]) completePaths)
        |> List.fold List.append List.empty


let solver1 (lines: string array) =
    let nodeDict = getNodeDict lines
    let paths = walkSmallOnce nodeDict "start" [] []

    paths |> List.length |> string

let printPaths (paths: string list list) =
    for path in paths do
        printfn $"{String.Join(',', path |> List.toArray)}"

let solver2 (lines: string array) =
    let nodeDict = getNodeDict lines

    let paths =
        walkSmallMaybeTwice nodeDict "start" [] []

    paths |> List.length |> string
