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
            |> Map.add (fst el) (m|> Map.tryFind (fst el) |> function | Some x -> x @ [ (snd el) ] | None -> [(snd el)])
            |> Map.add (snd el) (m|> Map.tryFind (snd el) |> function | Some x -> x @ [ (fst el) ] | None -> [(fst el)]))
        Map.empty

let rec walk (nodeDict: Map<string, string list>) (step:string) (currentPath: string list) (completePaths: string list list) =
    if step = "end" then (currentPath @ [step]) :: completePaths
    elif step.[0] |> Char.IsLower && List.contains step currentPath    then
        completePaths
    else
        nodeDict.Item step |> List.map (fun x -> walk nodeDict x (currentPath @ [step]) completePaths) |> List.fold List.append List.empty


let solver1 (lines: string array) =
    let nodeDict = getNodeDict lines
    let paths = walk nodeDict "start" [] []

    paths |> List.length |> string

let solver2 (lines: string array) = failwith "error"
