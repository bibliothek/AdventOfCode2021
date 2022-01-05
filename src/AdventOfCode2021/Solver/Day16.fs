module AdventOfCode2021.Solver.Day16

open System

type Packet = { typeId: int; version: int; value: int64 option; children: Packet list }

type State =
    { Packets: Packet list
      Chars: char [] }

let initState chars =
    { Packets = []; Chars = chars }

let getBinaryRepresentation (input: string) =
    input
    |> Seq.map
        (fun (x: char) ->
            let s =
                Convert.ToString(Convert.ToInt32(x.ToString(), 16), 2)
            String.Join("", (List.init (4 - s.Length) (fun _ -> "0")) @ [ s ]))

    |> fun x -> String.Join("", x)
    |> Seq.toArray

let charArrayToInt64 (chars: char []) =
    chars |> String |> fun x -> Convert.ToInt64(x, 2)

let charArrayToInt = charArrayToInt64 >> int

let rec readLiteral version (state: State) (previousGroups: char []) : State =
    if state.Chars.[0] = '0' then
        let value =
            previousGroups
            |> fun x ->
                Array.concat [ x; state.Chars.[1..4] ]
                |> charArrayToInt64

        let literalPacket =
            { version = version; value = Some value; children = []; typeId = 4 }

        {     Packets = state.Packets @ [literalPacket]
              Chars = state.Chars.[5..] }
    else
        readLiteral
            version
            { state with Chars = state.Chars.[5..] }
            (Array.concat [ previousGroups
                            state.Chars.[1..4] ])


let rec readPacket (state: State) : State =
    let version = state.Chars.[0..2] |> charArrayToInt
    let typeId = state.Chars.[3..5] |> charArrayToInt
    let updatedState = { state with Chars = state.Chars.[6..] }

    if typeId = 4 then
        readLiteral version updatedState [||]
    else
        readOperator version typeId updatedState

and calculateSubState (state:State) =
    if state.Chars.Length = 0 then state
    else
        calculateSubState (readPacket state)

and readOperator version typeId (state: State) : State =
    let lengthType = state.Chars.[0]

    let packet =
        { typeId = typeId; version = version; value = None; children = [] }

    if lengthType = '0' then
        let numBits = state.Chars.[1..15] |> charArrayToInt
        let subChars = state.Chars.[16..(16+numBits - 1)]
        let subState = calculateSubState { Packets = []
                                           Chars =  subChars}
        {Packets = state.Packets @ [{packet with children = subState.Packets }]; Chars = state.Chars.[16+numBits..]}

    else
        let numSubPackets = state.Chars.[1..11] |> charArrayToInt

        let updatedState =
            { state with
                  Packets = []
                  Chars = state.Chars.[12..] }
        let subState =
            [| 0 .. (numSubPackets - 1) |]
            |> Array.fold (fun (state: State) _ -> (readPacket state) ) updatedState
        {Packets = state.Packets @ [{packet with children = subState.Packets }]; Chars = subState.Chars}

let rec sumVersions (packets: Packet list) acc =
    match packets with
    | [] -> acc
    | head :: tail -> sumVersions tail (acc + head.version + (sumVersions head.children 0))

let solver1 (lines: string array) =
    let binaryRep = getBinaryRepresentation lines.[0]
    let state = readPacket (initState binaryRep)
    sumVersions state.Packets 0 |> string

let getFoldingOperation (typeId: int, head:int64)=
    match typeId with
    | 0 -> ((+), 0L)
    | 1 -> ((fun (state:int64) (el:int64) -> state * el), 1L)
    | 2 -> ((fun (state:int64) (el:int64) -> (if state < el then state else el)), head)
    | 3 -> ((fun (state:int64) (el:int64) -> (if state > el then state else el)), head)
    | _ -> failwith "unexpected type"

let getPairwiseOperation (typeId: int) (first:int64) (second:int64) =
    match typeId with
    | 5 -> if first > second then 1L else 0L
    | 6 -> if first < second then 1L else 0L
    | 7 -> if first = second then 1L else 0L
    | _ -> failwith "unexpected type"

let rec calculateResult (packets: Packet list) acc =
    match packets with
    | [] -> acc
    |  head :: tail  ->
            match head.value with
            | Some v -> calculateResult tail (acc @ [v])
            | None ->
                let subValues: int64 list = calculateResult head.children []
                if head.typeId < 5 then
                    let operation, init = getFoldingOperation (head.typeId, subValues.Head)
                    let result = subValues |> List.fold operation init
                    calculateResult tail (acc @ [result])
                else
                    let result = getPairwiseOperation head.typeId subValues.Head subValues.Tail.Head
                    calculateResult tail (acc @ [result])

let solver2 (lines: string array) =
    let binaryRep = getBinaryRepresentation lines.[0]
    let state = readPacket (initState binaryRep)
    (calculateResult state.Packets []) |> List.head |> string