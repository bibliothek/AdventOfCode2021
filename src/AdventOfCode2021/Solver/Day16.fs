module AdventOfCode2021.Solver.Day16

open System

type Literal = { value: int64; version: int }

type Operator = { typeId: int; version: int }

type Packet =
    | Literal of Literal
    | Operator of Operator

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
            Literal({ version = version; value = value })

        { state with
              Packets = state.Packets @ [ literalPacket ]
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
        Operator({ typeId = typeId; version = version })

    if lengthType = '0' then
        let numBits = state.Chars.[1..15] |> charArrayToInt
        let subChars = state.Chars.[16..(16+numBits - 1)]
        let subState = calculateSubState { Packets = [ packet ]
                                           Chars =  subChars}
        {Packets = state.Packets @ subState.Packets; Chars = state.Chars.[16+numBits..]}

    else
        let numSubPackets = state.Chars.[1..11] |> charArrayToInt

        let updatedState =
            { state with
                  Packets = state.Packets @ [ packet ]
                  Chars = state.Chars.[12..] }
        [| 0 .. (numSubPackets - 1) |]
        |> Array.fold (fun (state: State) _ -> readPacket state) updatedState

let solver1 (lines: string array) =
    let binaryRep = getBinaryRepresentation lines.[0]
    let state = readPacket (initState binaryRep)
    state.Packets |> List.sumBy (fun x -> match x with | Literal l -> l.version | Operator o -> o.version) |> string

let solver2 (lines: string array) = failwith "error"
