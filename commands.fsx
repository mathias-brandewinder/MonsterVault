(*
Explore modeling & simulating actions
*)

(*
TODO
- Move can be extended with Dash action
- Move: a creature can move if remaining movement allows it
- Move: a creature can move if Terrain allows it
- Cost of movement depends on Terrain
- How to handle no creature in Initiative?
- Initialization: prevent creatures in same position
- Reset available/used movement when turn finishes
? Separate State elements that never change to limit copying
? How should impossible commands be handled
? Should creature state switch between Inactive | Active (data)
? Can I propose only possible Commands for a Creature
x Creature can always finish turn (Done)
x When Done, next creature in Initiative is Up
x When Initiative List is finished, Next Turn starts
*)

type CreatureID = | CreatureID of int

type Position = { 
    North: int
    West: int
    }

// restrict to 4 for now
type Direction = 
    | North
    | West
    | South
    | East

type Terrain = 
    | Difficult
    | Blocked

type Map = Map<Position,Terrain>
    
let move (pos:Position) (dir:Direction) =
    match dir with 
    | North -> { pos with North = pos.North + 1 }
    | West -> { pos with West = pos.West + 1 }
    | South -> { pos with North = pos.North - 1 }
    | East -> { pos with West = pos.West - 1 }

[<RequireQualifiedAccess>]
module Creature = 

    type Stats = {
        Movement: int
        }

    type State = {
        Position: Position
        MovementUsed: int
        }

    type Info = {
        Stats: Stats
        Position: Position
        }

type Command = 
    | Done
    | Move of Direction

type State = {
    Turn: int
    CreatureUp: CreatureID
    Initiative: CreatureID list
    Creatures: Map<CreatureID, Creature.State>
    CreatureStats: Map<CreatureID, Creature.Stats>
    Map: Map
    } 

let initialize (map: Map) (creatures: (CreatureID * Creature.Info) list) = 
    let initiative = creatures |> List.map fst
    let stats = 
        creatures 
        |> Seq.map (fun (id, info) -> id, info.Stats)
        |> Map.ofSeq
    let states = 
        creatures
        |> Seq.map (fun (id, info) -> 
            id,
            {   
                Creature.State.Position = info.Position
                Creature.State.MovementUsed = 0
            }
            )
        |> Map.ofSeq
    {
        Turn = 1
        Initiative = initiative
        CreatureUp = initiative |> List.head
        Creatures = states
        CreatureStats = stats
        Map = map
    }

let handle state (id,cmd) =    
    match id = state.CreatureUp with 
    | false -> failwith "invalid command"
    | true ->
        match cmd with
        | Move(direction) -> 
            let currentState = state.Creatures.[id]
            let nextPos = move currentState.Position direction
            let terrain = state.Map |> Map.tryFind nextPos
            let cost = 
                match terrain with
                | None -> 5 
                | Some(t) ->
                    match t with 
                    | Difficult -> 10
                    | Blocked -> failwith "Impossible move: destination is Blocked"                    
            let stats = state.CreatureStats.[id]
            
            // TODO incorporate Dash 
            if currentState.MovementUsed + cost > stats.Movement
            then failwith "Impossible move: creature exceeded maximum allowed movement"

            let nextState = { 
                currentState with 
                    Position = nextPos
                    MovementUsed = currentState.MovementUsed + cost
                }
            { state with
                Creatures = 
                    state.Creatures 
                    |> Map.add id nextState
            }
        | Done ->
            match state.Initiative with
            | [] -> 
                failwith "Impossible: no creature."
            | initiative ->
                let currentIndex = 
                    initiative
                    |> List.tryFindIndex (fun c -> id = c)
                let nextIndex, nextTurn = 
                    match currentIndex with
                    | None -> failwith "Impossible: cannot find creature in initiative."
                    | Some(index) ->
                        let nextIndex = index + 1
                        if nextIndex = (initiative |> List.length)
                        then 0, state.Turn + 1
                        else nextIndex, state.Turn
                
                let resetCurrentCreatureState = 
                    let current = state.Creatures.[id]
                    { current with MovementUsed = 0 }
                let nextUp = initiative |> List.item nextIndex

                { state with 
                    Creatures = 
                        state.Creatures 
                        |> Map.add id resetCurrentCreatureState
                    Turn = nextTurn
                    CreatureUp = nextUp
                }

// Trying things out
let c1 = CreatureID 1
let c1Info: Creature.Info = {
    Stats = { 
        Movement = 25 
        }
    Position = {
        North = 0
        West = 0
        }
    }

let c2 = CreatureID 2
let c2Info: Creature.Info = {
    Stats = { 
        Movement = 30 
        }
    Position = {
        North = 10
        West = 10
        }
    }

let map = 
    [
        { North = 1; West = 0 }, Difficult
        { North = 0; West = 1 }, Blocked
    ]
    |> Map.ofSeq

let initialState = 
    initialize 
        map
        [ 
        c1, c1Info 
        c2, c2Info 
        ]

let state1 = handle initialState (c1,Done)
let state2 = handle state1 (c2,Done)
let state3 = handle state2 (c1, Move North)
let state4 = handle state3 (c1, Move North)
let state5 = handle state4 (c1, Done)
let state6 = handle state5 (c2, Move South)
let state7 = handle state6 (c2, Move South)
let state8 = handle state7 (c2, Move South)
let state9 = handle state8 (c2, Move South)
let state10 = handle state9 (c2, Move South)
let state11 = handle state10 (c2, Move South)

// fails: exceeded movement
// let state12 = handle state11 (c2, Move South)

// test difficult, blocked terrain
let difficultTerrain = handle initialState (c1, Move North)
let blockedTerrain = handle initialState (c1, Move West)
