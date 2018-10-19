(*
Explore modeling & simulating actions
*)

(*
TODO
- Move: a creature can move if remaining movement allows it
- Move: a creature can move if Terrain allows it
- Move can be extended with Dash action
- Cost of movement depends on Terrain
- How to handle no creature in Initiative?
- Initialization: prevent creatures in same position
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
        RemainingMovement: int
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
    } 

let initialize (creatures: (CreatureID * Creature.Info) list) = 
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
                Creature.State.RemainingMovement = 0
            }
            )
        |> Map.ofSeq
    {
        Turn = 1
        Initiative = initiative
        CreatureUp = initiative |> List.head
        Creatures = states
        CreatureStats = stats
    }

let handle state (id,cmd) =    
    match id = state.CreatureUp with 
    | false -> failwith "invalid command"
    | true ->
        match cmd with
        | Move(direction) -> 
            let currentState = state.Creatures.[state.CreatureUp]
            let nextPos = move currentState.Position direction
            let cost = 5 // TODO use Terrain information later
            let nextState = { 
                currentState with 
                    Position = nextPos
                    RemainingMovement = currentState.RemainingMovement - cost
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
                let nextUp = initiative |> List.item nextIndex
                { state with 
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

let initialState = 
    initialize [ 
        c1, c1Info 
        c2, c2Info 
        ]

let state1 = handle initialState (c1,Done)
let state2 = handle state1 (c2,Done)
let state3 = handle state2 (c1, Move North)
