(*
Work in progress: modeling combat.
*)

[<Measure>]type ft

type Direction = 
    | N
    | NW
    | W
    | SW
    | S
    | SE
    | E
    | NE

type Position = {
    North: int
    West: int
    }

let move (dir: Direction) (pos: Position) = 
    match dir with
    | N -> { pos with North = pos.North + 1 }
    | NW -> 
        { pos with 
            North = pos.North + 1
            West = pos.West + 1
        }
    | W -> { pos with West = pos.West + 1 }
    | SW ->
        { pos with
            North = pos.North - 1
            West = pos.West + 1
        }
    | S -> { pos with North = pos.North - 1 }
    | SE ->
        { pos with
            North = pos.North - 1
            West = pos.West - 1
        }
    | E -> { pos with West = pos.West - 1 }
    | NE ->
        { pos with
            North = pos.North + 1
            West = pos.West - 1
        }

let cellSize = 5<ft>

type CreatureID = | CreatureID of int

[<RequireQualifiedAccess>]
module Creature = 

    type Statistics = {
        Movement: int<ft>
        }

    type State = {
        MovementLeft: int<ft>
        Position: Position
        }
    
    let initialize (stats: Statistics, pos: Position) =
        {
            MovementLeft = stats.Movement
            Position = pos
        }

type World = {
    Initiative: CreatureID list
    Active: CreatureID
    Creatures: Map<CreatureID, Creature.State>
    }
    with
    static member Initialize(creatures: (CreatureID * Creature.Statistics * Position) list) =
        let initiative = 
            creatures 
            |> List.map (fun (creatureID, _, _) -> creatureID)
        {
            Initiative = initiative
            Active = initiative |> List.head
            Creatures = 
                creatures 
                |> List.map (fun (creatureId, stats, pos) -> 
                    creatureId,
                    Creature.initialize (stats, pos)
                    ) 
                |> Map.ofList 
        }

type Command = 
    | Move of Direction
    | Done

let update (creatureID: CreatureID, cmd: Command) (world: World) = 
    
    if world.Active <> creatureID
    then failwith (sprintf "Error: it is not %A's turn." creatureID)
    else
        let currentState = world.Creatures.[creatureID]

        match cmd with
        | Move(direction) ->
            let movementLeft = currentState.MovementLeft
            if movementLeft < cellSize
            then
                sprintf "Error: %A does not have enough movement left" creatureID
                |> failwith 
            else
                let updatedState = 
                    { currentState with 
                        Position = currentState.Position |> move direction 
                        MovementLeft = currentState.MovementLeft - cellSize
                    }
                { world with
                    Creatures = 
                        world.Creatures 
                        |> Map.add creatureID updatedState
                }
        | Done ->
            let activeIndex = 
                world.Initiative 
                |> List.findIndex (fun id -> id = creatureID)
            let nextUp = (activeIndex + 1) % world.Initiative.Length
            { world with Active = world.Initiative.Item nextUp }

let creature1 = 
    CreatureID 1, 
    { Creature.Movement = 30<ft> },
    { North = 0; West = 0 } 
    

let creature2 = 
    CreatureID 2, 
    { Creature.Movement = 20<ft> },
    { North = 5; West = 5 } 

let world = 
    [
        creature1
        creature2
    ]
    |> World.Initialize 

world 
|> update (CreatureID 1, Move N)
|> update (CreatureID 1, Move N)
|> update (CreatureID 1, Move N)
|> update (CreatureID 1, Move N)
|> update (CreatureID 1, Move SE) 
|> update (CreatureID 1, Done) 
|> update (CreatureID 2, Move SE) 
|> update (CreatureID 2, Done)
|> update (CreatureID 1, Move N) 
