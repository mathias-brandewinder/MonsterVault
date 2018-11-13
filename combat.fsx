(*
Work in progress: modeling combat.
*)

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

{ North = 0; West = 0 }
|> move N
|> move NW
|> move W

type CreatureID = | CreatureID of int

type World = {
    Creatures: Map<CreatureID, Position>
    }

type Command = 
    | Move of Direction

let update (creatureID: CreatureID, cmd: Command) (world: World) = 
    
    let currentPosition = world.Creatures.[creatureID]

    match cmd with
    | Move(direction) ->
        let updatedPosition = currentPosition |> move direction         
        { world with
            Creatures = 
                world.Creatures 
                |> Map.add creatureID updatedPosition
        }

let world = {
    Creatures = [
        CreatureID 1, { North = 0; West = 0 }
        CreatureID 2, { North = 5; West = 5 }
        ]
        |> Map.ofList
    }

world 
|> update (CreatureID 1, Move N)
|> update (CreatureID 2, Move SE) 
