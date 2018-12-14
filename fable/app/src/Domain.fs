namespace MonsterVault

module Domain =

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

    type Action = 
        | Dash

    type CreatureID = | CreatureID of int

    [<RequireQualifiedAccess>]
    module Creature = 

        type Statistics = {
            Movement: int<ft>
            }

        type State = {
            MovementLeft: int<ft>
            Position: Position
            ActionTaken: Action option
            }
        
        let initialize (stats: Statistics, pos: Position) =
            {
                MovementLeft = stats.Movement
                Position = pos
                ActionTaken = None
            }

    type BattleMap = {
        Width: int
        Height: int
        }

    type World = {
        BattleMap: BattleMap
        Initiative: CreatureID list
        Active: CreatureID
        Creatures: Map<CreatureID, Creature.State>
        Statistics: Map<CreatureID, Creature.Statistics>
        }
        with
        static member Initialize(map: BattleMap, creatures: (CreatureID * Creature.Statistics * Position) list) =
            let initiative = 
                creatures 
                |> List.map (fun (creatureID, _, _) -> creatureID)
            {
                BattleMap = map
                Initiative = initiative
                Active = initiative |> List.head
                Creatures = 
                    creatures 
                    |> List.map (fun (creatureId, stats, pos) -> 
                        creatureId,
                        Creature.initialize (stats, pos)
                        ) 
                    |> Map.ofList 
                Statistics = 
                    creatures 
                    |> List.map (fun (creatureId, stats, _) -> 
                        creatureId,
                        stats
                        ) 
                    |> Map.ofList 
            }

    type Command = 
        | Move of Direction
        | Action of Action
        | Done

    module Rules = 

        type Rule = 
            abstract member Validate: 
                World -> CreatureID * Command -> Result<CreatureID * Command, string>

        let errorMessage (creatureID, command) message =
            sprintf "%A / %A failed: %s" command creatureID message

        let ``A creature must be active to act`` =
            { new Rule with
                member this.Validate world (creatureID, command) =
                    if world.Active <> creatureID
                    then 
                        "it is not the creature's turn"
                        |> errorMessage (creatureID, command) 
                        |> Error
                    else Ok (creatureID, command)
            }

        let ``A creature cannot move if it has not enough movement left`` =
            { new Rule with
                member this.Validate world (creatureID, command) =
                    let currentState = world.Creatures.[creatureID]
                    match command with
                    | Move(_) ->
                        let movementLeft = currentState.MovementLeft
                        if movementLeft < cellSize
                        then
                            "creature does not have enough movement left"
                            |> errorMessage (creatureID, command) 
                            |> Error
                        else Ok (creatureID, command)
                    | _ -> Ok (creatureID, command)
            }

        let ``A creature cannot move to a space occupied by another creature`` =
            { new Rule with
                member this.Validate world (creatureID, command) =
                    let currentState = world.Creatures.[creatureID]
                    match command with
                    | Move(direction) ->
                        let destination = 
                            currentState.Position 
                            |> move direction
                        if world.Creatures |> Map.exists (fun ID state -> ID <> creatureID && state.Position = destination)
                        then
                            "cannot move into the space of another creature"
                            |> errorMessage (creatureID, command) 
                            |> Error
                        else Ok (creatureID, command)
                    | _ -> Ok (creatureID, command)
            }

        let ``A creature can take at most one action per turn`` =
            { new Rule with
                member this.Validate world (creatureID, command) =
                    let currentState = world.Creatures.[creatureID]
                    match command with
                    | Action(_) ->
                        match currentState.ActionTaken with
                        | Some(_) -> 
                            "creature has already taken one action this turn"
                            |> errorMessage (creatureID, command) 
                            |> Error
                        | None -> Ok (creatureID, command)
                    | _ -> Ok (creatureID, command)
            }

        let rules = [
            ``A creature must be active to act``
            ``A creature cannot move if it has not enough movement left``
            ``A creature cannot move to a space occupied by another creature``
            ``A creature can take at most one action per turn``
            ]

        let validate world (creatureID, command) =
            (Ok (creatureID, command), rules)
            ||> Seq.fold (fun state rule -> 
                state
                |> Result.bind (rule.Validate world)
                )

    let update (world: World) (creatureID: CreatureID, cmd: Command) = 
        
        let currentState = world.Creatures.[creatureID]
        match cmd with
        | Move(direction) ->
            let destination = 
                currentState.Position 
                |> move direction
            let updatedState = 
                { currentState with 
                    Position = destination 
                    MovementLeft = currentState.MovementLeft - cellSize
                }
            { world with
                Creatures = 
                    world.Creatures 
                    |> Map.add creatureID updatedState
            }
        | Action(action) ->
            match action with
            | Dash ->
                let creatureStats = world.Statistics.[creatureID]
                let creatureState = 
                    { currentState with 
                        MovementLeft = currentState.MovementLeft + creatureStats.Movement
                        ActionTaken = Some Dash
                    }
                { world with
                    Creatures = 
                        world.Creatures 
                        |> Map.add creatureID creatureState
                }
        | Done ->
            let creatureStats = world.Statistics.[creatureID]
            let creatureState = 
                { currentState with 
                    MovementLeft = creatureStats.Movement 
                    ActionTaken = None
                }
            let activeIndex = 
                world.Initiative 
                |> List.findIndex (fun id -> id = creatureID)
            let nextUp = (activeIndex + 1) % world.Initiative.Length
            let nextActive = world.Initiative.Item nextUp
            { world with 
                Active = nextActive
                Creatures = 
                    world.Creatures 
                    |> Map.add creatureID creatureState     
            }

    let apply (creatureID, command) world =
        match Rules.validate world (creatureID, command) with
        | Error(msg) ->
            printfn "%s" msg
            world  
        | Ok(creatureID, command) -> 
            update world (creatureID, command)

    module TestSample = 

        let creature1 = 
            CreatureID 1, 
            { Creature.Movement = 30<ft> },
            { North = 10; West = 10 } 
            
        let creature2 = 
            CreatureID 2, 
            { Creature.Movement = 20<ft> },
            { North = 5; West = 5 } 

        let map = {
            Width = 40
            Height = 40
            }

        let world =
            (map, [ creature1; creature2 ])
            |> World.Initialize 
