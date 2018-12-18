namespace MonsterVault

module Domain =

    open System

    type Dice = 
        | D of Sides : int
        static member (*) (times: int, dice: Dice) = Roll (times, dice)
    and Roll = 
        | Roll of int * Dice
        | Value of int
        | Add of Roll list
        static member (+) (v1: Roll, v2: Roll) = 
            match v1, v2 with
            | Add(rolls1), Add(rolls2) -> Add(rolls1 @ rolls2)
            | Add(rolls1), roll2 -> Add(rolls1 @ [ roll2 ])
            | roll1, Add(rolls2) -> Add(roll1 :: rolls2)
            | roll1, roll2 -> Add [ roll1 ; roll2 ]
        static member (+) (roll: Roll, num:int) = roll + Value num
        static member (+) (num: int, roll:Roll) = Value num + roll
        static member roll =
            let rng = Random ()
            fun (roll: Roll) ->
                match roll with
                | Roll (times, D (sides)) -> 
                    Seq.init times (fun _ -> rng.Next(1, sides))
                    |> Seq.sum
                | Value (value) -> value
                | Add(rolls) -> rolls |> List.sumBy Roll.roll

    let d4 = D 4
    let d6 = D 6
    let d8 = D 8
    let d10 = D 10
    let d12 = D 12
    let d20 = D 20

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
    
    let distance pos1 pos2 = 
        max
            (abs (pos1.North - pos2.North))
            (abs (pos1.West - pos2.West))
        |> fun d -> cellSize * d

    type CreatureID = | CreatureID of int
    
    module Weapon =
        
        type MeleeInfo = { 
            Range: int<ft> 
            }
        
        type RangedInfo = {
            ShortRange: int<ft>
            LongRange: int<ft>
            }

        type Attack = 
            | Melee of MeleeInfo
            | Ranged of RangedInfo

        type Description = {
            Name: string
            Attack: Attack
            HitBonus: int
            Damage: Roll
            }

    type Action = 
        | Dash
        | Attack of Weapon.Description * CreatureID

    type Reaction =
        | AttackOfOpportunity of Weapon.Description * CreatureID

    [<RequireQualifiedAccess>]
    module Creature = 

        type Statistics = {
            Movement: int<ft>
            HitPoints: int
            ArmorClass: int
            Attacks: List<Weapon.Description> 
            }

        type State = {
            MovementLeft: int<ft>
            HitPointsLeft: int
            Position: Position
            ActionTaken: Action option
            ReactionTaken: Reaction option
            }
        
        let initialize (stats: Statistics, pos: Position) =
            {
                MovementLeft = stats.Movement
                HitPointsLeft = stats.HitPoints
                Position = pos
                ActionTaken = None
                ReactionTaken = None
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

        let ``A creature can only attack within weapon range`` =
            { new Rule with
                member this.Validate world (creatureID, command) =
                    let currentState = world.Creatures.[creatureID]
                    match command with
                    | Action(action) ->
                        match action with
                        | Attack (weapon, targetID) ->
                            let target = world.Creatures.[targetID]
                            let dist = distance currentState.Position target.Position
                            let maximumDistance = 
                                match weapon.Attack with
                                | Weapon.Attack.Melee (info) -> info.Range
                                | Weapon.Attack.Ranged (info) -> info.LongRange
                            if dist <= maximumDistance
                            then Ok (creatureID, command)
                            else 
                                sprintf "maximum range for attack is %i" dist
                                |> errorMessage (creatureID, command) 
                                |> Error
                        | _ -> Ok (creatureID, command)
                    | _ -> Ok (creatureID, command)
            }

        let rules = [
            ``A creature must be active to act``
            ``A creature cannot move if it has not enough movement left``
            ``A creature cannot move to a space occupied by another creature``
            ``A creature can take at most one action per turn``
            ``A creature can only attack within weapon range``
            ]

        let validate world (creatureID, command) =
            (Ok (creatureID, command), rules)
            ||> Seq.fold (fun state rule -> 
                state
                |> Result.bind (rule.Validate world)
                )
    
    let alternatives (world: World) =

        let creatureID = world.Active
        let movements = 
            [ N; NW; W; SW; S; SE; E; NE ] 
            |> List.map Move
        let standardActions = 
            [ Dash ] 
            |> List.map Action
        let attacks = 
            world.Statistics.[creatureID].Attacks
            |> Seq.collect (fun attack -> 
                world.Initiative
                |> Seq.filter (fun targetID -> targetID <> creatureID)
                |> Seq.map (fun targetID ->
                    Attack(attack, targetID) 
                    |> Action
                    )
                )
            |> Seq.toList

        let miscellaneous = [ Done ]

        movements @ standardActions @ miscellaneous @ attacks
        |> List.map (fun action -> Rules.validate world (creatureID, action))
        |> List.filter (
            function 
            | Ok(_) -> true 
            | Error(_) -> false)
        |> List.map (
            function 
            | Ok(command) -> command
            | Error(_) -> failwith "Impossible"
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
            | Attack(weapon, targetID) ->
                let creatureStats = world.Statistics.[creatureID]
                let targetState =
                    let attack = 1 * d20 |> Roll.roll
                    printfn "Attack: %i" attack
                    let target = world.Statistics.[targetID]
                    let targetAC = target.ArmorClass
                    let targetState = world.Creatures.[targetID]
                    let damage = 
                        if attack < targetAC
                        then 0
                        else weapon.Damage |> Roll.roll   
                    printfn "Damage %i" damage                     
                    { targetState with HitPointsLeft = targetState.HitPointsLeft - damage }

                let creatureState = 
                    { currentState with 
                        ActionTaken = Some (Attack(weapon, targetID))
                    }

                { world with
                    Creatures = 
                        world.Creatures 
                        |> Map.add creatureID creatureState
                        |> Map.add targetID targetState
                }
        | Done ->
            let activeIndex = 
                world.Initiative 
                |> List.findIndex (fun id -> id = creatureID)
            let nextUp = (activeIndex + 1) % world.Initiative.Length
            let nextActiveID = world.Initiative.Item nextUp
            let nextActiveStats = world.Statistics.[nextActiveID]
            let nextActiveState = 
                { world.Creatures.[nextActiveID] with
                    MovementLeft = nextActiveStats.Movement
                    ActionTaken = None
                    ReactionTaken = None
                }
            { world with 
                Active = nextActiveID
                Creatures = 
                    world.Creatures 
                    |> Map.add nextActiveID nextActiveState  
            }

    let apply (creatureID, command) world =
        match Rules.validate world (creatureID, command) with
        | Error(msg) ->
            printfn "%s" msg
            world  
        | Ok(creatureID, command) -> 
            update world (creatureID, command)

    module TestSample = 

        let scimitar = {
            Weapon.Description.Name = "Scimitar"
            Weapon.Description.Attack = Weapon.Melee ({ Range = 5<ft> })
            Weapon.Description.HitBonus = 4
            Weapon.Description.Damage = 1 * d6
            }

        let shortbow = {
            Weapon.Description.Name = "Shortbow"
            Weapon.Description.Attack = 
                Weapon.Ranged ({ ShortRange = 80<ft>; LongRange = 320<ft> })
            Weapon.Description.HitBonus = 4
            Weapon.Description.Damage = 1 * d6
            }

        let creature1 = 
            CreatureID 1, 
            { 
                Creature.HitPoints = 7
                Creature.Movement = 30<ft>
                Creature.ArmorClass = 15
                Creature.Attacks = [ scimitar; shortbow ]
            },
            { North = 10; West = 10 } 
            
        let creature2 = 
            CreatureID 2, 
            { 
                Creature.HitPoints = 7
                Creature.Movement = 30<ft>
                Creature.ArmorClass = 15
                Creature.Attacks = [ scimitar ]
            },
            { North = 5; West = 5 } 

        let map = {
            Width = 40
            Height = 40
            }

        let world =
            (map, [ creature1; creature2 ])
            |> World.Initialize 
