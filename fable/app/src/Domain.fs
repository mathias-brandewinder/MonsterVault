namespace MonsterVault

[<Measure>]type ft

[<AutoOpen>]
module DiceRolls = 

    open System
    let rng = Random ()

    type Modifier = 
        | Advantage
        | Disadvantage

    type Dice = 
        | D of Sides : int
        static member (*) (times: int, dice: Dice) = Roll (times, dice)
    and Roll = 
        | Roll of int * Dice
        | Value of int
        | Add of Roll list
        static member (+) (v1: Roll, v2: Roll) = 
            match v1, v2 with
            | Add (rolls1), Add (rolls2) -> Add (rolls1 @ rolls2)
            | Add (rolls1), roll2 -> Add(rolls1 @ [ roll2 ])
            | roll1, Add (rolls2) -> Add(roll1 :: rolls2)
            | roll1, roll2 -> Add [ roll1; roll2 ]
        static member (+) (roll: Roll, num: int) = roll + Value num
        static member (+) (num: int, roll: Roll) = Value num + roll
        static member roll (roll: Roll) =
            match roll with
            | Roll (times, D (sides)) -> 
                Seq.init times (fun _ -> rng.Next(1, sides + 1))
                |> Seq.sum
            | Value (value) -> value
            | Add (rolls) -> rolls |> List.sumBy Roll.roll
        static member roll (roll: Roll, modifier: Modifier) =
            let roll1 = roll |> Roll.roll
            let roll2 = roll |> Roll.roll
            match modifier with
            | Disadvantage ->
                min roll1 roll2
            | Advantage ->
                max roll1 roll2
      
    let d4 = D 4
    let d6 = D 6
    let d8 = D 8
    let d10 = D 10
    let d12 = D 12
    let d20 = D 20

module Abilities = 

    type Ability = 
        | STR
        | DEX
        | CON 
        | INT 
        | WIS
        | CHA

    type Scores = {
        STR: int
        DEX: int
        CON: int
        INT: int
        WIS: int
        CHA: int
        }

    let scoreToModifier score = 
        (score / 2) - 5
        |> min 10
        |> max -5  

    let score scores ability =
        match ability with
        | STR -> scores.STR
        | DEX -> scores.DEX
        | CON -> scores.CON
        | INT -> scores.INT 
        | WIS -> scores.WIS
        | CHA -> scores.CHA  

    let modifier abilities ability = 
        ability
        |> score abilities
        |> scoreToModifier

module Space = 

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

module Weapons = 

    type Damage = Roll

    type Grip =
        | SingleHanded
        | TwoHanded

    type Usage = {
        Grip: Grip
        Damage: Damage
        }

    module Melee = 

        type Versatile = {
            SingleHandedDamage: Damage
            TwoHandedDamage: Damage
            }

        type Handling = 
            | Limited of Usage
            | Versatile of Versatile

        type Attacks = {
            Finesse: bool
            Handling: Handling
            Reach: int<ft>
            }

    module Ranged = 

        type Range = {
            Short: int<ft>
            Long: int<ft>
            }

        type Attacks = {
            Range: Range
            Usage: Usage
            }

    module Thrown = 

        type Attacks = {
            Melee: int<ft>
            ShortRange: int<ft>
            LongRange: int<ft>
            Handling: Melee.Handling
            }
            with
            member this.MeleeAttacks: Melee.Attacks = {
                Finesse = false
                Handling = this.Handling
                Reach = this.Melee
                }
            member this.RangedAttacks: Ranged.Attacks = {
                Range = { Short = this.ShortRange; Long = this.LongRange }
                Usage = 
                    match this.Handling with
                    | Melee.Limited(usage) -> usage
                    | Melee.Versatile(usage) -> 
                        { 
                            Grip = SingleHanded
                            Damage = usage.SingleHandedDamage 
                        }
                }

    type Attacks = 
        | Melee of Melee.Attacks
        | Ranged of Ranged.Attacks
        | Thrown of Thrown.Attacks

    type Proficiency = 
        | Simple
        | Martial

    type Weight = 
        | Light
        | Medium
        | Heavy 

    type Stats = {
        Proficiency: Proficiency
        Weight: Weight
        }

    type Weapon = {
        Name: string
        Stats: Stats
        Attacks: Attacks
        }

[<RequireQualifiedAccess>]
module Creature = 

    open Weapons
    open Abilities

    type Statistics = {
        Abilities: Scores
        ProficiencyBonus: int
        Movement: int<ft>
        HitPoints: int
        ArmorClass: int
        Attacks: List<Weapon> 
        WeaponsProficiency: Weapons.Proficiency
        }

module Attacks = 

    open Abilities
    open Weapons

    type AttackType = 
        | Melee
        | Ranged

    type Reach = 
        | Melee of int<ft>
        | Ranged of Ranged.Range

    type Statistics = {
        Weapon: string
        Grip: Grip
        Type: AttackType
        Reach: Reach
        HitBonus: int
        Damage: Roll
        }

    let abilityBonus (stats: Creature.Statistics) (weapon: Weapon) =
        
        let finesse = 
            match weapon.Attacks with
            | Attacks.Melee(info) -> info.Finesse
            | Attacks.Thrown(info) -> info.MeleeAttacks.Finesse
            | Attacks.Ranged(_) -> false

        match finesse with
        | true ->  [ STR; DEX ] 
        | false -> 
            match weapon.Attacks with
            | Attacks.Melee(_) -> [ STR]
            | Attacks.Ranged(_) -> [ DEX ]
            | Attacks.Thrown(_) -> [ STR ]
        |> Seq.maxBy (modifier stats.Abilities)
        |> modifier stats.Abilities

    let proficiencyBonus (stats: Creature.Statistics) (weapon: Weapon) =
                
        match (weapon.Stats.Proficiency, stats.WeaponsProficiency) with
        | Martial, Simple -> 0
        | _ -> stats.ProficiencyBonus

    let using (weapon: Weapon) (stats: Creature.Statistics) =

        let hitBonus = abilityBonus stats weapon + proficiencyBonus stats weapon  
        let damageBonus = abilityBonus stats weapon

        let meleeAttacks (attacks: Melee.Attacks) = 
            match attacks.Handling with
            | Melee.Limited(info) -> 
                {   
                    Weapon = weapon.Name
                    Type = AttackType.Melee
                    HitBonus = hitBonus
                    Grip = info.Grip
                    Damage = info.Damage + damageBonus
                    Reach = attacks.Reach |> Melee   
                }
                |> List.singleton
            | Melee.Versatile(info) ->
                [
                    {
                        Weapon = weapon.Name
                        Type = AttackType.Melee
                        HitBonus = hitBonus
                        Grip = SingleHanded
                        Damage = info.SingleHandedDamage + damageBonus
                        Reach = attacks.Reach |> Melee   
                    }
                    {
                        Weapon = weapon.Name
                        Type = AttackType.Melee
                        HitBonus = hitBonus
                        Grip = TwoHanded
                        Damage = info.TwoHandedDamage + damageBonus
                        Reach = attacks.Reach |> Melee   
                    }
                ]

        let rangedAttacks (info: Ranged.Attacks) = 
            {
                Weapon = weapon.Name
                Type = AttackType.Ranged
                HitBonus = hitBonus
                Grip = info.Usage.Grip
                Damage = info.Usage.Damage + damageBonus
                Reach = info.Range |> Ranged
            }
            |> List.singleton

        match weapon.Attacks with
        | Weapons.Melee(info) -> meleeAttacks info
        | Weapons.Ranged(info) -> rangedAttacks info
        | Weapons.Thrown(info) -> 
            [
                yield! info.MeleeAttacks |> meleeAttacks
                yield! info.RangedAttacks |> rangedAttacks
            ]    

module Domain =

    open DiceRolls
    open Space

    type CreatureID = | CreatureID of int
    
    type Action = 
        | Dash
        | Attack of Attacks.Statistics * CreatureID

    [<RequireQualifiedAccess>]
    module Creature = 

        type State = {
            MovementLeft: int<ft>
            HitPointsLeft: int
            Position: Position
            ActionTaken: Action option
            }
        
        let initialize (stats: Creature.Statistics, pos: Position) =
            {
                MovementLeft = stats.Movement
                HitPointsLeft = stats.HitPoints
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
                                match weapon.Reach with
                                | Attacks.Reach.Melee (reach) -> reach
                                | Attacks.Reach.Ranged (range) -> range.Long
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
        let creature = world.Statistics.[creatureID]
        let movements = 
            [ N; NW; W; SW; S; SE; E; NE ] 
            |> List.map Move
        let standardActions = 
            [ Dash ] 
            |> List.map Action
        let attacks = 
            world.Statistics.[creatureID].Attacks
            |> Seq.collect (fun weapon -> Attacks.using weapon creature)
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
            | Attack(attack, targetID) ->
                let creatureStats = world.Statistics.[creatureID]
                let targetState =
                    let targetState = world.Creatures.[targetID]
                    let dist = distance currentState.Position targetState.Position
                    let attackRoll = 
                        match attack.Reach with
                        | Attacks.Reach.Ranged(range) ->
                            if dist <= range.Short
                            then  1 * d20 |> Roll.roll
                            else  (1 * d20, Disadvantage) |> Roll.roll
                        | Attacks.Reach.Melee(_) -> 
                            1 * d20 |> Roll.roll
                    let target = world.Statistics.[targetID]
                    let targetAC = target.ArmorClass
                    let damage = 
                        if attackRoll = 1
                        then 0
                        elif attackRoll = 20
                        then 
                            Add [ attack.Damage; attack.Damage ]
                            |> Roll.roll
                        else
                            if attackRoll < targetAC
                            then 0
                            else attack.Damage |> Roll.roll
                    { targetState with 
                        HitPointsLeft = targetState.HitPointsLeft - damage 
                    }

                let creatureState = 
                    { currentState with 
                        ActionTaken = Some (Attack(attack, targetID))
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

    open Space
    open Weapons
    open Domain

    let scimitar = {
        Name = "scimitar"
        Stats = {
            Proficiency = Martial
            Weight = Light
            }
        Attacks = 
            Melee {
                Handling = Melee.Limited { Grip = SingleHanded; Damage = 1 * d6 }
                Reach = 5<ft>
                Finesse = true      
            }
        }

    let shortbow = { 
        Name = "shortbow"
        Stats = {
            Proficiency = Simple
            Weight = Medium
            }
        Attacks = 
            Ranged {
                Range = { Short = 80<ft>; Long = 320<ft> }
                Usage = { Grip = TwoHanded; Damage = 1 * d6 }
                }                    
        }

    let spear = {
        Name = "spear"
        Stats = {
            Proficiency = Simple
            Weight = Medium
            }
        Attacks = 
            Thrown {
                Melee = 5<ft>
                ShortRange = 20<ft>
                LongRange = 60<ft>
                Handling = Melee.Versatile {
                    SingleHandedDamage = 1 * d6
                    TwoHandedDamage = 1 * d8
                }
            }
        }

    let goblin = 
        let stats: Abilities.Scores = {
            STR = 8
            DEX = 14
            CON = 10
            INT = 10
            WIS = 8
            CHA = 8
            }
        { 
            Creature.Abilities = stats
            Creature.ProficiencyBonus = 2
            Creature.HitPoints = 7
            Creature.Movement = 30<ft>
            Creature.ArmorClass = 15
            Creature.WeaponsProficiency = Weapons.Martial
            Creature.Attacks = [ scimitar; shortbow ]
        }

    let creature1 = 
        CreatureID 1, 
        goblin,
        { North = 20; West = 20 } 
        
    let creature2 = 
        CreatureID 2, 
        goblin,
        { North = 25; West = 25 } 

    let map = {
        Width = 40
        Height = 40
        }

    let world =
        (map, [ creature1; creature2 ])
        |> World.Initialize 
