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

    type Grip =
        | SingleHanded
        | TwoHanded

    type Usage = {
        Grip: Grip
        Damage: Roll
        }

    module Melee = 

        type Versatile = {
            SingleHandedDamage: Roll
            TwoHandedDamage: Roll
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

    open Space

    type CreatureID = | CreatureID of int
    type GroupID = | GroupID of int

    type CombatOutcome = 
        | Draw 
        | Victory of GroupID 

    type CombatState = 
        | Ongoing 
        | Finished of CombatOutcome

    type TurnState = {
        Creature: CreatureID
        MovementLeft: int<ft>
        HasTakenAction: bool
        }

    [<RequireQualifiedAccess>]
    module Creature = 

        type State = {
            HasTakenReaction: bool
            HitPoints: int
            Position: Position
            Dead: bool
            Group: GroupID
            }
            with 
            member this.CanAct =
                not (this.Dead)
            member this.CanReact =
                this.CanAct && (not this.HasTakenReaction)   
       
        let initialize (stats: Creature.Statistics, group: GroupID, pos: Position) =
            {
                HitPoints = stats.HitPoints
                Group = group
                Position = pos
                HasTakenReaction = false
                Dead = false
            }

    type BattleMap = {
        Width: int
        Height: int
        }

    type GlobalState = {
        BattleMap: BattleMap
        Initiative: CreatureID list
        Turn: Option<TurnState>
        CreatureState: Map<CreatureID, Creature.State>
        Statistics: Map<CreatureID, Creature.Statistics>
        }
        with
        static member Initialize(map: BattleMap, creatures: (CreatureID * GroupID * Creature.Statistics * Position) list) =
            let initiative = 
                creatures 
                |> List.map (fun (creatureID, _, _, _) -> creatureID)
            let turn = 
                match creatures with 
                | [] -> None
                | (creature, _, stats, _) :: _ ->
                    {
                        Creature = creature 
                        MovementLeft = stats.Movement
                        HasTakenAction = false
                    }
                    |> Some
            {
                BattleMap = map
                Initiative = initiative
                Turn = turn
                CreatureState = 
                    creatures 
                    |> List.map (fun (creatureId, group, stats, pos) -> 
                        creatureId,
                        Creature.initialize (stats, group, pos)
                        ) 
                    |> Map.ofList 
                Statistics = 
                    creatures 
                    |> List.map (fun (creatureId, _, stats, _) -> 
                        creatureId,
                        stats
                        ) 
                    |> Map.ofList 
            }
        static member CombatState state = 
            match state.Initiative with 
            | [] -> Finished Draw
            | _ ->
                let activeGroups = 
                    state.CreatureState 
                    |> Map.filter (fun key value -> value.CanAct)
                    |> Seq.map (fun kv -> kv.Value.Group)
                    |> Seq.distinct
                    |> Seq.toList
                match activeGroups with
                | [] -> Finished Draw 
                | [ winner ] -> Finished (Victory winner)
                | _ -> Ongoing
                
    type Outcome =
        | Move of CreatureID * Direction
        | SuccessfulAttack of CreatureID * CreatureID * int
        | FailedAttack of CreatureID * CreatureID
        with 
        static member applyEffect (outcome: Outcome) (state: GlobalState) =
            match outcome with
            | Outcome.Move (creature, direction) ->                  
                let currentTurn = state.Turn.Value
                let currentState = state.CreatureState.[creature]
                let updatedState = { currentState with Position = currentState.Position |> move direction }
                { state with
                    CreatureState = state.CreatureState |> Map.add creature updatedState 
                    Turn = 
                        { currentTurn with 
                            MovementLeft = currentTurn.MovementLeft - 5<ft> 
                        }
                        |> Some
                }
            | Outcome.FailedAttack _ -> state
            | Outcome.SuccessfulAttack (_, target, damage) -> 
                // TODO: handle dying state, instant death
                let targetState = state.CreatureState.[target]
                let hitPoints = max 0 targetState.HitPoints - damage
                let updatedState = 
                    { targetState with
                        HitPoints = hitPoints 
                        Dead = hitPoints <= 0
                    }
                { state with
                    CreatureState =
                        state.CreatureState
                        |> Map.add target updatedState
                }
        static member updateAction (outcome: Outcome) (state: GlobalState) =
            match outcome with
            | Outcome.Move _ -> state         
            | Outcome.FailedAttack _ -> 
                { state with 
                    Turn = Some { state.Turn.Value with HasTakenAction = true } 
                }
            | Outcome.SuccessfulAttack _ -> 
                { state with 
                    Turn = Some { state.Turn.Value with HasTakenAction = true } 
                }
        static member updateReaction (outcome: Outcome) (state: GlobalState) =
            match outcome with
            | Outcome.Move _ -> failwith "Error: move is not a possible reaction"
            | Outcome.FailedAttack (source, target) -> 
                let attackerState = state.CreatureState.[source]
                { state with 
                    CreatureState = 
                        state.CreatureState
                        |> Map.add source { attackerState with HasTakenReaction = true } 
                }
            | Outcome.SuccessfulAttack (source, target, damage) -> 
                let attackerState = state.CreatureState.[source]
                { state with 
                    CreatureState = 
                        state.CreatureState
                        |> Map.add source { attackerState with HasTakenReaction = true } 
                }

    module Actions = 

        type Action =
            | Move of Direction
            | Attack of (CreatureID * Attacks.Statistics)

        type ActionTaken =
            | FinishTurn 
            | Action of Action

        let alternatives (state: GlobalState) =
            state.Turn
            |> Option.bind (fun turn ->
                let creature = turn.Creature
                if state.CreatureState.[creature].CanAct
                then 
                    [           
                        if turn.MovementLeft >= 5<ft>
                        then 
                            yield! 
                                [ N; NW; W; SW; S; SE; E; NE ]
                                |> List.map (fun dir -> Action(Move(dir))) 
                        if (not turn.HasTakenAction)
                        then                        
                            let weapons = state.Statistics.[creature].Attacks
                            let attackerStats = state.Statistics.[creature]
                            let attacks = weapons |> List.collect (fun w -> Attacks.using w attackerStats)

                            yield! 
                                state.Initiative
                                |> List.filter (fun target -> target <> creature)
                                |> List.filter (fun target -> not (state.CreatureState.[target].Dead))
                                |> List.collect (fun target -> 
                                    attacks 
                                    |> List.map (fun attack -> Action(Attack (target, attack))))

                        yield FinishTurn
                    ]
                    |> Some
                else None
                )

    module Reactions = 

        type Reaction =
            | OpportunityAttack of (CreatureID * Attacks.Statistics)
            | Riposte of CreatureID

        type ReactionTaken = 
            | Pass 
            | Reaction of Reaction

        let toAction (globalState: GlobalState) (trigger: CreatureID, outcome: Outcome) (creature: CreatureID) =
            if creature = trigger
            then None
            elif (not globalState.CreatureState.[creature].CanReact)
            then None
            else
                match outcome with
                | Move _ -> 
                    // do we have melee attacks
                    let weapons = globalState.Statistics.[creature].Attacks
                    let attackerStats = globalState.Statistics.[creature]
                    let attacks = 
                        weapons 
                        |> List.collect (fun w -> Attacks.using w attackerStats)
                        |> List.filter (fun w -> w.Type = Attacks.Melee)
                    match attacks with 
                    | [] -> None 
                    | _ -> 
                        attacks
                        |> List.map (fun attack -> OpportunityAttack (trigger, attack))
                        |> fun oppAttacks -> Some(creature, oppAttacks)
                | SuccessfulAttack _ -> None 
                | FailedAttack _ -> None

        let toReaction (globalState: GlobalState) (trigger: CreatureID, outcome: Outcome) (creature: CreatureID) =
            if creature = trigger
            then None
            elif (not globalState.CreatureState.[creature].CanReact)
            then None
            else
                match outcome with
                | Move _ -> None
                | SuccessfulAttack (origin, target, _) -> 
                    if creature = target 
                    then Some (creature, [ Riposte origin ])
                    else None 
                | FailedAttack _ -> None

    open Actions 
    open Reactions 

    type ActionNeeded = {
        Creature: CreatureID
        Alternatives: list<ActionTaken>
        }

    type ReactionNeeded = {
        Creature: CreatureID
        Alternatives: list<ReactionTaken>
        }

    type ReactionsChecked = {
        Unchecked: list<CreatureID>
        Checked: list<CreatureID>
        }
        with
        static member check creatureID checks = 
            { checks with
                Unchecked = checks.Unchecked |> List.filter (fun x -> x <> creatureID)
                Checked = creatureID :: checks.Checked
            } 

    type UnconfirmedActionResult = {
        ReactionsChecked: ReactionsChecked
        Creature: CreatureID
        Action: Action
        Outcome: Outcome
        }

    type UnconfirmedReactionResult = {
        ReactionsChecked: ReactionsChecked
        Creature: CreatureID
        Reaction: Reaction
        Outcome: Outcome
        }

    type WaitingForConfirmation = 
        | Action of UnconfirmedActionResult
        | Reaction of UnconfirmedReactionResult * WaitingForConfirmation

    type Machine = 
        | CombatFinished of CombatOutcome
        | ActionNeeded of ActionNeeded
        | ReactionNeeded of ReactionNeeded * WaitingForConfirmation
        with 
        static member Reacting machine =
            let rec reacting acc pending = 
                match pending with 
                | WaitingForConfirmation.Action _ -> acc
                | WaitingForConfirmation.Reaction (reaction, rest) -> 
                    let acc = reaction.Creature :: acc
                    reacting acc rest        
            match machine with 
            | CombatFinished _ -> []
            | ActionNeeded _ -> []
            | ReactionNeeded(reaction, pending) ->
                reacting [ reaction.Creature ] pending 

    type Transition = 
        | InvalidCommand of string
        | StartCombat of (unit -> GlobalState * Machine)
        | FinishTurn of CreatureID
        | AttemptAction of (CreatureID * Actions.Action)
        | ConfirmAction of UnconfirmedActionResult
        | ExecuteAction of (CreatureID * Outcome)
        | ActionCompleted
        | ActionCancelled
        | ReactionTriggered of (CreatureID * ReactionNeeded)
        | AttemptReaction of (CreatureID * Reactions.Reaction)
        | ConfirmReaction of UnconfirmedReactionResult
        | ReactionCompleted
        | ReactionCancelled of CreatureID
        | ExecuteReaction of (CreatureID * Outcome)
        | PassReaction of CreatureID

    let rec execute (globalState: GlobalState, machine: Machine) (transition: Transition) : (GlobalState * Machine) =
        printfn "%A" transition
        match transition with
        | InvalidCommand _ -> globalState, machine
        | StartCombat init -> init ()
        | FinishTurn creature ->
            match (GlobalState.CombatState globalState) with 
            | Finished result -> globalState, Machine.CombatFinished result
            | Ongoing -> 
                let turn = globalState.Turn.Value
                let nextCreatureUp = 
                    globalState.Initiative 
                    |> List.findIndex (fun x -> x = turn.Creature)
                    |> fun index -> (index + 1) % (globalState.Initiative.Length)
                    |> fun index -> globalState.Initiative.Item index
                let nextTurn = {
                    Creature = nextCreatureUp
                    MovementLeft = 30<ft>                
                    HasTakenAction = false
                    }        
                let nextCreatureState = 
                    { globalState.CreatureState.[nextCreatureUp] with
                        HasTakenReaction = false
                    }
                let globalState = 
                    { globalState with 
                        Turn = Some nextTurn 
                        CreatureState = 
                            globalState.CreatureState
                            |> Map.add nextCreatureUp nextCreatureState
                    }
                let alternatives = Actions.alternatives globalState
                match alternatives with
                | None -> FinishTurn nextCreatureUp |> execute (globalState, machine)
                | Some alternatives ->
                    globalState, 
                    ActionNeeded({ Creature = nextCreatureUp; Alternatives = alternatives })

        | AttemptAction (creature, action) -> 
            let outcome = 
                match action with
                | Action.Move dir -> Outcome.Move (creature, dir)
                | Attack (target, attack) -> 
                    // TODO properly handle attack resolution
                    let attackRoll = Roll.roll (1 * d20) + attack.HitBonus
                    let ac = globalState.Statistics.[target].ArmorClass
                    if attackRoll < ac
                    then Outcome.FailedAttack (creature, target)
                    else Outcome.SuccessfulAttack (creature, target, attack.Damage |> Roll.roll)
            let reactions = 
                globalState.Initiative
                |> List.choose (
                    Reactions.toAction globalState (creature, outcome))
            match reactions with
            | [] -> 
                ExecuteAction (creature, outcome) 
                |> execute (globalState, machine)
            | _ ->
                let unconfirmed : UnconfirmedActionResult = {
                    ReactionsChecked = { 
                        Unchecked = globalState.Initiative
                        Checked = []
                        }
                    Creature = creature
                    Action = action
                    Outcome = outcome
                    }
                ConfirmAction unconfirmed
                |> execute (globalState, machine)

        | ConfirmAction unconfirmed ->
            let reactions = 
                unconfirmed.ReactionsChecked.Unchecked
                |> List.choose (
                    Reactions.toAction globalState (unconfirmed.Creature, unconfirmed.Outcome))
            match reactions with
            | [] -> 
                ExecuteAction (unconfirmed.Creature, unconfirmed.Outcome) 
                |> execute (globalState, machine)
            | (triggered, reactions) :: _ ->
                let reactionNeeded = {
                    ReactionNeeded.Creature = triggered
                    Alternatives = 
                        ReactionTaken.Pass ::
                        (reactions
                        |> List.map ReactionTaken.Reaction)                   
                        }
                let machine = 
                    Machine.ReactionNeeded(
                        reactionNeeded, 
                        WaitingForConfirmation.Action(unconfirmed)
                        )
                ReactionTriggered(triggered, reactionNeeded)
                |> execute (globalState, machine)  

        | ExecuteAction (creature, outcome) ->
            let state = 
                globalState
                |> Outcome.updateAction outcome
                |> Outcome.applyEffect outcome 
            ActionCompleted |> execute (state, machine)   

        | ActionCancelled ->
            let currentTurn = globalState.Turn.Value
            FinishTurn currentTurn.Creature 
            |> execute (globalState, machine)

        | ActionCompleted ->
            match (GlobalState.CombatState globalState) with 
            | Finished result -> globalState, Machine.CombatFinished result
            | Ongoing -> 
                match globalState.Turn with 
                | None -> failwith "Impossible: when an action completes, there must be a turn"
                | Some turn ->
                    match Actions.alternatives globalState with 
                    | None -> 
                        FinishTurn turn.Creature 
                        |> execute (globalState, machine)
                    | Some alternatives ->
                        let machine = 
                            ActionNeeded({ Creature = turn.Creature; Alternatives = alternatives })
                        globalState, machine 

        | ReactionTriggered (creature, reactionNeeded) -> 
            globalState, machine

        | AttemptReaction (creature, reaction) ->
            let outcome = 
                match reaction with
                | Reaction.OpportunityAttack (target, attackStatistics) -> 
                    // TODO properly handle attack resolution
                    Outcome.SuccessfulAttack (creature, target, 1)
                | Reaction.Riposte target -> 
                    // TODO properly handle riposte / attack resolution
                    Outcome.SuccessfulAttack (creature, target, 1) 
            let alreadyReacting = machine |> Machine.Reacting
            let notReacting = 
                globalState.Initiative 
                |> List.filter (fun x -> not (alreadyReacting |> List.contains x))     
            let reactions = 
                notReacting
                |> List.choose (
                    Reactions.toReaction globalState (creature, outcome))

            match reactions with
            | [] -> 
                ExecuteReaction (creature, outcome) 
                |> execute (globalState, machine)
            | _ ->
                let unconfirmed : UnconfirmedReactionResult = {
                    ReactionsChecked = {
                        Unchecked = notReacting
                        Checked = alreadyReacting
                    }
                    Creature = creature
                    Reaction = reaction
                    Outcome = outcome
                    }
                ConfirmReaction unconfirmed
                |> execute (globalState, machine)

        | ConfirmReaction unconfirmed ->
            let reactions = 
                unconfirmed.ReactionsChecked.Unchecked
                |> List.choose (
                    Reactions.toReaction globalState (unconfirmed.Creature, unconfirmed.Outcome))
            match reactions with
            | [] -> 
                ExecuteReaction (unconfirmed.Creature, unconfirmed.Outcome) 
                |> execute (globalState, machine)
            | (triggered, reactions) :: _ ->
                let reactionNeeded = {
                    ReactionNeeded.Creature = triggered
                    Alternatives = 
                        ReactionTaken.Pass ::
                        (reactions
                        |> List.map ReactionTaken.Reaction)                   
                        }
                       
                let pending = 
                    match machine with 
                    | CombatFinished _ -> failwith "Not possible"
                    | Machine.ActionNeeded _ -> failwith "Not possible"
                    | Machine.ReactionNeeded (_, pending) -> pending
                let machine = 
                    Machine.ReactionNeeded(
                        reactionNeeded, 
                        WaitingForConfirmation.Reaction(unconfirmed, pending)
                        )
                ReactionTriggered(triggered, reactionNeeded)
                |> execute (globalState, machine)  

        | ExecuteReaction (creature, outcome) ->        
            let state = 
                globalState
                |> Outcome.updateReaction outcome
                |> Outcome.applyEffect outcome 

            ReactionCompleted |> execute (state, machine)

        | ReactionCompleted ->
            match (GlobalState.CombatState globalState) with 
            | Finished result -> globalState, Machine.CombatFinished result
            | Ongoing -> 
                match machine with 
                | Machine.CombatFinished _ -> failwith "Impossible state"
                | Machine.ActionNeeded pending ->  failwith "Impossible state"
                | Machine.ReactionNeeded (reaction, pending) -> 
                    match pending with
                    | WaitingForConfirmation.Action unconfirmed ->
                        // TODO check if other conditions cancel the original Action
                        if globalState.CreatureState.[unconfirmed.Creature].HitPoints > 0
                        then 
                            ConfirmAction unconfirmed 
                            |> execute (globalState, machine)
                        else 
                            ActionCancelled 
                            |> execute (globalState, machine)
                    | WaitingForConfirmation.Reaction (unconfirmed, pending) -> 
                        let fakeReaction =  { ReactionNeeded.Creature = unconfirmed.Creature; Alternatives = [] }
                        let machine = Machine.ReactionNeeded (fakeReaction, pending)
                        // TODO check if other conditions cancel the original reaction
                        if globalState.CreatureState.[unconfirmed.Creature].HitPoints > 0
                        then 
                            ConfirmReaction unconfirmed 
                            |> execute (globalState, machine)
                        else 
                            ReactionCancelled unconfirmed.Creature
                            |> execute (globalState, machine)

        | PassReaction creature ->
            let pending = 
                match machine with
                | CombatFinished _ -> failwith "Impossible state"
                | ActionNeeded _ -> failwith "Impossible state"
                | ReactionNeeded (_, pending) -> pending
            match pending with 
            | WaitingForConfirmation.Action unconfirmed -> 
                let unconfirmed = 
                    { unconfirmed with
                        ReactionsChecked = 
                            unconfirmed.ReactionsChecked 
                            |> ReactionsChecked.check creature 
                    }
                ConfirmAction unconfirmed
                |> execute (globalState, machine)                       
            | WaitingForConfirmation.Reaction (unconfirmed, pending) -> 
                let unconfirmed = 
                    { unconfirmed with
                        ReactionsChecked = 
                            unconfirmed.ReactionsChecked 
                            |> ReactionsChecked.check creature 
                    }
                let fakeReaction = { ReactionNeeded.Creature = unconfirmed.Creature; Alternatives = [] }
                let machine = Machine.ReactionNeeded (fakeReaction, pending)
                ConfirmReaction unconfirmed
                |> execute (globalState, machine)

        | ReactionCancelled creature -> 
            // identical to PassReaction,
            // but mark reaction as taken
            let pending = 
                match machine with
                | CombatFinished _ -> failwith "Impossible state"
                | ActionNeeded _ -> failwith "Impossible state"
                | ReactionNeeded (_, pending) -> pending

            let creatureState = 
                { globalState.CreatureState.[creature] with 
                    HasTakenReaction = true 
                }
            let globalState = 
                { globalState with 
                    CreatureState = 
                        globalState.CreatureState 
                        |> Map.add creature creatureState
                }

            match pending with 
            | WaitingForConfirmation.Action unconfirmed -> 
                let unconfirmed = 
                    { unconfirmed with
                        ReactionsChecked = 
                            unconfirmed.ReactionsChecked 
                            |> ReactionsChecked.check creature 
                    }
                ConfirmAction unconfirmed
                |> execute (globalState, machine)            
            | WaitingForConfirmation.Reaction (unconfirmed, pending) -> 
                let unconfirmed = 
                    { unconfirmed with
                        ReactionsChecked = 
                            unconfirmed.ReactionsChecked 
                            |> ReactionsChecked.check creature 
                    }
                let fakeReaction = { ReactionNeeded.Creature = unconfirmed.Creature; Alternatives = [] }
                let machine = Machine.ReactionNeeded (fakeReaction, pending)
                ConfirmReaction unconfirmed
                |> execute (globalState, machine)

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

    let goblin : Creature.Statistics = 
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
            Creature.Statistics.HitPoints = 7
            Creature.Movement = 30<ft>
            Creature.ArmorClass = 15
            Creature.WeaponsProficiency = Weapons.Martial
            Creature.Attacks = [ scimitar; shortbow ]
        }

    let creature1 = 
        CreatureID 1, 
        GroupID 1,
        goblin,
        { North = 20; West = 20 } 
        
    let creature2 = 
        CreatureID 2, 
        GroupID 2,
        goblin,
        { North = 25; West = 25 } 

    let map = {
        Width = 40
        Height = 40
        }

    let world =
        (map, [ creature1; creature2 ])
        |> GlobalState.Initialize 


    let machine = 
        { 
            ActionNeeded.Creature = CreatureID 1
            ActionNeeded.Alternatives = Actions.alternatives world |> Option.get
        }
        |> ActionNeeded

    let initialized = 
        (world, machine)