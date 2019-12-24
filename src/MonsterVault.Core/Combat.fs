namespace MonsterVault
open System

module Combat =

    open Attacks

    type CombatOutcome =
        | Draw
        | Victory of GroupID

    type CombatState =
        | Ongoing
        | Finished of CombatOutcome

    type Turn = {
        Creature: CreatureID
        MovementLeft: int<ft>
        HasTakenAction: bool
        Disengaging: bool
        }


    type BattleMap = {
        Width: int
        Height: int
        }

    type GlobalState = {
        BattleMap: BattleMap
        Initiative: CreatureID list
        Turn: Option<Turn>
        CreatureState: Map<CreatureID, Creature.State>
        Statistics: Map<CreatureID, Creature.Statistics>
        }
        with
        static member Initialize (map: BattleMap, creatures: (GroupID * Creature.Statistics * Position) list) =
            let initiative =
                creatures
                |> List.mapi (fun i _ -> CreatureID i)
            let turn =
                match creatures with
                | [] -> None
                | (_, stats, _) :: _ ->
                    {
                        Creature = initiative |> List.head
                        MovementLeft = stats.Movement
                        HasTakenAction = false
                        Disengaging = false
                    }
                    |> Some

            {
                BattleMap = map
                Initiative = initiative
                Turn = turn
                CreatureState =
                    creatures
                    |> List.mapi (fun index (group, stats, pos) ->
                        CreatureID index,
                        Creature.initialize (stats, group, pos)
                        )
                    |> Map.ofList
                Statistics =
                    creatures
                    |> List.mapi (fun index (_, stats, _) ->
                        CreatureID index,
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
                    |> Map.filter (fun _ value -> value.CanAct)
                    |> Seq.map (fun kv -> kv.Value.Group)
                    |> Seq.distinct
                    |> Seq.toList
                match activeGroups with
                | [] -> Finished Draw
                | [ winner ] -> Finished (Victory winner)
                | _ -> Ongoing

    type Outcome =
        | Move of CreatureID * Direction * int<ft>
        | SuccessfulAttack of CreatureID * CreatureID * int
        | FailedAttack of CreatureID * CreatureID
        | Dash of CreatureID * int<ft>
        | Dodge of CreatureID
        | Disengage of CreatureID
        with
        // apply the outcome to the state of the world
        static member applyEffect (outcome: Outcome) (state: GlobalState) =
            match outcome with
            | Outcome.Move (creature, direction, cost) ->
                let currentTurn = state.Turn.Value
                let currentState = state.CreatureState.[creature]
                let updatedState =
                    { currentState with
                        Position = currentState.Position |> move direction
                    }
                { state with
                    CreatureState =
                        state.CreatureState
                        |> Map.add creature updatedState
                    Turn =
                        { currentTurn with
                            MovementLeft = currentTurn.MovementLeft - cost
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
            | Outcome.Dash (_, distance) ->
                let currentTurn = state.Turn.Value
                { state with
                    Turn =
                        { currentTurn with
                            MovementLeft = currentTurn.MovementLeft + distance
                        }
                        |> Some
                }
            | Outcome.Dodge creature ->
                let creatureState = state.CreatureState.[creature]
                let updatedState =
                    { creatureState with
                        Dodging = true
                    }
                { state with
                    CreatureState =
                        state.CreatureState
                        |> Map.add creature updatedState
                }
            | Outcome.Disengage creature ->
                let currentTurn = state.Turn.Value
                { state with
                    Turn =
                        { currentTurn with
                            Disengaging = true
                        }
                        |> Some
                }
        // update the turn, to keep track of
        // whether or not actions are still possible
        static member updateAction (outcome: Outcome) (state: GlobalState) =
            match outcome with
            | Outcome.Move _ -> state
            | Outcome.FailedAttack _
            | Outcome.SuccessfulAttack _
            | Outcome.Dash _
            | Outcome.Dodge _
            | Outcome.Disengage _ ->
                { state with
                    Turn = Some { state.Turn.Value with HasTakenAction = true }
                }
        // update the creature state, to keep track of
        // whether or not reactions are still possible
        static member updateReaction (outcome: Outcome) (state: GlobalState) =
            match outcome with
            | Outcome.Move _ -> failwith "Error: move is not a possible reaction"
            | Outcome.FailedAttack (source, _) ->
                let attackerState = state.CreatureState.[source]
                { state with
                    CreatureState =
                        state.CreatureState
                        |> Map.add source { attackerState with HasTakenReaction = true }
                }
            | Outcome.SuccessfulAttack (source, _, _) ->
                let attackerState = state.CreatureState.[source]
                { state with
                    CreatureState =
                        state.CreatureState
                        |> Map.add source { attackerState with HasTakenReaction = true }
                }
            | Outcome.Dash _ -> failwith "Error: dash is not a possible reaction"
            | Outcome.Dodge _ -> failwith "Error: dodge is not a possible reaction"
            | Outcome.Disengage _ -> failwith "Error: disengage is not a possible reaction"

    module Actions =

        type Action =
            | Move of Direction
            | Attack of (CreatureID * Attack)
            | Dash
            | Dodge
            | Disengage

        type ActionTaken =
            | FinishTurn
            | Action of Action

        let movementCost (state: GlobalState) (pos: Position) (dir: Direction) =
            // TODO add difficult terrain, etc...
            cellSize

        module Rules =

            type Rule = GlobalState -> (CreatureID * Action) -> bool

            let ``A creature must be active to act`` : Rule =
                fun state ->
                    fun (creatureID, _) ->
                        match state.Turn with
                        | None -> false
                        | Some turn -> turn.Creature = creatureID

            let ``A creature cannot move if it has not enough movement left`` : Rule =
                fun state ->
                    fun (creatureID, action) ->
                        match action with
                        | Move direction ->
                            match state.Turn with
                            | None -> false
                            | Some(turn) ->
                                let creatureState = state.CreatureState.[creatureID]
                                movementCost state creatureState.Position direction <= turn.MovementLeft
                        | _ -> true

            let ``A creature cannot move to a space occupied by another creature`` : Rule =
                fun state ->
                    fun (creatureID, action) ->
                        match action with
                        | Move direction ->
                            let creatureState = state.CreatureState.[creatureID]
                            let destination =
                                creatureState.Position
                                |> move direction
                            state.CreatureState
                            |> Map.exists (fun ID state -> ID <> creatureID && state.Position = destination)
                            |> not
                        | _ -> true

            let ``A creature can take at most one action per turn`` : Rule =
                fun state ->
                    fun (_, action) ->
                        match action with
                        | Move _ -> true
                        | _ ->
                            match state.Turn with
                            | None -> false
                            | Some turn ->
                                not turn.HasTakenAction

            let ``A creature can only attack within attack range`` : Rule =
                fun state ->
                    fun (creatureID, action) ->
                        match action with
                        | Attack (targetID, attack) ->
                            let attackerState = state.CreatureState.[creatureID]
                            let targetState = state.CreatureState.[targetID]
                            let dist = distance attackerState.Position targetState.Position
                            let maximumDistance =
                                match attack.Type with
                                | Attacks.Melee (reach) -> reach
                                | Attacks.Ranged (range) -> range.Long
                            dist <= maximumDistance
                        | _ -> true

            let rules = [
                ``A creature must be active to act``
                ``A creature cannot move if it has not enough movement left``
                ``A creature cannot move to a space occupied by another creature``
                ``A creature can take at most one action per turn``
                ``A creature can only attack within attack range``
                ]

            let validateAgainst state =
                fun x -> (true, rules) ||> List.fold (fun flag rule -> rule state x && flag)

        let alternatives (state: GlobalState) =
            state.Turn
            |> Option.bind (fun turn ->
                let creature = turn.Creature
                if state.CreatureState.[creature].CanAct
                then
                    [
                        // every possible movement
                        yield!
                            directions
                            |> List.map Move

                        yield Dash
                        yield Dodge
                        yield Disengage
                        // every possible attack
                        let attackerStats = state.Statistics.[creature]
                        let attacks = attackerStats.AllAttacks ()
                        yield!
                            state.Initiative
                            |> List.filter (fun target -> target <> creature)
                            |> List.filter (fun target -> state.CreatureState.[target].Group <> state.CreatureState.[creature].Group)
                            |> List.filter (fun target -> not (state.CreatureState.[target].Dead))
                            |> List.collect (fun target ->
                                attacks
                                |> List.map (fun attack -> (Attack (target, attack))))
                    ]
                    |> List.filter (fun action -> Rules.validateAgainst state (creature, action))
                    |> List.map Action
                    |> fun actions -> FinishTurn :: actions
                    |> Some
                else None
                )

    module Reactions =

        type Reaction =
            | OpportunityAttack of (CreatureID * Attack)
            | Riposte of (CreatureID * Attack)

        type ReactionTaken =
            | Pass
            | Reaction of Reaction

        let opportunityAttacks (globalState: GlobalState) (trigger: CreatureID, outcome: Outcome) (creature: CreatureID) =
            if creature = trigger
            then []
            elif (not globalState.CreatureState.[creature].CanReact)
            then []
            elif globalState.CreatureState.[trigger].Group = globalState.CreatureState.[creature].Group
            then []
            else
                match outcome with
                | Move (_, dir, _) ->
                    // is the creature Disengaging
                    let disengaging =
                        match globalState.Turn with
                        | None -> false // we should not even have to check
                        | Some turn -> turn.Disengaging
                    if disengaging
                    then []
                    else
                        // do we have melee attacks that we can use within range,
                        // and are we in range before, out of range after
                        let distanceBefore =
                            distance
                                globalState.CreatureState.[trigger].Position
                                globalState.CreatureState.[creature].Position
                        let distanceAfter =
                            distance
                                (globalState.CreatureState.[trigger].Position |> move dir)
                                globalState.CreatureState.[creature].Position

                        let attackerStats = globalState.Statistics.[creature]
                        attackerStats.AllAttacks ()
                        |> List.filter (fun attack ->
                            match attack.Type with
                            | Attacks.Melee reach -> distanceBefore <= reach && distanceAfter > reach
                            | Attacks.Ranged _ -> false)
                        |> List.map (fun attack -> OpportunityAttack(trigger, attack))
                | SuccessfulAttack _ -> []
                | FailedAttack _ -> []
                | Dash _ -> []
                | Dodge _ -> []
                | Disengage _ -> []

        let riposte (globalState: GlobalState) (trigger: CreatureID, outcome: Outcome) (creature: CreatureID) =
            if creature = trigger
            then []
            elif (not globalState.CreatureState.[creature].CanReact)
            then []
            else
                match outcome with
                | Move _ -> []
                | SuccessfulAttack (origin, target, _) ->
                    if creature <> target
                    then []
                    elif globalState.CreatureState.[target].Group = globalState.CreatureState.[creature].Group
                    then []
                    else
                        let dist =
                            distance
                                globalState.CreatureState.[trigger].Position
                                globalState.CreatureState.[creature].Position

                        let attackerStats = globalState.Statistics.[creature]
                        attackerStats.AllAttacks ()
                        |> List.filter (fun attack ->
                            match attack.Type with
                            | Attacks.Melee reach -> dist <= reach
                            | Attacks.Ranged _ -> false)
                        |> List.map (fun attack -> Riposte (origin, attack))
                | FailedAttack _ -> []
                | Dash _ -> []
                | Dodge _ -> []
                | Disengage _ -> []

        let alternatives (globalState: GlobalState) (trigger: CreatureID, outcome: Outcome) (creature: CreatureID) =
            [
                opportunityAttacks globalState (trigger, outcome) creature
                riposte globalState (trigger, outcome) creature
            ]
            |> List.collect id
            |> function
                | [] -> None
                | reactions -> Some (creature, reactions)

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
        // identify creatures that are already involved
        // in the chain of reactions.
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
            | ReactionNeeded (reaction, pending) ->
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

    let rec execute (globalState: GlobalState, machine: Machine, journal: list<Transition>) (transition: Transition) : (GlobalState * Machine * list<Transition>) =
        printfn "%A" transition
        let journal = transition :: journal
        match transition with
        | InvalidCommand _ -> globalState, machine, journal
        | StartCombat init ->
            let state, machine = init ()
            state, machine, []
        | FinishTurn creature ->
            match (GlobalState.CombatState globalState) with
            | Finished result -> globalState, Machine.CombatFinished result, journal
            | Ongoing ->
                let turn = globalState.Turn.Value
                let nextCreatureUp =
                    globalState.Initiative
                    |> List.findIndex (fun x -> x = turn.Creature)
                    |> fun index -> (index + 1) % (globalState.Initiative.Length)
                    |> fun index -> globalState.Initiative.Item index

                let nextTurn = {
                    Creature = nextCreatureUp
                    MovementLeft = globalState.Statistics.[nextCreatureUp].Movement
                    HasTakenAction = false
                    Disengaging = false
                    }
                let nextCreatureState =
                    { globalState.CreatureState.[nextCreatureUp] with
                        HasTakenReaction = false
                        Dodging = false
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
                | None -> FinishTurn nextCreatureUp |> execute (globalState, machine, journal)
                | Some alternatives ->
                    globalState,
                    ActionNeeded({ Creature = nextCreatureUp; Alternatives = alternatives }),
                    journal

        | AttemptAction (creature, action) ->
            let outcome =
                match action with
                | Action.Move dir ->
                    let position = globalState.CreatureState.[creature].Position
                    let cost = movementCost globalState position dir
                    Outcome.Move (creature, dir, cost)
                | Attack (target, attack) ->
                    let ac = globalState.Statistics.[target].ArmorClass
                    let dodging = globalState.CreatureState.[target].Dodging
                    let longRange =
                        match attack.Type with
                        | AttackType.Ranged range ->
                            let posAttacker = globalState.CreatureState.[creature].Position
                            let posTarget = globalState.CreatureState.[target].Position
                            let dist = distance posAttacker posTarget
                            dist > range.Short
                        | AttackType.Melee _ -> false
                    match Attacks.damage (dodging || longRange) ac attack with
                    | None -> Outcome.FailedAttack (creature, target)
                    | Some damage -> Outcome.SuccessfulAttack (creature, target, damage)
                | Action.Dash ->
                    let movement = globalState.Statistics.[creature].Movement
                    Outcome.Dash (creature, movement)
                | Action.Dodge ->
                    Outcome.Dodge creature
                | Action.Disengage ->
                    Outcome.Disengage creature
            let reactions =
                globalState.Initiative
                |> List.choose (
                    Reactions.alternatives globalState (creature, outcome))
            match reactions with
            | [] ->
                ExecuteAction (creature, outcome)
                |> execute (globalState, machine, journal)
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
                |> execute (globalState, machine, journal)

        | ConfirmAction unconfirmed ->
            let reactions =
                unconfirmed.ReactionsChecked.Unchecked
                |> List.choose (
                    Reactions.alternatives globalState (unconfirmed.Creature, unconfirmed.Outcome))
            match reactions with
            | [] ->
                ExecuteAction (unconfirmed.Creature, unconfirmed.Outcome)
                |> execute (globalState, machine, journal)
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
                |> execute (globalState, machine, journal)

        | ExecuteAction (creature, outcome) ->
            let state =
                globalState
                |> Outcome.updateAction outcome
                |> Outcome.applyEffect outcome
            ActionCompleted |> execute (state, machine, journal)

        | ActionCancelled ->
            let currentTurn = globalState.Turn.Value
            FinishTurn currentTurn.Creature
            |> execute (globalState, machine, journal)

        | ActionCompleted ->
            match (GlobalState.CombatState globalState) with
            | Finished result -> globalState, Machine.CombatFinished result, journal
            | Ongoing ->
                match globalState.Turn with
                | None -> failwith "Impossible: when an action completes, there must be a turn"
                | Some turn ->
                    match Actions.alternatives globalState with
                    | None ->
                        FinishTurn turn.Creature
                        |> execute (globalState, machine, journal)
                    | Some alternatives ->
                        let machine =
                            ActionNeeded({ Creature = turn.Creature; Alternatives = alternatives })
                        globalState, machine, journal

        | ReactionTriggered (creature, reactionNeeded) ->
            globalState, machine, journal

        | AttemptReaction (creature, reaction) ->
            let outcome =
                match reaction with
                | Reaction.OpportunityAttack (target, attackStatistics) ->
                    let ac = globalState.Statistics.[target].ArmorClass
                    let dodging = globalState.CreatureState.[target].Dodging
                    match Attacks.damage dodging ac attackStatistics with
                    | None -> Outcome.FailedAttack (creature, target)
                    | Some damage -> Outcome.SuccessfulAttack (creature, target, damage)
                | Reaction.Riposte (target, attackStatistics) ->
                    let ac = globalState.Statistics.[target].ArmorClass
                    let dodging = globalState.CreatureState.[target].Dodging
                    match Attacks.damage dodging ac attackStatistics with
                    | None -> Outcome.FailedAttack (creature, target)
                    | Some damage -> Outcome.SuccessfulAttack (creature, target, damage)
            let alreadyReacting = machine |> Machine.Reacting
            let notReacting =
                globalState.Initiative
                |> List.filter (fun x -> not (alreadyReacting |> List.contains x))
            let reactions =
                notReacting
                |> List.choose (
                    Reactions.alternatives globalState (creature, outcome))

            match reactions with
            | [] ->
                ExecuteReaction (creature, outcome)
                |> execute (globalState, machine, journal)
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
                |> execute (globalState, machine, journal)

        | ConfirmReaction unconfirmed ->
            let reactions =
                unconfirmed.ReactionsChecked.Unchecked
                |> List.choose (
                    Reactions.alternatives globalState (unconfirmed.Creature, unconfirmed.Outcome))
            match reactions with
            | [] ->
                ExecuteReaction (unconfirmed.Creature, unconfirmed.Outcome)
                |> execute (globalState, machine, journal)
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
                |> execute (globalState, machine, journal)

        | ExecuteReaction (creature, outcome) ->
            let state =
                globalState
                |> Outcome.updateReaction outcome
                |> Outcome.applyEffect outcome

            ReactionCompleted |> execute (state, machine, journal)

        | ReactionCompleted ->
            match (GlobalState.CombatState globalState) with
            | Finished result -> globalState, Machine.CombatFinished result, journal
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
                            |> execute (globalState, machine, journal)
                        else
                            ActionCancelled
                            |> execute (globalState, machine, journal)
                    | WaitingForConfirmation.Reaction (unconfirmed, pending) ->
                        let fakeReaction =  { ReactionNeeded.Creature = unconfirmed.Creature; Alternatives = [] }
                        let machine = Machine.ReactionNeeded (fakeReaction, pending)
                        // TODO check if other conditions cancel the original reaction
                        if globalState.CreatureState.[unconfirmed.Creature].HitPoints > 0
                        then
                            ConfirmReaction unconfirmed
                            |> execute (globalState, machine, journal)
                        else
                            ReactionCancelled unconfirmed.Creature
                            |> execute (globalState, machine, journal)

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
                |> execute (globalState, machine, journal)
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
                |> execute (globalState, machine, journal)

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
                |> execute (globalState, machine, journal)
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
                |> execute (globalState, machine, journal)
