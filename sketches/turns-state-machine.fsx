(*
Design Sketch: handling actions and reactions resolution
*)

type CreatureID = | CreatureID of int
type GroupID = | GroupID of int

type CombatOutcome = 
    | Draw 
    | Victory of GroupID 

type CombatState = 
    | Ongoing 
    | Finished of CombatOutcome

type CreatureState = {
    HasTakenReaction: bool
    Dead: bool
    HitPoints: int
    Group: GroupID // this should go to Creature Stats
    }
    with 
    member this.CanAct =
        not (this.Dead)
    member this.CanReact =
        this.CanAct && (not this.HasTakenReaction)    

type TurnState = {
    Creature: CreatureID
    MovementLeft: int
    HasTakenAction: bool
    }

type GlobalState = {
    Initiative: list<CreatureID>
    CreatureState: Map<CreatureID, CreatureState>
    TurnState: Option<TurnState>
    }
    with 
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
    | Move of CreatureID
    | SuccessfulAttack of CreatureID * CreatureID * int
    | FailedAttack of CreatureID * CreatureID
    with 
    static member applyEffect (outcome: Outcome) (state: GlobalState) =
        match outcome with
        | Outcome.Move creature -> 
            let currentTurn = state.TurnState.Value
            { state with
                TurnState = 
                    { currentTurn with 
                        MovementLeft = currentTurn.MovementLeft - 5 
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
                TurnState = Some { state.TurnState.Value with HasTakenAction = true } 
            }
        | Outcome.SuccessfulAttack _ -> 
            { state with 
                TurnState = Some { state.TurnState.Value with HasTakenAction = true } 
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
        | Move 
        | Attack of CreatureID

    type ActionTaken =
        | FinishTurn 
        | Action of Action

    let alternatives (state: GlobalState) =
        state.TurnState
        |> Option.bind (fun turn ->
            let creature = turn.Creature
            if state.CreatureState.[creature].CanAct
            then 
                [           
                    if turn.MovementLeft >= 5
                    then yield Action(Move) 
                    if (not turn.HasTakenAction)
                    then                        
                        yield! 
                            state.Initiative
                            |> List.filter (fun target -> target <> creature)
                            |> List.filter (fun target -> not (state.CreatureState.[target].Dead))
                            |> List.map (fun target -> Action(Attack target))
                    yield FinishTurn
                ]
                |> Some
            else None
            )

module Reactions = 

    type Reaction =
        | OpportunityAttack of CreatureID
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
            | Move _ -> Some(creature, [ OpportunityAttack trigger ])
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

type UnconfirmedActionResult = {
    UncheckedReactions: list<CreatureID>
    CheckedReactions: list<CreatureID>
    Creature: CreatureID
    Action: Action
    Outcome: Outcome
    }

type UnconfirmedReactionResult = {
    UncheckedReactions: list<CreatureID>
    CheckedReactions: list<CreatureID>
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

type Msg = 
    | RestartCombat
    | CreatureAction of (CreatureID * ActionTaken)
    | CreatureReaction of (CreatureID * ReactionTaken)

let init () =

    let initiative = [ CreatureID 1; CreatureID 2; CreatureID 3 ]
    let state = [
        CreatureID 1, { HitPoints = 7; Dead = false; HasTakenReaction = false; Group = GroupID 1 }
        CreatureID 2, { HitPoints = 7; Dead = false; HasTakenReaction = false; Group = GroupID 1 }
        CreatureID 3, { HitPoints = 7; Dead = false; HasTakenReaction = false; Group = GroupID 2 }
        ] 

    let turnState = {
        Creature = CreatureID 1
        MovementLeft = 30
        HasTakenAction = false
        }

    let globalState = {
        Initiative = initiative
        TurnState = Some turnState
        CreatureState = state |> Map.ofList
        }

    let machine = 
        { 
            ActionNeeded.Creature = CreatureID 1
            ActionNeeded.Alternatives = Actions.alternatives globalState |> Option.get
        }
        |> ActionNeeded

    (globalState, machine)

type Transition = 
    | InvalidCommand of string
    | StartCombat 
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
    | StartCombat -> init ()
    | FinishTurn creature ->
        match (GlobalState.CombatState globalState) with 
        | Finished result -> globalState, Machine.CombatFinished result
        | Ongoing -> 
            let turn = globalState.TurnState.Value
            let nextCreatureUp = 
                globalState.Initiative 
                |> List.findIndex (fun x -> x = turn.Creature)
                |> fun index -> (index + 1) % (globalState.Initiative.Length)
                |> fun index -> globalState.Initiative.Item index
            let nextTurn = {
                Creature = nextCreatureUp
                MovementLeft = 30                
                HasTakenAction = false
                }        
            let nextCreatureState = 
                { globalState.CreatureState.[nextCreatureUp] with
                    HasTakenReaction = false
                }
            let globalState = 
                { globalState with 
                    TurnState = Some nextTurn 
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
            | Action.Move _ -> Outcome.Move creature
            | Attack target -> 
                // TODO properly handle attack resolution
                Outcome.SuccessfulAttack (creature, target, 1)
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
                UncheckedReactions = globalState.Initiative
                CheckedReactions = []
                Creature = creature
                Action = action
                Outcome = outcome
                }
            ConfirmAction unconfirmed
            |> execute (globalState, machine)

    | ConfirmAction unconfirmed ->
        let reactions = 
            unconfirmed.UncheckedReactions
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
        let currentTurn = globalState.TurnState.Value
        FinishTurn currentTurn.Creature 
        |> execute (globalState, machine)

    | ActionCompleted ->
        match (GlobalState.CombatState globalState) with 
        | Finished result -> globalState, Machine.CombatFinished result
        | Ongoing -> 
            match globalState.TurnState with 
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
            | Reaction.OpportunityAttack target -> 
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
                UncheckedReactions = notReacting
                CheckedReactions = alreadyReacting
                Creature = creature
                Reaction = reaction
                Outcome = outcome
                }
            ConfirmReaction unconfirmed
            |> execute (globalState, machine)

    | ConfirmReaction unconfirmed ->
        let reactions = 
            unconfirmed.UncheckedReactions
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
                    CheckedReactions = creature :: unconfirmed.CheckedReactions
                    UncheckedReactions = unconfirmed.UncheckedReactions |> List.filter (fun x -> x <> creature)
                }
            ConfirmAction unconfirmed
            |> execute (globalState, machine)            
        | WaitingForConfirmation.Reaction (unconfirmed, pending) -> 
            let unconfirmed = 
                { unconfirmed with
                    CheckedReactions = creature :: unconfirmed.CheckedReactions
                    UncheckedReactions = unconfirmed.UncheckedReactions |> List.filter (fun x -> x <> creature)
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
                    CheckedReactions = creature :: unconfirmed.CheckedReactions
                    UncheckedReactions = unconfirmed.UncheckedReactions |> List.filter (fun x -> x <> creature)
                }
            ConfirmAction unconfirmed
            |> execute (globalState, machine)            
        | WaitingForConfirmation.Reaction (unconfirmed, pending) -> 
            let unconfirmed = 
                { unconfirmed with
                    CheckedReactions = creature :: unconfirmed.CheckedReactions
                    UncheckedReactions = unconfirmed.UncheckedReactions |> List.filter (fun x -> x <> creature)
                }
            let fakeReaction = { ReactionNeeded.Creature = unconfirmed.Creature; Alternatives = [] }
            let machine = Machine.ReactionNeeded (fakeReaction, pending)
            ConfirmReaction unconfirmed
            |> execute (globalState, machine)



let update (globalState: GlobalState, machine: Machine) (msg: Msg) = 
    
    let internalCommand = 
        match msg with 
        | RestartCombat -> StartCombat
        | CreatureAction (creature, action) ->           
            match machine with 
            | Machine.CombatFinished _ -> InvalidCommand "Combat finished / no action"
            | Machine.ReactionNeeded _ -> InvalidCommand "Expecting a reaction, not an action"
            | Machine.ActionNeeded actionNeeded ->
                if (not (actionNeeded.Alternatives |> List.contains action))
                then InvalidCommand "Unexpected action"
                else 
                    match action with 
                    | ActionTaken.FinishTurn -> Transition.FinishTurn creature
                    | ActionTaken.Action(action) -> AttemptAction (creature, action)
        | CreatureReaction (creature, reaction) -> 
            match machine with 
            | Machine.CombatFinished _ -> InvalidCommand "Combat finished / no reaction"
            | Machine.ActionNeeded _ -> InvalidCommand "Expecting an action, not a reaction"
            | Machine.ReactionNeeded (reactionNeeded, pending) ->
                if (not (reactionNeeded.Alternatives |> List.contains reaction))
                then InvalidCommand "Unexpected reaction"
                else 
                    match reaction with 
                    | Pass -> Transition.PassReaction creature
                    | Reactions.Reaction(reaction) -> AttemptReaction (creature, reaction)

    execute (globalState, machine) internalCommand 

let scenario1 () = 

    let fake = init ()

    let state1 = update fake Msg.RestartCombat

    let state2 = update state1 (CreatureAction((CreatureID 1), Actions.Action((Attack(CreatureID 2)))))

    let state3 = update state2 (CreatureAction((CreatureID 1), Actions.Action(Action.Move)))

    let state4 = update state3 (CreatureReaction((CreatureID 2), Reactions.Reaction(OpportunityAttack (CreatureID 1))))

    let state5 = update state4 (CreatureReaction((CreatureID 1), Reactions.Reaction(Riposte (CreatureID 2))))

    let state5 = update state4 (CreatureReaction((CreatureID 1), Reactions.Pass))

    let state6 = update state5 (CreatureReaction((CreatureID 3), Reactions.Reaction(OpportunityAttack (CreatureID 1))))

    let state6 = update state5 (CreatureReaction((CreatureID 3), Reactions.Pass))

    let state7 = update state6 (CreatureAction((CreatureID 1), Actions.Action(Action.Move)))

    let state8 = update state7 (CreatureAction((CreatureID 1), Actions.FinishTurn))

let scenario2 () =

    let fake = init ()

    let state1 = update fake Msg.RestartCombat

    let attack state = 
        
        let state = update state (CreatureAction((CreatureID 1), Actions.Action((Attack(CreatureID 3)))))
        let state = update state (CreatureAction((CreatureID 1), Actions.FinishTurn))
        let state = update state (CreatureAction((CreatureID 2), Actions.FinishTurn))
        update state (CreatureAction((CreatureID 3), Actions.FinishTurn))

    let after6Attacks = state1 |> Seq.unfold (fun x -> Some (x, attack x)) |> Seq.item 6

    update after6Attacks (CreatureAction((CreatureID 1), Actions.Action((Attack(CreatureID 3)))))