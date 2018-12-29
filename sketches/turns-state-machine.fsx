(*
Design Sketch: handling actions and reactions resolution
*)

type CreatureID = | CreatureID of int
type GroupID = | GroupID of int

type Action =
    | Move 
    | Attack of CreatureID
    | FinishTurn

type Reaction =
    | Pass 
    | OpportunityAttack of CreatureID
    | Riposte

type Outcome =
    | Move of CreatureID
    | SuccessfulAttack of CreatureID * CreatureID
    | FailedAttack of CreatureID * CreatureID

type ActionNeeded = {
    Creature: CreatureID
    Alternatives: list<Action>
    }

type ReactionNeeded = {
    Creature: CreatureID
    Alternatives: list<Reaction>
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


type CreatureState = {
    ReactionTaken: bool
    Dead: bool
    HitPoints: int
    }
    with 
    member this.CanAct =
        not (this.Dead)
    member this.CanReact =
        this.CanAct && (not this.ReactionTaken)       

type TurnState = {
    Creature: CreatureID
    MovementLeft: int
    ActionTaken: bool
    }

type GlobalState = {
    Initiative: list<CreatureID>
    CreatureState: Map<CreatureID, CreatureState>
    TurnState: Option<TurnState>
    }
    
module Reactions = 

    let toAction (globalState: GlobalState) (trigger: CreatureID, outcome: Outcome) (creature: CreatureID) =
        if creature = trigger
        then None
        elif (not globalState.CreatureState.[creature].CanReact)
        then None
        else
            match outcome with
            | Move _ -> Some(creature, OpportunityAttack trigger)
            | SuccessfulAttack _ -> None 
            | FailedAttack _ -> None

    let toReaction (globalState: GlobalState) (trigger: CreatureID, outcome: Outcome) (creature: CreatureID) =
        if creature = trigger
        then None
        elif (globalState.CreatureState.[creature].CanReact)
        then None
        else
            match outcome with
            | Move _ -> None
            | SuccessfulAttack _ -> Some(Riposte) 
            | FailedAttack _ -> None


type WaitingForConfirmation = 
    | Action of UnconfirmedActionResult
    | Reaction of UnconfirmedReactionResult * WaitingForConfirmation

type Machine = 
    | CombatFinished 
    | ActionNeeded of ActionNeeded
    | ReactionNeeded of ReactionNeeded * WaitingForConfirmation

type Msg = 
    | RestartCombat
    | CreatureAction of (CreatureID * Action)
    | CreatureReaction of (CreatureID * Reaction)

let init () =

    let initiative = [ CreatureID 1; CreatureID 2 ]
    let state = [
        CreatureID 1, { HitPoints = 7; Dead = false; ReactionTaken = false }
        CreatureID 2, { HitPoints = 7; Dead = false; ReactionTaken = false }
        ] 
    let turnState = {
        Creature = CreatureID 1
        MovementLeft = 30
        ActionTaken = false
        }

    let globalState = {
        Initiative = initiative
        TurnState = Some turnState
        CreatureState = state |> Map.ofList
        }

    let machine = 
        { 
            ActionNeeded.Creature = CreatureID 1
            ActionNeeded.Alternatives = [ Action.Move; Attack(CreatureID 2); FinishTurn ]  
        }
        |> ActionNeeded

    (globalState, machine)

type Transition = 
    // TODO: add CancelledCommand, CancelledReaction
    | InvalidCommand of string
    | StartCombat 
    | FinishTurn of CreatureID
    | AttemptAction of (CreatureID * Action)
    | ConfirmAction of UnconfirmedActionResult
    | ExecuteAction of (CreatureID * Outcome)
    | ActionCompleted
    | ReactionTriggered of (CreatureID * Reaction)
    | AttemptReaction of (CreatureID * Reaction)
    | PassReaction of CreatureID

let rec execute (globalState: GlobalState, machine: Machine) (transition: Transition) : (GlobalState * Machine) =
    printfn "%A" transition
    match transition with
    | InvalidCommand _ -> globalState, machine
    | StartCombat -> init ()
    | FinishTurn _ -> failwith "TODO: turn finish"
    | AttemptAction (creature, action) -> 
        let outcome = 
            match action with
            | Action.Move _ -> Outcome.Move creature
            | Attack target -> 
                // TODO properly handle attack resolution
                Outcome.SuccessfulAttack (creature, target)
            | _ -> failwith "Unsupported action"
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
        | (triggered, reaction) :: tl ->
            let reactionNeeded = {
                ReactionNeeded.Creature = triggered
                Alternatives = [ reaction; Reaction.Pass ] }
            let machine = 
                Machine.ReactionNeeded(
                    reactionNeeded, 
                    WaitingForConfirmation.Action(unconfirmed)
                    )
            ReactionTriggered(triggered, reaction)
            |> execute (globalState, machine)  

    | ExecuteAction (creature, outcome) ->
        let state = 
            match outcome with
            | Outcome.Move creature -> 
                { globalState with
                    TurnState = Some { globalState.TurnState.Value with MovementLeft = globalState.TurnState.Value.MovementLeft - 5 }
                }
            | Outcome.FailedAttack (origin, target) -> 
                { globalState with
                    TurnState = Some { globalState.TurnState.Value with ActionTaken = true }
                }
            | Outcome.SuccessfulAttack (origin, target) -> 
                let targetState = globalState.CreatureState.[target]
                { globalState with
                    TurnState = Some { globalState.TurnState.Value with ActionTaken = true }
                    CreatureState =
                        globalState.CreatureState
                        |> Map.add target { targetState with HitPoints = targetState.HitPoints - 1 }
                }
        ActionCompleted |> execute (state, machine)    
    | ActionCompleted ->
        // todo is turn over, is combat over
        // else action needed from current turn
        // TODO determine alternatives based on state
        let currentTurn = globalState.TurnState.Value
        let alternatives = [ 
            if currentTurn.MovementLeft >= 5 then yield Action.Move
            if currentTurn.ActionTaken = false then yield Action.Attack(CreatureID 2) 
            ]
        match alternatives with 
        | [] ->
            FinishTurn currentTurn.Creature |> execute (globalState, machine)
        | alts ->
            // we can still do something
            let machine = 
                ActionNeeded({ Creature = currentTurn.Creature; Alternatives = Action.FinishTurn :: alts })
            globalState, machine
    | ReactionTriggered(creature, reaction) -> 
        globalState, machine
    | AttemptReaction (creature, reaction) ->
        failwith "TODO implement attempt reaction"
    | PassReaction creature ->
        let pending = 
            match machine with
            | CombatFinished -> failwith "Impossible state"
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
            // TODO attempt action again, with new updated context
            //failwith "TODO handle confirmed action / passed reaction"
        | WaitingForConfirmation.Reaction (_, unconfirmed) -> 
            failwith "TODO handle confirmed action / passed reaction"
    // failwith "TODO"

let update (globalState: GlobalState, machine: Machine) (msg: Msg) = 
    
    let internalCommand = 
        match msg with 
        | RestartCombat -> StartCombat
        | CreatureAction (creature, action) ->           
            match machine with 
            | Machine.CombatFinished -> InvalidCommand "Combat finished / no action"
            | Machine.ReactionNeeded _ -> InvalidCommand "Expecting a reaction, not an action"
            | Machine.ActionNeeded actionNeeded ->
                if (not (actionNeeded.Alternatives |> List.contains action))
                then InvalidCommand "Unexpected action"
                else 
                    match action with 
                    | Action.FinishTurn -> Transition.FinishTurn creature
                    | _ -> AttemptAction (creature, action)
        | CreatureReaction (creature, reaction) -> 
            match machine with 
            | Machine.CombatFinished -> InvalidCommand "Combat finished / no reaction"
            | Machine.ActionNeeded _ -> InvalidCommand "Expecting an action, not a reaction"
            | Machine.ReactionNeeded (reactionNeeded, pending) ->
                if (not (reactionNeeded.Alternatives |> List.contains reaction))
                then InvalidCommand "Unexpected action"
                else 
                    match reaction with 
                    | Reaction.Pass -> Transition.PassReaction creature
                    | _ -> AttemptReaction (creature, reaction)

    execute (globalState, machine) internalCommand 

let fake = init ()

let state1 = update fake Msg.RestartCombat

let state2 = update state1 (CreatureAction((CreatureID 1), Action.Attack(CreatureID 2)))

let state3 = update state2 (CreatureAction((CreatureID 1), Action.Move))

let state4 = update state3 (CreatureReaction((CreatureID 2), Reaction.Pass))
