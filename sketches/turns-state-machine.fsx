(*
Design Sketch: handling actions and reactions resolution
*)

type CreatureID = | CreatureID of int
type GroupID = | GroupID of int

type CreatureState = {
    HasTakenReaction: bool
    Dead: bool
    HitPoints: int
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

type Outcome =
    | Move of CreatureID
    | SuccessfulAttack of CreatureID * CreatureID
    | FailedAttack of CreatureID * CreatureID

module Actions = 

    type Action =
        | Move 
        | Attack of CreatureID

    type ActionTaken =
        | FinishTurn 
        | Action of Action

module Reactions = 

    type Reaction =
        | OpportunityAttack of CreatureID
        | Riposte

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
            | SuccessfulAttack _ -> Some(creature, Riposte) 
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
    | CombatFinished 
    | ActionNeeded of ActionNeeded
    | ReactionNeeded of ReactionNeeded * WaitingForConfirmation
    with 
    static member Reacting machine =
        match machine with 
        | CombatFinished -> []
        | ActionNeeded _ -> []
        | ReactionNeeded(reaction, pending) ->
            let rec reacting acc pending = 
                match pending with 
                | WaitingForConfirmation.Action _ -> acc
                | WaitingForConfirmation.Reaction (reaction, rest) -> 
                    let acc = reaction.Creature :: acc
                    reacting acc rest
            reacting [ reaction.Creature ] pending 

type Msg = 
    | RestartCombat
    | CreatureAction of (CreatureID * ActionTaken)
    | CreatureReaction of (CreatureID * ReactionTaken)

let init () =

    let initiative = [ CreatureID 1; CreatureID 2; CreatureID 3 ]
    let state = [
        CreatureID 1, { HitPoints = 7; Dead = false; HasTakenReaction = false }
        CreatureID 2, { HitPoints = 7; Dead = false; HasTakenReaction = false }
        CreatureID 3, { HitPoints = 7; Dead = false; HasTakenReaction = false }
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
            ActionNeeded.Alternatives = [ 
                ActionTaken.Action(Move) 
                ActionTaken.Action(Attack(CreatureID 2))
                FinishTurn 
                ]  
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
    | ReactionTriggered of (CreatureID * Reactions.Reaction)
    | AttemptReaction of (CreatureID * Reactions.Reaction)
    | ConfirmReaction of UnconfirmedReactionResult
    | ReactionCompleted
    | ReactionCancelled
    | ExecuteReaction of (CreatureID * Outcome)
    | PassReaction of CreatureID

let rec execute (globalState: GlobalState, machine: Machine) (transition: Transition) : (GlobalState * Machine) =
    printfn "%A" transition
    match transition with
    | InvalidCommand _ -> globalState, machine
    | StartCombat -> init ()
    | FinishTurn creature ->
        // TODO check if combat is over
        let turn = globalState.TurnState.Value
        let nextCreatureUp = 
            globalState.Initiative 
            |> List.findIndex (fun x -> x = turn.Creature)
            |> fun index -> (index + 1) % (globalState.Initiative.Length)
            |> fun index -> globalState.Initiative.Item index
        // TODO properly implement
        if globalState.CreatureState.[nextCreatureUp].CanAct
        then 
            let turn = {
                Creature = nextCreatureUp
                MovementLeft = 30                
                HasTakenAction = false
                }
                 
            let alternatives = [
                Actions.Action(Actions.Move)
                Actions.FinishTurn
                ]
            { globalState with TurnState = Some turn }, 
            ActionNeeded({ Creature = nextCreatureUp; Alternatives = alternatives })

        else
            FinishTurn nextCreatureUp |> execute (globalState, machine)

    | AttemptAction (creature, action) -> 
        let outcome = 
            match action with
            | Action.Move _ -> Outcome.Move creature
            | Attack target -> 
                // TODO properly handle attack resolution
                Outcome.SuccessfulAttack (creature, target)
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
                Alternatives = [ 
                    Reactions.Reaction(OpportunityAttack(unconfirmed.Creature))
                    Pass 
                    ] 
                    }
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
                    TurnState = Some { globalState.TurnState.Value with HasTakenAction = true }
                }
            | Outcome.SuccessfulAttack (origin, target) -> 
                let targetState = globalState.CreatureState.[target]
                { globalState with
                    TurnState = Some { globalState.TurnState.Value with HasTakenAction = true }
                    CreatureState =
                        globalState.CreatureState
                        |> Map.add target { targetState with HitPoints = targetState.HitPoints - 1 }
                }
        ActionCompleted |> execute (state, machine)   

    | ActionCancelled ->
        let currentTurn = globalState.TurnState.Value
        FinishTurn currentTurn.Creature 
        |> execute (globalState, machine)

    | ActionCompleted ->
        // todo is turn over, is combat over
        // else action needed from current turn
        // TODO determine alternatives based on state
        let currentTurn = globalState.TurnState.Value
        let alternatives = [ 
            if currentTurn.MovementLeft >= 5 then yield Actions.Action(Move)
            if currentTurn.HasTakenAction = false then yield Actions.Action(Attack(CreatureID 2))
            ]
        match alternatives with 
        | [] ->
            FinishTurn currentTurn.Creature 
            |> execute (globalState, machine)
        | alts ->
            // we can still do something
            let machine = 
                ActionNeeded({ Creature = currentTurn.Creature; Alternatives = ActionTaken.FinishTurn :: alts })
            globalState, machine

    | ReactionTriggered(creature, reaction) -> 
        globalState, machine

    | AttemptReaction (creature, reaction) ->
        let outcome = 
            match reaction with
            | Reaction.OpportunityAttack target -> 
                // TODO properly handle attack resolution
                Outcome.SuccessfulAttack (creature, target)
            | Reaction.Riposte -> failwith "TODO implement Riposte"
        let alreadyReacting = machine |> Machine.Reacting
        let notReacting = 
            globalState.Initiative 
            |> List.filter (fun x -> not (alreadyReacting |> List.contains x))            
        let reactions = 
            notReacting
            |> List.filter (fun x -> alreadyReacting |> List.contains x)
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
        failwith "TODO implement confirm reaction"

    | ExecuteReaction (creature, outcome) ->
        
        let state = 
            let creatureState = globalState.CreatureState.[creature]
            match outcome with
            | Outcome.Move creature -> 
                failwith "Cannot move as a reaction"
            | Outcome.FailedAttack (origin, target) -> 
                { globalState with
                    CreatureState = 
                        globalState.CreatureState 
                        |> Map.add creature { creatureState with HasTakenReaction = true }
                }
            | Outcome.SuccessfulAttack (origin, target) -> 
                let targetState = globalState.CreatureState.[target]
                { globalState with
                    TurnState = Some { globalState.TurnState.Value with HasTakenAction = true }
                    CreatureState =
                        globalState.CreatureState
                        |> Map.add creature { creatureState with HasTakenReaction = true }
                        |> Map.add target { targetState with HitPoints = targetState.HitPoints - 1 }
                }

        ReactionCompleted |> execute (state, machine)

    | ReactionCompleted ->
        // check: is the previous level still valid?
        // currently means "is the creature below dead now?"
        match machine with 
        | Machine.CombatFinished -> failwith "Impossible state"
        | Machine.ActionNeeded pending ->  failwith "Impossible state"
        | Machine.ReactionNeeded (reaction, pending) -> 
            match pending with
            | WaitingForConfirmation.Action unconfirmed ->
                if globalState.CreatureState.[unconfirmed.Creature].HitPoints > 0
                then 
                    ConfirmAction unconfirmed 
                    |> execute (globalState, machine)
                else 
                    ActionCancelled 
                    |> execute (globalState, machine)
            | WaitingForConfirmation.Reaction (unconfirmed, pending) -> failwith "TODO"

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
                    | ActionTaken.FinishTurn -> Transition.FinishTurn creature
                    | ActionTaken.Action(action) -> AttemptAction (creature, action)
        | CreatureReaction (creature, reaction) -> 
            match machine with 
            | Machine.CombatFinished -> InvalidCommand "Combat finished / no reaction"
            | Machine.ActionNeeded _ -> InvalidCommand "Expecting an action, not a reaction"
            | Machine.ReactionNeeded (reactionNeeded, pending) ->
                if (not (reactionNeeded.Alternatives |> List.contains reaction))
                then InvalidCommand "Unexpected action"
                else 
                    match reaction with 
                    | Pass -> Transition.PassReaction creature
                    | Reactions.Reaction(reaction) -> AttemptReaction (creature, reaction)

    execute (globalState, machine) internalCommand 

let fake = init ()

let state1 = update fake Msg.RestartCombat

let state2 = update state1 (CreatureAction((CreatureID 1), Actions.Action((Attack(CreatureID 2)))))

let state3 = update state2 (CreatureAction((CreatureID 1), Actions.Action(Action.Move)))

let state4 = update state3 (CreatureReaction((CreatureID 2), Reactions.Reaction(OpportunityAttack (CreatureID 1))))

let state5 = update state4 (CreatureReaction((CreatureID 3), Reactions.Reaction(OpportunityAttack (CreatureID 1))))

let state6 = update state5 (CreatureAction((CreatureID 1), Actions.FinishTurn))
