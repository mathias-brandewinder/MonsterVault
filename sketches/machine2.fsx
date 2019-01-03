module Creature = 
    type ID = | ID of int

type Decision = 
    | Pass
    | Move 
    | Attack

type Alternatives = {
    Creature: Creature.ID
    Alternatives: Decision list
    }

type Outcome = 
    | Move
    | Attack

type Resolution = {
    Creature: Creature.ID
    Decision: Decision
    Outcome: Outcome
    }

// state changes: either apply outcome automatically, or ask for decision

type Unchecked = Creature.ID list

type Unconfirmed = {
    Resolution: Resolution
    Unchecked: Creature.ID list
    }

type WaitingForConfirmation = 
    | Action of Unconfirmed
    | Reaction of Unconfirmed * WaitingForConfirmation

type Machine = 
    | CombatFinished 
    | ActionNeeded of Alternatives
    | ReactionNeeded of Alternatives * WaitingForConfirmation

type Command = Creature.ID * Decision

type CombatEvent = 
    | Error of string 
    | Executed of Resolution

type State = {
    Initiative: list<Creature.ID>
    ReactionTaken: list<Creature.ID>
    Machine: Machine
    Log: list<CombatEvent>
    }

type Message = 
    | Start 
    | Decision of Command

let init () : State = {
    Initiative = [ Creature.ID 1; Creature.ID 2 ]
    ReactionTaken = [ ]
    Machine = ActionNeeded ( { Creature = Creature.ID 1; Alternatives = [ Decision.Move; Decision.Pass ] }) // TODO not right
    Log = []
    }

let reactionsInChain (machine: Machine) = 
    
    let rec buildReactions acc (pending: WaitingForConfirmation) = 
        match pending with
        | WaitingForConfirmation.Action _ -> acc
        | WaitingForConfirmation.Reaction(unconfirmed, rest) -> 
            buildReactions (unconfirmed.Resolution.Creature :: acc) rest
    
    match machine with
    | Machine.CombatFinished -> []
    | Machine.ActionNeeded _ -> []
    | Machine.ReactionNeeded (alts, pending) ->  buildReactions [ alts.Creature] pending

let nextTurn (state: State) : State =
    match state.Machine with
    | CombatFinished -> failwith "Combat is finished, cannot finish turn"
    | ReactionNeeded _ -> failwith "Waiting for reaction, cannot finish turn"
    | ActionNeeded(alternatives) ->
        // is combat over

        // next creature that has an Action
        failwith "TODO: implement next turn"

let actionOutcome (state: State) (creature: Creature.ID, decision: Decision) =
    match decision with 
    | Decision.Move -> Outcome.Move
    | _ -> failwith "TODO action outcomes implement"

let reactionOutcome (state: State) (creature: Creature.ID, decision: Decision) =
    match decision with 
    | Decision.Attack -> Outcome.Attack
    | _ -> failwith "TODO action outcomes implement"

let actionTriggersReaction (state: State) (unchecked: list<Creature.ID>) (executed: Resolution) = 
    match executed.Outcome with 
    | Move -> 
        // TODO implement proper opportunity attack
        // need notion of enemies
        unchecked
        // candidates
        |> List.filter (fun creature -> 
            // cannot react against oneself
            creature <> executed.Creature &&
            // can react only if reaction not taken yet
            not (state.ReactionTaken |> List.contains creature)
            )
        |> function
            | [] -> None
            | hd :: tl -> 
                { 
                    Creature = hd
                    Alternatives = [ Decision.Attack; Decision.Pass ]
                }
                |> Some 
    | _ -> None

let reactionTriggersReaction (state: State) (unchecked: list<Creature.ID>) (executed: Resolution) = 
    match executed.Outcome with 
    | Outcome.Attack -> 
        // TODO implement correctly:
        // goal now is to check reaction-to-reaction
        // example: Parry, ...
        // need notion of enemies
        unchecked
        // candidates
        |> List.filter (fun creature -> 
            // cannot react against oneself
            creature <> executed.Creature &&
            // can react only if reaction not taken yet
            not (state.ReactionTaken |> List.contains creature)
            )
        |> function
            | [] -> None
            | hd :: _ -> 
                { 
                    Creature = hd
                    Alternatives = [ Decision.Attack; Decision.Pass ]
                }
                |> Some 
    | _ -> None

let executeAction (state: State) (resolution: Resolution) =
    // TODO implement properly
    { state with
        Machine = 
            ActionNeeded({ 
                Creature = resolution.Creature
                Alternatives = [ Decision.Pass; Decision.Move ]
                }
                )
        Log = Executed resolution :: state.Log
    }

let update (state: State) (msg: Message) =
    match state.Machine with
    | CombatFinished -> 
        match msg with 
        | Start -> init ()
        | _ -> { state with Log = Error "Invalid" :: state.Log }
    | ActionNeeded (alternatives) ->
        match msg with
        | Start -> init ()
        | Decision(creature, decision) ->
            if alternatives.Creature <> creature || not (alternatives.Alternatives |> List.contains decision) 
            then { state with Log = Error "Invalid Action" :: state.Log }
            else 
                // check: pass, or act?
                // if pass, move to next turn
                match decision with
                | Pass -> nextTurn state
                | _ ->
                    // check: does this trigger a reaction?
                    // if yes, move to ReactionNeeded
                    let tentativeOutcome = actionOutcome state (creature, decision)
                    let tentativeResolution = {
                        Creature = creature
                        Decision = decision
                        Outcome = tentativeOutcome
                        }
                    let unchecked = state.Initiative
                    match (actionTriggersReaction state unchecked tentativeResolution) with
                    | Some (reaction) ->
                        { state with
                            Machine = 
                                ReactionNeeded(
                                    reaction, 
                                    Action (
                                        { 
                                            Resolution = tentativeResolution
                                            Unchecked = unchecked
                                        }
                                        )
                                    )
                        }
                    | None ->
                        executeAction state tentativeResolution
    | ReactionNeeded (alternatives, pending) ->
        match msg with
        | Start -> init ()
        | Decision (creature, decision) ->
            if alternatives.Creature <> creature || not (alternatives.Alternatives |> List.contains decision) 
            then { state with Log = Error "Invalid Reaction" :: state.Log }
            else
            // pass: mark as checked, check next unchecked
                match decision with
                | Pass -> 
                    match pending with 
                    | Action (unconfirmed) -> 
                        // remove the creature from the unchecked reactions list
                        let unchecked = 
                            unconfirmed.Unchecked
                            |> List.filter (fun x -> x <> creature)
                        // no need to update unconfirmed resolution,
                        // because the reaction was "pass"
                        let updatedResolution = unconfirmed.Resolution
                        match unchecked with
                        | [] -> 
                            executeAction state updatedResolution
                        | remainingUnchecked ->
                            match (actionTriggersReaction state remainingUnchecked updatedResolution) with
                            | Some (reaction) ->
                                { state with
                                    Machine = 
                                        ReactionNeeded(
                                            reaction, 
                                            Action (
                                                { 
                                                    Resolution = updatedResolution
                                                    Unchecked = remainingUnchecked
                                                }
                                                )
                                            )
                                }
                            | None -> executeAction state updatedResolution

                    // decision was to pass on taking a reaction,
                    // the next reaction is not invalidated
                    // if it is confirmed, we need to recursively dequeue
                    // otherwise we need to check the next possible
                    // reaction it triggers
                    | Reaction(unconfirmed, pending) -> 
                        // does this trigger another reaction?
                        // remove the creature from the unchecked reactions list
                        let unconfirmed = 
                            { unconfirmed with 
                                Unchecked = 
                                    unconfirmed.Unchecked
                                    |> List.filter (fun x -> x <> creature) 
                            }

                        match unconfirmed.Unchecked with
                        | [] -> 
                            failwith "TODO: propagate down the confirmed reaction"
                        | _ ->
                            failwith "TODO this is wrong"
                            match (actionTriggersReaction state unconfirmed.Unchecked unconfirmed.Resolution) with
                            | Some (reaction) ->
                                { state with
                                    Machine = 
                                        ReactionNeeded(
                                            reaction, 
                                            Action (unconfirmed)                                                
                                            )
                                }
                            | None -> 
                                failwith "TODO: propagate down the confirmed reaction"                        

                // otherwise: reaction was not pass, but proper reaction
                // updated reaction taken, propagate down, ...
                | _ ->  
                    // check: does this trigger a reaction?
                    // if yes, move to ReactionNeeded
                    let tentativeOutcome = reactionOutcome state (creature, decision)
                    let tentativeResolution = {
                        Creature = creature
                        Decision = decision
                        Outcome = tentativeOutcome
                        }

                    let inChain = reactionsInChain state.Machine |> Set.ofList
                    let unchecked = state.Initiative |> List.filter (fun x -> not (inChain.Contains x))

                    match (reactionTriggersReaction state unchecked tentativeResolution) with 
                    | None -> 
                        // reaction does not trigger another reaction:
                        // we can push down the tentativeResolution as confirmed



                        // TODO no reaction to reaction, propagate down"
                        // need some recursion here: push down until either a reaction is needed
                        // or we are fully resolved
                        state

                    | Some(reaction) ->
                        { state with
                            Machine = 
                                ReactionNeeded(
                                    reaction, 
                                    Reaction(
                                        { 
                                            Resolution = tentativeResolution
                                            Unchecked = unchecked
                                        },
                                        pending
                                    ))                                   
                        }                    

let state0 = init ()
let state1 = update state0 (Decision (Creature.ID 1, Decision.Move))
let state2 = update state1 (Decision (Creature.ID 2, Decision.Attack))

let state3 = update state2 (Decision (Creature.ID 1, Decision.Attack))




