module Creature = 

    type ID = ID of int

    type State =
        | InCombat 
        | Dead 

type Decision = 
    | Move 
    | Attack 
    | Pass 

type Outcome = 
    | Move
    | SuccessfulAttack 
    | FailedAttack 
    | Pass

module Combat = 

    type CreatureState = {
        CanTakeReaction: bool
        }

    type Combat = {
        Initiative: Creature.ID list
        CreaturesStates: Map<Creature.ID, CreatureState>
        Round: int
        }

type Command = Creature.ID * Decision
type Alternatives = list<Decision>

type ExpectedDecision = {
    Creature: Creature.ID
    Alternatives: Alternatives
    }
    with
    member this.IsValid (command: Command) =
        let creature, decision = command
        this.Creature = creature && 
        this.Alternatives |> List.contains decision

type ExecutedDecision = {
    Creature: Creature.ID
    Decision: Decision
    Outcome: Outcome
    }

let outcome (combat: Combat.Combat) (command: Command) =
    let _, decision = command
    match decision with
    // TODO actually resolve
    | Decision.Move -> Outcome.Move
    | Decision.Attack -> Outcome.SuccessfulAttack
    | Decision.Pass -> Outcome.Pass

let reaction (combat: Combat.Combat) (trigger: ExecutedDecision) (creatureID: Creature.ID) = 
    if trigger.Creature = creatureID
    then None
    elif not combat.CreaturesStates.[creatureID].CanTakeReaction
    then None
    else 
        // primitive implementation of opportunity attack
        match trigger.Outcome with
        | Move -> 
            Some {
                Creature = creatureID
                Alternatives = [ Decision.Attack; Decision.Pass ]
                }
        | _ -> None

type Checks = {
    Unchecked: list<Creature.ID>
    Checked: list<Creature.ID>
    }

type Machine = 
    | AwaitingAction of ExpectedDecision
    | AwaitingReaction of ExpectedDecision * Checks * ExecutedDecision * Machine 

type DecisionResult = 
    | Invalid of string
    | DecisionNeeded of Machine
    | ReactionNeeded of (Creature.ID * Decision * Outcome) * Machine
    | ReactionComplete of ExecutedDecision * Machine
    | Complete of ExecutedDecision

let resolve (combat: Combat.Combat) (machine: Machine) (command: Command) =
    // TODO update also Can Take Action, Reaction?
    let expectedDecision = 
        match machine with
        | AwaitingAction (expected) -> expected
        | AwaitingReaction (expected, _, _, _) -> expected
    match expectedDecision.IsValid command with
    | false -> Invalid "unexpected command"
    | true ->
        // we have a valid command
        let creatureID, decision = command
        match machine with
        | AwaitingAction (_) ->
            let executedDecision = {
                Creature = creatureID
                Decision = decision
                Outcome = outcome combat command
                }
            let possibleReactions = 
                combat.Initiative
                |> List.choose (reaction combat executedDecision)
                    
            match possibleReactions with
            | [] -> 
                executedDecision
                |> DecisionResult.Complete
            | hd :: tl -> 
                let machine = 
                    AwaitingReaction ( 
                        hd,
                        { Unchecked = tl |> List.map (fun x -> x.Creature); Checked = [] },
                        executedDecision,
                        machine
                        )
                DecisionResult.DecisionNeeded machine
        | AwaitingReaction (_, checks, trigger, triggerMachine) -> 
            // TODO need to potentially update the decision / outcome for triggering action,
            // based on what happened in the reaction
            let executedDecision = {
                Creature = creatureID
                Decision = decision
                Outcome = outcome combat command
                }
            // does this trigger another reaction?
            let possibleReactions = 
                combat.Initiative
                |> List.choose (reaction combat executedDecision)

            match possibleReactions with
            | [] -> 
                // no reaction, this is resolved
                // are all reactions checked?
                // next machine: either finished checks, or move to next check
                match checks.Unchecked with
                | [] -> 
                    // completely checked: propagate down
                    (executedDecision, triggerMachine)
                    |> DecisionResult.ReactionComplete
                | unchecked ->
                    let rec findReaction (state: Checks) = 
                        match state.Unchecked with
                        | [] -> state, None
                        | hd::tl -> 
                            let state = { state with Checked = hd :: state.Checked; Unchecked = tl }
                            match reaction combat executedDecision hd with
                            | None -> findReaction state
                            | Some(r) -> state, Some(r)
                    let (checks, re) = findReaction checks 
                    match re with
                    | None -> 
                        (executedDecision, triggerMachine)
                        |> DecisionResult.ReactionComplete                
                    | Some(expectedDecision) -> 
                        AwaitingReaction ( 
                            expectedDecision,
                            checks,
                            trigger,
                            triggerMachine
                            )
                        |> DecisionResult.DecisionNeeded
            | hd :: tl -> 
                let machine = 
                    AwaitingReaction ( 
                        hd,
                        { Unchecked = tl |> List.map (fun x -> x.Creature); Checked = [] },
                        executedDecision,
                        machine
                        )
                DecisionResult.DecisionNeeded machine                            

let resolve2 (combat: Combat.Combat) (machine: Machine) (command: Command) =
    // TODO update also Can Take Action, Reaction?
    let expectedDecision = 
        match machine with
        | AwaitingAction (expected) -> expected
        | AwaitingReaction (expected, _, _, _) -> expected
    match expectedDecision.IsValid command with
    | false -> Invalid "unexpected command"
    | true ->
        // we have a valid command
        let creatureID, decision = command
        match machine with
        | AwaitingAction (_) ->
            let executedDecision = {
                Creature = creatureID
                Decision = decision
                Outcome = outcome combat command
                }
            let possibleReactions = 
                combat.Initiative
                |> List.choose (reaction combat executedDecision)
                    
            match possibleReactions with
            | [] -> 
                executedDecision
                |> DecisionResult.Complete
            | hd :: tl -> 
                let machine = 
                    AwaitingReaction ( 
                        hd,
                        { Unchecked = tl |> List.map (fun x -> x.Creature); Checked = [] },
                        executedDecision,
                        machine
                        )
                DecisionResult.DecisionNeeded machine
        | AwaitingReaction (_, checks, trigger, triggerMachine) -> 
            // TODO need to potentially update the decision / outcome for triggering action,
            // based on what happened in the reaction
            let executedDecision = {
                Creature = creatureID
                Decision = decision
                Outcome = outcome combat command
                }
            // does this trigger another reaction?
            let possibleReactions = 
                combat.Initiative
                |> List.choose (reaction combat executedDecision)

            match possibleReactions with
            | [] -> 
                 

            | hd :: tl -> 
                let machine = 
                    AwaitingReaction ( 
                        hd,
                        { Unchecked = tl |> List.map (fun x -> x.Creature); Checked = [] },
                        executedDecision,
                        machine
                        )
                DecisionResult.DecisionNeeded machine   

// examples

// trivial

module Trivial = 

    let combat : Combat.Combat = {
        Initiative = [ Creature.ID 1 ]
        CreaturesStates = [
            Creature.ID 1, { Combat.CanTakeReaction = true }
            ]
            |> Map.ofList

        Round = 1
        }

    let initial = {
        Creature = Creature.ID 1
        Alternatives = [ Decision.Move; Decision.Pass ]
        }

    let machine1 = AwaitingAction initial

    resolve combat machine1 (Creature.ID 1, Decision.Move) 

// Simple: 2 creatures

module Simple =

    let combat : Combat.Combat = {
        Initiative = [ Creature.ID 1; Creature.ID 2 ]
        CreaturesStates = [
            Creature.ID 1, { Combat.CanTakeReaction = true }
            Creature.ID 2, { Combat.CanTakeReaction = true }
            ]
            |> Map.ofList

        Round = 1
        }

    let initial = {
        Creature = Creature.ID 1
        Alternatives = [ Decision.Move; Decision.Pass ]
        }

    let machine1 = AwaitingAction initial

    let step1 = resolve combat machine1 (Creature.ID 1, Decision.Move)

    let machine2 = 
        match step1 with
        | DecisionNeeded(machine) -> machine

    let step2 = resolve combat machine2 (Creature.ID 2, Decision.Pass)

// 3 creatures
module Complex = 
    let combat : Combat.Combat = {
        Initiative = [ Creature.ID 1; Creature.ID 2; Creature.ID 3 ]
        CreaturesStates = [
            Creature.ID 1, { Combat.CanTakeReaction = true }
            Creature.ID 2, { Combat.CanTakeReaction = true }
            Creature.ID 3, { Combat.CanTakeReaction = true }
            ]
            |> Map.ofList

        Round = 1
        }

    let initial = {
        Creature = Creature.ID 1
        Alternatives = [ Decision.Move; Decision.Pass ]
        }

    let machine1 = AwaitingAction initial

    let step1 = resolve combat machine1 (Creature.ID 1, Decision.Move)

    let machine2 = 
        match step1 with
        | DecisionNeeded (machine) -> machine

    let step2 = resolve combat machine2 (Creature.ID 2, Decision.Pass)
