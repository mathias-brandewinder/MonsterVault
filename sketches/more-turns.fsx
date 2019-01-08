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

let outcome (combat: Combat.Combat) (command: Command) =
    let _, decision = command
    match decision with
    // TODO actually resolve
    | Decision.Move -> Outcome.Move
    | Decision.Attack -> Outcome.SuccessfulAttack
    | Decision.Pass -> Outcome.Pass

let reaction (combat: Combat.Combat) (creatureID: Creature.ID) = 
    if combat.CreaturesStates.[creatureID].CanTakeReaction
    then Some [ Decision.Pass ]
    else None

type ReactionChecks = {
    Unchecked: list<Creature.ID>
    Checked: list<Creature.ID>
    DecisionState: DecisionState
    }

and DecisionState = 
    | AwaitingDecision of Creature.ID * Alternatives 
    | AwaitingReactions of Creature.ID * Decision * Outcome * ReactionChecks

type CommandResult = 
    | Invalid of string
    | DecisionNeeded of DecisionState
    | Complete of (Creature.ID * Decision * Outcome)

module StateMaching = 

    type Checks = {
        Unchecked: list<Creature.ID>
        Checked: list<Creature.ID>
        }

    type Machine = 
        | AwaitingDecision of Creature.ID * Alternatives
        // | AwaitingReactions of Checks * Outcome * Machine
        | AwaitingReactions of Creature.ID * Alternatives * Checks * Outcome * Machine 

    type Foo = 
        | Invalid of string
        | DecisionNeeded of Machine
        | PartiallyDecided of (Creature.ID * Decision * Outcome) * Machine
        | Complete of (Creature.ID * Decision * Outcome)

    let foo (combat: Combat.Combat) (machine: Machine) (command: Command) =
        let creatureID, decision = command
        match machine with
        | AwaitingDecision(creature, alternatives) ->
            if creature = creatureID && alternatives |> List.contains decision
            then 
                let result = outcome combat command
                let possibleReactions = 
                    combat.Initiative
                    |> List.filter (fun x -> x <> creatureID)
                    |> List.filter (fun x -> combat.CreaturesStates.[x].CanTakeReaction)
                    |> List.filter (fun x -> reaction combat x |> Option.isSome)
                match possibleReactions with
                | [] -> Foo.Complete (creatureID, decision, result)
                | hd :: _ -> 
                    let r = reaction combat hd |> Option.get
                    let machine = 
                        AwaitingReactions ( 
                            hd, r,
                            { Unchecked = possibleReactions; Checked = [] },
                            result,
                            machine
                            )
                    Foo.DecisionNeeded machine
            else 
                Foo.Invalid "Not the expected creature or decision"
        | AwaitingReactions (creature, alternatives, checks, parentOutcome, machine) -> 
            if creature = creatureID && alternatives |> List.contains decision
            then 
                let decisionOutcome = outcome combat command
                // next machine: either finished checks, or move to next check
                let updatedMachine = 
                    match checks.Unchecked with
                    | [] -> machine
                    | hd :: unchecked ->
                        let r = reaction combat hd |> Option.get
                        AwaitingReactions ( 
                            hd, r,
                            { Unchecked = unchecked; Checked = hd :: checks.Checked },
                            parentOutcome,
                            machine
                            )
                // TODO need to potentially update the decision / outcome for triggering action,
                // based on what happened in the reaction
                PartiallyDecided ((creature, decision, decisionOutcome), updatedMachine) 
            else 
                Foo.Invalid "Not the expected creature or decision"
        




// DecisionState -> Command -> DecisionState + List of events?
let rec resolve (combat: Combat.Combat) (state: DecisionState) (command: Command) =
    let creatureID, decision = command
    match state with
    | AwaitingDecision (creature, alternatives) ->
        if creature = creatureID && alternatives |> List.contains decision
        then 
            let result = outcome combat command
            let possibleReactions = 
                combat.Initiative
                |> List.filter (fun x -> x <> creatureID)
                |> List.filter (fun x -> combat.CreaturesStates.[x].CanTakeReaction)
                |> List.filter (fun x -> reaction combat x |> Option.isSome)
            match possibleReactions with
            | [] -> CommandResult.Complete (creatureID, decision, result)
            | hd :: _ -> 
                let awaiting = hd, reaction combat hd |> Option.get
                let decisionState = 
                    AwaitingReactions(
                        creatureID, 
                        decision, 
                        result, 
                        { Unchecked = possibleReactions; Checked = []; DecisionState = AwaitingDecision awaiting })
                CommandResult.DecisionNeeded decisionState
        else CommandResult.Invalid "Not the creature turn"
    | AwaitingReactions(creature, decision, outcome, checks) ->
        match checks.DecisionState with
        | AwaitingReactions(_, _, _, checks) -> resolve combat checks.DecisionState command
        | AwaitingDecision(_) -> 
            let resolution = resolve combat checks.DecisionState command
            // rebuild the decision state now


type PendingDecision = {
    Creature: Creature.ID
    Alternatives: Alternatives
    Decision: Decision
    Outcome: Outcome
    UncheckedReactions: list<Creature.ID>
    ProcessedReactions: list<Creature.ID>
    Pending: Option<PendingDecision>
    }

let test = { 
    Creature = Creature.ID 1
    Alternatives = [ Decision.Move ]
    Decision = Decision.Move
    Outcome = Outcome.Move
    ProcessedReactions = [ Creature.ID 2 ]
    Pending = Some {
        Creature = Creature.ID 3
        Alternatives = [ Decision.Move ]
        Decision = Decision.Move
        Outcome = Outcome.Move
        ProcessedReactions = [ Creature.ID 4 ]
        Pending = None
        }
    }



let currentLevel (pending: PendingDecision) =
    let rec relevant (current: PendingDecision) = 
        match current.Pending with
        | None -> current
        | Some(current) -> current
    relevant pending

let reaction (pending: PendingDecision) (creature: Creature.ID) =
    let state = currentLevel pending
    if state.Creature = creature
    then None
    elif state.ProcessedReactions |> List.contains creature
    then None
    else Some (creature, [ Decision.Pass ]) // TODO: determine actual reactions if any

let handleDecision (pending: PendingDecision) (alternatives: Alternatives, command: Command) =
    42

// application level

type Model = 
    | Ongoing of Combat.Combat
    | Finished 


type Msg =
    | Start

let init () : Model = 
    Ongoing {
        Round = 0
        }

let update (model: Model) (msg: Msg) =
    match model with
    | Finished -> 
        match msg with 
        | Start -> init ()
    | Ongoing(combat) ->
        match msg with 
        | Start -> init ()