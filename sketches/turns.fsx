type CreatureID = | CreatureID of int
type GroupID = | GroupID of int

type CreatureState = 
    | Fighting
    | Dying
    | Stabilized 
    | Dead

type CombatOutcome = 
    | WonBy of GroupID
    | Ongoing
    | Draw 

type Action =
    | FinishTurn
    | Attack of CreatureID

type Reaction =
    | OpportunityAttack of CreatureID 
    | DoNothing 

type Decision = 
    | Move
    | ForcedMove
    | Action of Action
    | Reaction of Reaction
    | Bonus of Action

type Command = CreatureID * Decision

type Outcome = 
    | FinishedTurn of CreatureID
    | Moved of CreatureID
    | SuccessfulAttack 
    | FailedAttack 
    | PassedReaction of CreatureID

type Reactions = {
    Trigger: Command * Outcome
    Pending: CreatureID * Reaction 
    }

type PendingReactions = {
    Pending: Reactions list
    Processing: CreatureID * Reaction
    Processed: CreatureID * Reaction
    }

type Result = 
    | Immediate of Outcome
    | Invalid of Command * string
    | Pending of list<Reactions> 

type Turn = {
    CreatureID: CreatureID
    MovementLeft: int
    Pending: Option<list<Reactions>>
    ReactionsProcessed: Set<Command * CreatureID>
    }

type Combat = {
    Initiative: list<CreatureID>
    Round: int
    Turn: Turn
    }

// who makes next decision? What are the choices?
let alternatives (combat: Combat) =
    let turn = combat.Turn
    match turn.Pending with
    // No pending decision: simple decision
    | None ->
        [            
            if turn.MovementLeft > 0
            then yield (turn.CreatureID, Move)

            yield (turn.CreatureID, Action FinishTurn)
        ]
    | Some(pending) -> 
        match pending.Head.Pending with
        | creatureID, OpportunityAttack(targetID) ->
            [
                creatureID, Reaction (DoNothing)
                creatureID, Reaction (OpportunityAttack(targetID))
            ]

let reaction (combat: Combat) (command: Command) =
    let creatureID, decision = command
    match decision with
    | Move ->
        let turn = combat.Turn
        let processed = turn.ReactionsProcessed
        combat.Initiative
        |> List.filter (fun x -> x <> creatureID)
        |> List.filter (fun x -> processed.Contains(command, x) |> not) 
        |> List.map (fun x -> x, OpportunityAttack creatureID)
        |> function
            | [] -> None
            | hd::tl -> hd |> Some
    | _ -> None
    
// could be failure or outcome
let result (combat: Combat) (command: Command) =
    let possible = alternatives combat
    if not (possible |> List.contains command)
    then Invalid (command, "nope")
    else
        let creatureID, decision = command
        match decision with
        | Move ->
                if combat.Turn.MovementLeft <= 0
                then Invalid (command, "No movement left")           
                else 
                    let outcome = Moved creatureID
                    match (reaction combat command) with
                    | None -> Immediate (Moved creatureID)
                    | Some (reaction) -> 
                        {
                            Trigger = command, Moved creatureID
                            Pending = reaction
                        }
                        |> List.singleton
                        |> Pending
        | Action(action) -> 
            match action with
            | FinishTurn -> Immediate (FinishedTurn creatureID)
            | Attack(targetID) -> failwith "TODO attack action"
        | Reaction(reaction) -> 
            match reaction with
            | DoNothing -> Immediate (PassedReaction creatureID)
            | _ -> failwith "TODO reaction"
        | Bonus(action) -> failwith "TODO"
        | ForcedMove -> failwith "TODO"

let state (combat: Combat) (actionResult: Result) =

    match actionResult with
    | Invalid (cmd, error) -> 
        printfn "%s" error
        combat
    | Immediate(outcome) ->
        match outcome with
        | FinishedTurn creatureID -> 
            let nextIndex = 
                combat.Initiative 
                |> List.findIndex (fun x -> x = creatureID)
                |> fun index ->
                    (index + 1) % (combat.Initiative.Length)
            let nextUp = 
                combat.Initiative 
                |> List.item nextIndex
            { combat with
                Turn = { 
                    CreatureID = nextUp
                    MovementLeft = 3 
                    Pending = None
                    ReactionsProcessed = Set.empty
                    }
                Round = 
                    if nextIndex = 0 
                    then combat.Round + 1 
                    else combat.Round
            }
        | Moved creatureID ->
            { combat with
                Turn = 
                    { combat.Turn with
                        MovementLeft = combat.Turn.MovementLeft - 1
                    }
            }
        | PassedReaction creatureID ->
            let pending = combat.Turn.Pending |> Option.get 
            
            { combat with
                Turn = 
                    { combat.Turn with
                        ReactionsProcessed = combat.Turn.ReactionsProcessed.Add creatureID
                    }
            }            
        | FailedAttack -> failwith "TODO"
        | SuccessfulAttack -> failwith "TODO"
    | Pending(reactions) ->
        { combat with
            Turn = 
                { combat.Turn with
                    Pending = Some reactions
                }
        }

// let alternatives (combat: Combat) =


let combat = {
    Initiative = [ CreatureID 1; CreatureID 2; CreatureID 3 ]
    Round = 1
    Turn = {
        CreatureID = CreatureID 1
        MovementLeft = 3
        Pending = None
        ReactionsProcessed = Set.empty
        }
    }

let apply (command: Command) (combat: Combat)  =
    result combat command
    |> state combat 

let stateAfter = 
    combat
    |> apply (CreatureID 1, Action FinishTurn)
    |> apply (CreatureID 2, Action FinishTurn)
    |> apply (CreatureID 3, Action FinishTurn)
    |> apply (CreatureID 1, Action FinishTurn)
    |> apply (CreatureID 2, Move)

stateAfter    
|> apply (CreatureID 1, Reaction DoNothing)
