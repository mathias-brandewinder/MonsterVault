(*
Explore modeling & simulating actions
*)

(*
TODO
- How to handle no creature in Initiative?
x Creature can always finish turn (Done)
x When Done, next creature in Initiative is Up
x When Initiative List is finished, Next Turn starts
*)

type CreatureID = | CreatureID of int

type Command = 
    | Done

type State = {
    Turn: int
    CreatureUp: CreatureID
    Initiative: CreatureID list
    } 

let initialize (initiative: CreatureID list) = 
    {
        Turn = 1
        Initiative = initiative
        CreatureUp = initiative |> List.head
    }

let handle state (id,cmd) =    
    match id = state.CreatureUp with 
    | false -> failwith "invalid command"
    | true ->
        match cmd with
        | Done ->
            match state.Initiative with
            | [] -> 
                failwith "Impossible: no creature."
            | initiative ->
                let currentIndex = 
                    initiative
                    |> List.tryFindIndex (fun c -> id = c)
                let nextIndex, nextTurn = 
                    match currentIndex with
                    | None -> failwith "Impossible: cannot find creature in initiative."
                    | Some(index) ->
                        let nextIndex = index + 1
                        if nextIndex = (initiative |> List.length)
                        then 0, state.Turn + 1
                        else nextIndex, state.Turn
                let nextUp = initiative |> List.item nextIndex
                { state with 
                    Turn = nextTurn
                    CreatureUp = nextUp
                }

// Trying things out
let c1 = CreatureID 1
let c2 = CreatureID 2
let initialState = initialize [ c1; c2 ]

let state1 = handle initialState (c1,Done)
let state2 = handle state1 (c2,Done)