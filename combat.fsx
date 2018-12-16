(*
Work in progress: modeling combat.

Notes / TODOs
- out-of-turn activities, ex: opportunity attack
- crossing creature space and size 
*)

#load "./fable/app/src/Domain.fs"
open MonsterVault.Domain

let apply (creatureID, command) world =
    match Rules.validate world (creatureID, command) with
    | Error(msg) ->
        printfn "%s" msg
        world  
    | Ok(creatureID, command) -> 
        update world (creatureID, command)

TestSample.world
|> apply (CreatureID 1, Move N)
|> apply (CreatureID 1, Move N)
|> apply (CreatureID 1, Move N)
|> apply (CreatureID 1, Move N)
|> apply (CreatureID 1, Move SE) 
|> apply (CreatureID 1, Move SE) 
|> apply (CreatureID 1, Move SE) 
|> apply (CreatureID 1, Done) 
|> apply (CreatureID 2, Move SE) 
|> apply (CreatureID 2, Done)
|> apply (CreatureID 1, Move N) 
|> apply (CreatureID 1, Move N) 
|> apply (CreatureID 1, Move N) 
|> apply (CreatureID 1, Move N) 
|> apply (CreatureID 1, Move N) 
|> apply (CreatureID 1, Move N) 
|> apply (CreatureID 1, Action Dash) 
|> apply (CreatureID 1, Move N) 
