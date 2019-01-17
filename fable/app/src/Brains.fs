namespace MonsterVault

module AutomatedDecision = 

    open Combat

    type Information = GlobalState

    type DecisionNeeded = 
        | Action of ActionNeeded
        | Reaction of ReactionNeeded

    type DecisionInformation = {
        Info: Information
        DecisionNeeded: DecisionNeeded
        }

    type Strategy =
        abstract TakeAction : 
            ActionNeeded -> Information -> CreatureID * Actions.ActionTaken
        abstract TakeReaction :
            ReactionNeeded -> Information -> CreatureID * Reactions.ReactionTaken

    // Pick a random action from the available alternatives
    module RandomChoice =

        let takeAction (actionNeeded: ActionNeeded) _ =
            let len = 
                actionNeeded.Alternatives 
                |> List.length
            let decision = 
                actionNeeded.Alternatives
                |> List.item (rng.Next len)

            actionNeeded.Creature, decision

        let takeReaction (reactionNeeded: ReactionNeeded) _ =
            let len = 
                reactionNeeded.Alternatives 
                |> List.length
            let decision = 
                reactionNeeded.Alternatives
                |> List.item (rng.Next len)

            reactionNeeded.Creature, decision

        let strategy = 
            { new Strategy with            
                member this.TakeAction info choices = takeAction  info choices
                member this.TakeReaction info choices = takeReaction info choices
            }

    // Basic tactics
    module Naive =

        let takeAction (actionNeeded: ActionNeeded) (info: Information) =
            let creature = actionNeeded.Creature
            let attackerStats = info.Statistics.[creature]
            let attackerState = info.CreatureState.[creature]
            let attacks = attackerStats.AllAttacks ()
            // if no ranged attack available, move towards enemy
            let hasRangedAttacks = 
                attacks
                |> Seq.exists (fun attack -> attack.Type = Attacks.Ranged)
            if (not hasRangedAttacks)
            then 
                let closestLiveEnemy = 
                    info.CreatureState
                    |> Map.filter (fun creatureID state -> 
                        state.Group <> attackerState.Group
                        && (not state.Dead)
                        )
                    |> fun enemies -> 
                        if Map.isEmpty enemies  
                        then None
                        else
                            enemies
                            |> Seq.minBy (fun kv -> distance kv.Value.Position attackerState.Position)
                            |> Some
                match closestLiveEnemy with 
                | None -> RandomChoice.takeAction actionNeeded info
                | Some kv ->
                    if distance kv.Value.Position attackerState.Position <= cellSize
                    then 
                            actionNeeded.Alternatives
                            |> Seq.tryFind (fun alt -> 
                                match alt with 
                                | Actions.ActionTaken.Action(Actions.Attack(target, attack)) -> target = kv.Key
                                | _ -> false
                                )
                            |> fun meleeAttack ->
                                match meleeAttack with
                                | None -> RandomChoice.takeAction actionNeeded info
                                | Some(attack) -> creature, attack
                    else
                    // pick a move that gets us closer
                    let move = 
                        actionNeeded.Alternatives
                        |> List.map (fun alt -> 
                            match alt with 
                            | Actions.ActionTaken.Action(Actions.Move(dir)) -> Some dir
                            | _ -> None)
                        |> List.choose id
                        |> fun moves ->
                            match moves with 
                            | [] -> RandomChoice.takeAction actionNeeded info
                            | dirs ->
                                dirs 
                                |> List.minBy (fun dir -> 
                                    let after = move dir attackerState.Position
                                    distance kv.Value.Position after
                                    )
                                |> fun dir -> creature, Actions.ActionTaken.Action(Actions.Move dir)
                        
                    move
            else
                RandomChoice.takeAction actionNeeded info

        let takeReaction (reactionNeeded: ReactionNeeded) _ =
            let len = 
                reactionNeeded.Alternatives 
                |> List.length
            let decision = 
                reactionNeeded.Alternatives
                |> List.item (rng.Next len)

            reactionNeeded.Creature, decision

        let strategy = 
            { new Strategy with            
                member this.TakeAction info choices = takeAction  info choices
                member this.TakeReaction info choices = takeReaction info choices
            }