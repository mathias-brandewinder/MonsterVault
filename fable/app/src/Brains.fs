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