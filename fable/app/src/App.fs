namespace MonsterVault

module App =

    open Elmish
    open Elmish.React
    open Fable.Helpers.React
    open Fable.Helpers.React.Props
    open MonsterVault.Domain
    open MonsterVault.Domain.Actions
    open MonsterVault.Domain.Reactions

    // MODEL

    type Model = {
        GlobalState: GlobalState
        Machine: Machine
        Journal: string list
        }

    type Msg = 
        | RestartCombat
        | CreatureAction of (CreatureID * ActionTaken)
        | CreatureReaction of (CreatureID * ReactionTaken)

    let init () = 
        let combat, machine = TestSample.initialized
        {
            GlobalState = combat
            Machine = machine
            Journal = []
        }

    // UPDATE
    let update (msg: Msg) (model: Model) =

        let globalState, machine = model.GlobalState, model.Machine

        let internalCommand = 
            match msg with 
            | RestartCombat -> StartCombat (init >> fun x -> x.GlobalState, x.Machine)
            | CreatureAction (creature, action) ->           
                match machine with 
                | Machine.CombatFinished _ -> InvalidCommand "Combat finished / no action"
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
                | Machine.CombatFinished _ -> InvalidCommand "Combat finished / no reaction"
                | Machine.ActionNeeded _ -> InvalidCommand "Expecting an action, not a reaction"
                | Machine.ReactionNeeded (reactionNeeded, pending) ->
                    if (not (reactionNeeded.Alternatives |> List.contains reaction))
                    then InvalidCommand "Unexpected reaction"
                    else 
                        match reaction with 
                        | Pass -> Transition.PassReaction creature
                        | Reactions.Reaction(reaction) -> AttemptReaction (creature, reaction)

        execute (globalState, machine) internalCommand |> fun (state, machine) -> { GlobalState = state; Machine = machine; Journal = [] }

    // VIEW (rendered with React)

    let tileSize = 15

    let tileAt (model: Model) (x, y) color =
        let map = model.GlobalState.BattleMap
        let width = map.Width
        let height = map.Height
        rect [ 
            SVGAttr.X (tileSize * (width - x - 1))
            SVGAttr.Y (tileSize * (height - y - 1))
            SVGAttr.Width (tileSize - 2)
            SVGAttr.Height (tileSize - 2)
            SVGAttr.Rx 2
            SVGAttr.Ry 2
            SVGAttr.Fill color 
          ] [ ]

    let battleMap (model: Model) dispatch =

        let map = model.GlobalState.BattleMap
        let width = map.Width
        let height = map.Height

        svg [                    
                SVGAttr.Width (width * tileSize)
                SVGAttr.Height (height * tileSize)
            ]
            [
                let map = model.GlobalState.BattleMap
                for x in 0 .. (map.Width - 1) do
                    for y in 0 .. (map.Height - 1) do
                        yield tileAt model (x,y) "LightGray"

                for creature in model.GlobalState.CreatureState do
                    let state = creature.Value
                    let color = 
                        if creature.Key = model.GlobalState.Turn.Value.Creature
                        then "Red"
                        else "Orange"
                    yield tileAt model (state.Position.West, state.Position.North) color
            ]

    let panelStyle = Style [ Padding 5; MarginBottom 5; MarginLeft 5; BorderRadius 5; BackgroundColor "LightGray" ]

    let state (model: Model) dispatch =
        div [ panelStyle ] [ str (string (model.GlobalState.CreatureState)) ]

    let message (msg: Msg) =
        match msg with
        | Msg.CreatureAction (creature, action) -> action |> string
        | Msg.CreatureReaction (creature, reaction) -> reaction |> string
        | Msg.RestartCombat -> "Restart" 

    let commands (model: Model) dispatch =

        let machine = model.Machine
        let messages = 
            match machine with 
            | Machine.ActionNeeded (actionNeeded) -> 
                actionNeeded.Alternatives
                |> List.map (fun action -> actionNeeded.Creature, action)
                |> List.map (Msg.CreatureAction)
            | Machine.ReactionNeeded (reactionNeeded, _) -> 
                reactionNeeded.Alternatives
                |> List.map (fun action -> reactionNeeded.Creature, action)
                |> List.map (Msg.CreatureReaction)
            | Machine.CombatFinished _ -> [ Msg.RestartCombat ]

        div [ panelStyle ]
            [
                yield div [] [ str "Alternatives" ]
                for msg in messages -> 
                    button [ OnClick (fun _ -> dispatch msg) ] [ str (message msg) ] 
            ]
    
    let journal (model: Model) dispatch =
        div [ panelStyle ]
            [
                for entry in model.Journal do
                    yield str entry
                    yield br []
            ]

    let view (model: Model) dispatch =

        div []
            [ 
                div [ Style [ Float "left" ]]
                    [ battleMap model dispatch ]

                div [ Style [ Float "left"; Width 200 ] ]
                    [
                        state model dispatch
                        commands model dispatch
                        journal model dispatch
                    ]
            ]

    // App
    Program.mkSimple init update view
    |> Program.withReact "elmish-app"
    |> Program.withConsoleTrace
    |> Program.run
