namespace MonsterVault

module App =

    open Elmish
    open Elmish.React
    open Fable.Helpers.React
    open Fable.Helpers.React.Props
    open MonsterVault.Combat
    open MonsterVault.Combat.Actions
    open MonsterVault.Combat.Reactions
    open MonsterVault.Data
    open MonsterVault.AutomatedDecision

    // MODEL

    module Simulation =

        type DecisionMode = 
            | Automated 
            | Manual

        type Settings = 
            | SelectMode of DecisionMode
            | RestartCombat

    module Creature = 

        type Decision = 
            | Action of (CreatureID * ActionTaken)
            | Reaction of (CreatureID * ReactionTaken)

    type Msg = 
        | Decision of Creature.Decision
        | Simulation of Simulation.Settings

    type Model = {
        GlobalState: GlobalState
        Machine: Machine
        Journal: list<Transition>
        Mode: Simulation.DecisionMode
        }
    
    let agent (strategy: AutomatedDecision.Strategy) =    
        MailboxProcessor<DecisionInformation * Dispatch<Msg>>.Start(
            fun inbox ->
                let rec loop () = 
                    async {
                        let! (info, dispatch) = inbox.Receive ()
                        do! Async.Sleep 100          

                        match info.DecisionNeeded with
                        | AutomatedDecision.Action action ->
                            strategy.TakeAction action info.Info
                            |> Creature.Action
                            |> Decision
                            |> dispatch
                        | AutomatedDecision.Reaction reaction ->
                            RandomChoice.takeReaction reaction info.Info
                            |> Creature.Reaction
                            |> Decision
                            |> dispatch
            
                        return! loop ()
                    }
                loop ()
            )

    let decide = 
        let decisionAgent = agent Naive.strategy
        fun information ->
            fun dispatch -> 
                decisionAgent.Post (information, dispatch)

    let init () = 
        let state, machine = initialState ()
        {
            GlobalState = state
            Machine = machine
            Journal = []
            Mode = Simulation.Manual
        },
        Cmd.ofMsg (Msg.Simulation(Simulation.RestartCombat))

    // UPDATE
    let update (msg: Msg) (model: Model) =

        match msg with
        | Simulation info ->
            match info with
            | Simulation.SelectMode mode -> { model with Mode = mode }, Cmd.none
            | Simulation.RestartCombat -> 
                let state, cmd = init ()
                { state with Mode = model.Mode }, Cmd.none

        | Decision decision ->
            let globalState, machine = model.GlobalState, model.Machine
            let internalCommand = 
                match decision with 
                | Creature.Action (creature, action) ->           
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
                | Creature.Reaction (creature, reaction) -> 
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
            
            let updated = 
                execute (globalState, machine, model.Journal) internalCommand 
                |> fun (state, machine, journal) -> 
                    { GlobalState = state; Machine = machine; Journal = journal; Mode = model.Mode }
            
            match updated.Mode with
            | Simulation.Manual -> updated, Cmd.none
            | Simulation.Automated ->
                match updated.Machine with 
                | Machine.ActionNeeded action ->
                    let info = { 
                        Info = updated.GlobalState
                        DecisionNeeded = AutomatedDecision.Action action              
                        }
                    updated, Cmd.ofSub (decide info)
                | Machine.ReactionNeeded (reaction, _) ->
                    let info = { 
                        Info = updated.GlobalState
                        DecisionNeeded = AutomatedDecision.Reaction reaction              
                        }
                    updated, Cmd.ofSub (decide info)
                | _ ->
                    updated, Cmd.none

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

    let tileHighlight (model: Model) (x, y) color =
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
            SVGAttr.StrokeWidth 2
            SVGAttr.Stroke color 
            SVGAttr.Fill "none"
          ] [ ]

    let tileMarked (model: Model) (x, y) color =
        let map = model.GlobalState.BattleMap
        let width = map.Width
        let height = map.Height
        svg []
            [
            line 
                [ 
                    SVGAttr.X1 (tileSize * (width - x - 1))
                    SVGAttr.Y1 (tileSize * (height - y - 1))
                    SVGAttr.X2 (tileSize * (width - x) - 2)
                    SVGAttr.Y2 (tileSize * (height - y) - 2)
                    SVGAttr.StrokeWidth 2
                    SVGAttr.Stroke color
                ] 
                []
            line 
                [ 
                    SVGAttr.X1 (tileSize * (width - x - 1))
                    SVGAttr.Y2 (tileSize * (height - y - 1))
                    SVGAttr.X2 (tileSize * (width - x) - 2)
                    SVGAttr.Y1 (tileSize * (height - y) - 2)
                    SVGAttr.StrokeWidth 2
                    SVGAttr.Stroke color
                ] 
                []                
            ]

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
                            if state.Group = GroupID 1 then "Orange"
                            else "RoyalBlue"
                            
                    yield tileAt model (state.Position.West, state.Position.North) color
                    
                    if creature.Key = model.GlobalState.Turn.Value.Creature
                    then 
                        yield tileHighlight model (state.Position.West, state.Position.North) "Red"

                    if model.GlobalState.CreatureState.[creature.Key].Dead
                    then 
                        yield tileMarked model (state.Position.West, state.Position.North) "Black"
            ]

    let panelStyle = Style [ Padding 5; MarginBottom 5; MarginLeft 5; BorderRadius 5; BackgroundColor "LightGray" ]
    let vignetteStyle = Style [ Padding 2 ]

    let state (model: Model) dispatch =
        let stats = model.GlobalState.Statistics
        let states = model.GlobalState.CreatureState
        let descriptionOf creatureID = 
            [
                yield str (stats.[creatureID].Description)
                yield br []
                yield str (sprintf "Hit points: %i/%i" states.[creatureID].HitPoints stats.[creatureID].HitPoints)
                yield br []
                if states.[creatureID].Dead then yield (str "Dead")
                if states.[creatureID].HasTakenReaction then yield (str "Reaction taken")         
            ]
            
        div [ panelStyle ] 
            [ 
                for creatureID in model.GlobalState.Initiative do
                    yield
                        div [ vignetteStyle ] (descriptionOf creatureID)
            ]

    let message (msg: Msg) =
        match msg with
        | Decision decision ->
            match decision with
            | Creature.Action (creature, action) -> action |> string
            | Creature.Reaction (creature, reaction) -> reaction |> string
        | Msg.Simulation(info) -> 
            match info with
            | Simulation.SelectMode mode -> mode |> string
            | Simulation.RestartCombat -> "Restart" 

    let commands (model: Model) dispatch =

        let machine = model.Machine
        let messages = 
            match machine with 
            | Machine.ActionNeeded (actionNeeded) -> 
                actionNeeded.Alternatives
                |> List.map (fun action -> actionNeeded.Creature, action)
                |> List.map Creature.Action
                |> List.map Msg.Decision
            | Machine.ReactionNeeded (reactionNeeded, _) -> 
                reactionNeeded.Alternatives
                |> List.map (fun action -> reactionNeeded.Creature, action)
                |> List.map Creature.Reaction
                |> List.map Msg.Decision
            | Machine.CombatFinished _ -> 
                [   
                    Msg.Simulation(Simulation.RestartCombat) 
                    Msg.Simulation(Simulation.SelectMode Simulation.Manual)
                    Msg.Simulation(Simulation.SelectMode Simulation.Automated)
                ]

        div [ panelStyle ]
            [
                yield (str (model.GlobalState.Turn.Value.Creature |> string))
                yield div [] [ str "Alternatives" ]
                for msg in messages -> 
                    button [ OnClick (fun _ -> dispatch msg) ] [ str (message msg) ] 
            ]
    
    let journal (model: Model) dispatch =
        div [ panelStyle ]
            [
                for entry in model.Journal do
                    yield (str (string entry))
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
                    ]

                div [ Style [ Float "left"; Width 200 ] ]
                    [
                        journal model dispatch
                    ]                    
            ]

    // App
    Program.mkProgram init update view
    |> Program.withReact "elmish-app"
    |> Program.withConsoleTrace
    |> Program.run
