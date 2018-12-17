namespace MonsterVault
open Fable.Import.React

module App =

    open Elmish
    open Elmish.React
    open Fable.Helpers.React
    open Fable.Helpers.React.Props
    open MonsterVault.Domain

    // MODEL

    type Model = {
        World: World
        Journal: string list
        }

    type Msg = CreatureID * Command

    let init () = {
        World = TestSample.world
        Journal = []
        }

    // UPDATE

    let update (msg:Msg) (model:Model) =
        match Rules.validate (model.World) msg with
        | Error(error) -> 
            { model with 
                Journal = 
                    error :: model.Journal 
                    |> List.truncate 5
            }
        | Ok (creatureID, command) -> 
            let world = update model.World (creatureID, command)
            { model with
                World = world
                Journal = 
                    (sprintf "%A: %A" creatureID command) :: model.Journal
                    |> List.truncate 5
            }

    // VIEW (rendered with React)

    let tileSize = 15

    let tileAt (model:Model) (x,y) color =
        let map = model.World.BattleMap
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

    let battleMap (model:Model) dispatch =

        let map = model.World.BattleMap
        let width = map.Width
        let height = map.Height

        svg [                    
                SVGAttr.Width (width * tileSize)
                SVGAttr.Height (height * tileSize)
            ]
            [
                let map = model.World.BattleMap
                for x in 0 .. (map.Width - 1) do
                    for y in 0 .. (map.Height - 1) do
                        yield tileAt model (x,y) "LightGray"

                for creature in model.World.Creatures do
                    let state = creature.Value
                    let color = 
                        if creature.Key = model.World.Active
                        then "Red"
                        else "Orange"
                    yield tileAt model (state.Position.West, state.Position.North) color
            ]

    let panelStyle = Style [ Padding 5; MarginBottom 5; MarginLeft 5; BorderRadius 5; BackgroundColor "LightGray" ]

    let state (model:Model) dispatch =
        div [ panelStyle ] [ str (string (model.World.Creatures)) ]

    let commands (model:Model) dispatch =

        let cmds = alternatives model.World

        div [ panelStyle ]
            [
                yield div [] [ str "Alternatives" ]
                for cmd in cmds -> 
                    button [ OnClick (fun _ -> dispatch cmd) ] [ str (string cmd) ] 
            ]
    
    let journal (model:Model) dispatch =
        div [ panelStyle ]
            [
                for entry in model.Journal do
                    yield str entry
                    yield br []
            ]

    let view (model:Model) dispatch =

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
