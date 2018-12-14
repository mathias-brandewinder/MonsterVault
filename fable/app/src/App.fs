namespace MonsterVault

module App =

    open Elmish
    open Elmish.React
    open Fable.Helpers.React
    open Fable.Helpers.React.Props
    open MonsterVault.Domain

    // MODEL

    type Model = World

    type Msg = CreatureID * Command

    let init () = world

    // UPDATE

    let update (msg:Msg) (model:Model) =
        apply msg model

    // VIEW (rendered with React)

    let view (model:Model) dispatch =

        let sendCommand cmd =
            (model.Active, cmd)
            |> dispatch

        div []
            [ 
                div [] [ str (string model) ]

                div [] [ str "Movement" ]

                button [ OnClick (fun _ -> sendCommand (Move N)) ] [ str "N" ]
                button [ OnClick (fun _ -> sendCommand (Move NW)) ] [ str "NW" ]
                button [ OnClick (fun _ -> sendCommand (Move W)) ] [ str "W" ]
                button [ OnClick (fun _ -> sendCommand (Move SW)) ] [ str "SW" ]
                button [ OnClick (fun _ -> sendCommand (Move S)) ] [ str "S" ]
                button [ OnClick (fun _ -> sendCommand (Move SE)) ] [ str "SE" ]
                button [ OnClick (fun _ -> sendCommand (Move E)) ] [ str "E" ]
                button [ OnClick (fun _ -> sendCommand (Move NE)) ] [ str "NE" ]

                div [] [ str "Actions" ]

                button [ OnClick (fun _ -> sendCommand (Action Dash)) ] [ str "Dash" ]

                div [] [ str "Other" ]

                button [ OnClick (fun _ -> sendCommand (Done)) ] [ str "Done" ]
            ]

    // App
    Program.mkSimple init update view
    |> Program.withReact "elmish-app"
    |> Program.withConsoleTrace
    |> Program.run
