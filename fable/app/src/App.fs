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
        World = world
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

    let view (model:Model) dispatch =

        let sendCommand cmd =
            (model.World.Active, cmd)
            |> dispatch

        let journal =
            div []
                [
                    for entry in model.Journal do
                        yield str entry
                        yield br []
                ]

        div []
            [ 
                div [] [ str (string (model.World)) ]

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

                div [] [ str "Journal" ]
                
                journal
            ]

    // App
    Program.mkSimple init update view
    |> Program.withReact "elmish-app"
    |> Program.withConsoleTrace
    |> Program.run
