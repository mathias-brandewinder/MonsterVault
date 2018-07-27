#load "monster.fsx"
open Monster

[<RequireQualifiedAccess>]
module Markdown = 

    let signed (value) = 
        if value > 0
        then sprintf "+%i" value
        else sprintf "%i" value

    let abilities (abilities:Ability.Abilities) =
        [
            Ability.abilities |> List.map (sprintf "%A") |> String.concat " | "
            Ability.abilities |> List.map (fun _ -> ":---:") |> String.concat " | "
            Ability.abilities |> List.map (Ability.score abilities >> sprintf "%i") |> String.concat " | "
            Ability.abilities |> List.map (Ability.modifier abilities >> signed) |> String.concat " | "
        ]
        |> String.concat "  \n"
