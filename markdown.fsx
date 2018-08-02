#load "monster.fsx"
open Monster

[<RequireQualifiedAccess>]
module Markdown = 

    let signed (value) = 
        if value > 0
        then sprintf "+%i" value
        else sprintf "%i" value

    let alignment alignment =
        let social,moral = alignment
        let social = 
            match social with
            | Lawful -> "lawful"
            | Social.Neutral -> "neutral"
            | Chaotic -> "chaotic"
        let moral = 
            match moral with
            | Good -> "good"
            | Neutral -> "neutral"
            | Evil -> "evil"
        match alignment with
        | Social.Neutral,Neutral -> "neutral"
        | _ -> sprintf "%s %s" social moral

    let abilities (abilities:Ability.Abilities) =
        [
            Ability.abilities |> List.map (sprintf "%A") |> String.concat " | "
            Ability.abilities |> List.map (fun _ -> ":---:") |> String.concat " | "
            Ability.abilities |> List.map (Ability.score abilities >> sprintf "%i") |> String.concat " | "
            Ability.abilities |> List.map (Ability.modifier abilities >> signed) |> String.concat " | "
        ]
        |> String.concat "  \n"

    let monsterSheet (monster:Monster) =
        let hitPoints = Monster.HitPoints monster
        [
            sprintf "# %s" monster.Name            
            sprintf "_%A %A, %s_" monster.Size monster.CreatureType (alignment monster.Alignment)
                       
            sprintf "**Hit Points** %i (%s)" (hitPoints |> Roll.Average) (hitPoints |> Roll.Render)
            sprintf "**Speed** %i ft." monster.Speed

            monster.Abilities |> abilities 
        ]
        |> String.concat "  \n"
