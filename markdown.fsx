#load "monster.fsx"
open System
open System.Globalization
open Monster

[<RequireQualifiedAccess>]
module Markdown = 

    let signed (value) = 
        if value > 0
        then sprintf "+%i" value
        else sprintf "%i" value

    let textInfo = CultureInfo("en-US",false).TextInfo

    let lower (txt:string) = txt.ToLowerInvariant ()
    let allCaps (txt:string) = txt.ToUpperInvariant ()
    let titleCase (txt:string) = txt |> textInfo.ToTitleCase
    let capitalize (txt:string) =
        if String.IsNullOrWhiteSpace txt
        then txt
        else (txt.[0] |> Char.ToUpperInvariant |> string) + txt.Substring(1)

    let paragraphs (blocks:string seq) = 
        blocks 
        |> String.concat "  \n"

    let commaSeparated (blocks:string seq) = 
        blocks 
        |> String.concat ", "
        
    let parenthesized (txt:string) = sprintf "(%s)" txt

    let alignment (alignment:Alignment) =
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

    let protectiveGear (gear:ProtectiveGear) =
        [
            match gear.Armor with
            | None -> ignore ()
            | Some(armor) ->
                yield
                    match armor with
                    | Padded -> "padded"
                    | Leather -> "leather armor"
                    | StuddedLeather -> "studded leather"
                    | Hide -> "hide armor"
                    | ChainShirt -> "chain shirt"
                    | ScaleMail -> "scale mail"
                    | BreastPlate -> "breast plate"
                    | HalfPlate -> "half plate"
                    | RingMail -> "ring mail"
                    | ChainMail -> "chain mail"
                    | Splint -> "splint"
                    | Plate -> "plate"
            if gear.Shield then yield "shield"    
        ]

    let armorClass (monster:Monster) =
        let ac = Monster.AC monster
        let equipment = 
            match monster.Protection with
            | Natural(bonus) -> 
                if bonus = 0 
                then [ ]
                else [ "natural armor" ]
            | Equipment(gear) -> protectiveGear gear
            |> function
            | [] -> ""
            | items -> 
                items 
                |> commaSeparated
                |> titleCase
                |> parenthesized
        sprintf "**Armor Class** %i %s" ac equipment 

    let abilities (abilities:Ability.Abilities) =
        [
            Ability.abilities |> List.map (sprintf "%A") |> String.concat " | "
            Ability.abilities |> List.map (fun _ -> ":---:") |> String.concat " | "
            Ability.abilities |> List.map (Ability.score abilities >> sprintf "%i") |> String.concat " | "
            Ability.abilities |> List.map (Ability.modifier abilities >> signed) |> String.concat " | "
        ]
        |> paragraphs

    let monsterSheet (monster:Monster) =
        let hitPoints = Monster.HitPoints monster
        [
            sprintf "# %s" monster.Name |> titleCase      
            sprintf "_%A %A, %s_" monster.Size monster.CreatureType (alignment monster.Alignment)
            
            monster |> armorClass
            sprintf "**Hit Points** %i (%s)" (hitPoints |> Roll.Average) (hitPoints |> Roll.Render)
            sprintf "**Speed** %i ft." monster.Speed

            monster.Abilities |> abilities 
        ]
        |> paragraphs
