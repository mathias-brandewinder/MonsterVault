type Ability = 
    | STR
    | DEX
    | CON 
    | INT 
    | WIS
    | CHA

module Ability = 

    type Scores = {
        STR: int
        DEX: int
        CON: int
        INT: int
        WIS: int
        CHA: int
        }

    let scoreToModifier score = 
        (score / 2) - 5
        |> min 10
        |> max -5

    type ScoreBonus = {
        Ability: Ability
        Bonus: int
        }

    type Abilities = {
        Scores: Scores
        Bonuses: ScoreBonus list
        }

    let score abilities ability =
        let baseScore = 
            let scores = abilities.Scores
            match ability with
            | STR -> scores.STR
            | DEX -> scores.DEX
            | CON -> scores.CON
            | INT -> scores.INT 
            | WIS -> scores.WIS
            | CHA -> scores.CHA
        let bonuses = 
            abilities.Bonuses 
            |> List.sumBy (fun bonus -> 
                if bonus.Ability = ability 
                then bonus.Bonus 
                else 0)
        baseScore + bonuses 

    let modifier abilities ability = 
        ability
        |> score abilities
        |> scoreToModifier

open Ability

let goblin = {
    Scores = {
        STR = 8
        DEX = 14
        CON = 10
        INT = 10
        WIS = 8
        CHA = 8
        }
    Bonuses = [ ]
    }

score goblin DEX
modifier goblin DEX