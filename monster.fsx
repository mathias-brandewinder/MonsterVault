type Ability = 
    | STR
    | DEX
    | CON 
    | INT 
    | WIS
    | CHA

module Ability = 

    let abilities = [ STR; DEX; CON; INT; WIS; CHA ]

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

type Dice = | D of Sides : int

type Roll = 
    | Roll of int * Dice
    | Value of int
    | Add of Roll list
    static member Default (roll:Roll) =
        let rec eval (roll:Roll) =
            match roll with
            | Roll(times,D(sides)) -> (times * (sides + 1)) / 2
            | Value(value) -> value
            | Add(rolls) ->
                rolls |> List.sumBy eval
        eval roll
    static member Render (roll:Roll) =
        match roll with
        | Roll(times,D(sides)) -> sprintf "%id%i" times sides
        | Value(value) -> sprintf "%i" value
        | Add(rolls) -> 
            rolls 
            |> List.map Roll.Render 
            |> String.concat "+"

Roll.Default (Add [ Roll(3, D 8); Value 9 ])
Roll.Render (Add [ Roll(3, D 8); Value 9 ])
