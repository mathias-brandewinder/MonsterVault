type Ability = 
    | STR
    | DEX
    | CON 
    | INT 
    | WIS
    | CHA

type AbilityScores = {
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

type AbilityScoreBonus = {
    Ability: Ability
    Bonus: int
    }

type Abilities = {
    Scores: AbilityScores
    Bonuses: AbilityScoreBonus list
    }

let modifier abilities ability = 
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
    |> scoreToModifier

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

modifier goblin DEX