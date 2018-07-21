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

let modifier abilities ability = 
    match ability with
    | STR -> abilities.STR
    | DEX -> abilities.DEX
    | CON -> abilities.CON
    | INT -> abilities.INT 
    | WIS -> abilities.WIS
    | CHA -> abilities.CHA
    |> scoreToModifier

let goblin = {
    STR = 8
    DEX = 14
    CON = 10
    INT = 10
    WIS = 8
    CHA = 8
    }

modifier goblin DEX