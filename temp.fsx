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

type Ability = 
    | STR
    | DEX
    | CON 
    | INT 
    | WIS
    | CHA

let score scores ability =
    match ability with
    | STR -> scores.STR
    | DEX -> scores.DEX
    | CON -> scores.CON
    | INT -> scores.INT 
    | WIS -> scores.WIS
    | CHA -> scores.CHA

let modifier scores ability =
    ability
    |> score scores
    |> scoreToModifier

let goblin = {
    STR = 8
    DEX = 14
    CON = 10
    INT = 10
    WIS = 8
    CHA = 8
    }

modifier goblin STR

let abilities = [ STR; DEX; CON; INT; WIS; CHA ]

[<RequireQualifiedAccess>]
module Markdown = 

    let signed (value) = 
        if value > 0
        then sprintf "+%i" value
        else sprintf "%i" value

    let abilities (scores:Scores) =
        [
            abilities 
            |> List.map (sprintf "%A") 
            |> String.concat " | "
            
            abilities 
            |> List.map (fun _ -> ":---:") 
            |> String.concat " | "
            
            abilities 
            |> List.map (score scores >> sprintf "%i") 
            |> String.concat " | "
            
            abilities 
            |> List.map (modifier scores >> signed) 
            |> String.concat " | "
        ]
        |> String.concat "  \n"

goblin |> Markdown.abilities




type Dice = | D of Sides : int
type Roll = 
    | Roll of int * Dice
    | Value of int
    | Add of Roll list
    static member Default (roll:Roll) =
        let rec eval (total:int) (roll:Roll) =
            match roll with
            | Roll(times,D(sides)) -> (times * (sides + 1)) / 2
            | Value(value) -> value
            | Add(rolls) ->
                rolls |> List.sumBy (eval 0)
        eval 0 roll

Roll.Default (Add [ Roll(3, D 8); Value 9 ])

let rec display (roll:Roll) =
    match roll with
    | Roll(number,D(sides)) -> sprintf "%id%i" number sides
    | Value(number) -> sprintf "%i" number
    | Add(rolls) -> 
        rolls 
        |> List.map display 
        |> String.concat "+"

Add [ Roll(3, D 8); Value 9 ] |> display
Add [ Add [ Roll(3, D 8); Value 2 ]; Add [ Roll(2, D 10); Value 4 ] ] |> display

// how does the CON modifier work?
// also, default different for Adventurers and Monsters
let hitPoints (description:Description) (abilities:Ability.Abilities) =
    let sides = 
        match description.Size with
        | Tiny -> 4
        | Small -> 6
        | Medium -> 8
        | Large -> 10
        | Huge -> 12
        | Gargantuan -> 20 
    let constitution =
        Ability.modifier abilities CON
    let level = description.Level
    if constitution <> 0
    then 
        Add [ Roll(level,D sides); Value(level * constitution) ]
    else
        Roll(level,D sides)
        
hitPoints Medium 3 |> display 



type Skill = 
    // STR
    | Athletics
    // DEX
    | Acrobatics
    | SleightOfHand
    | Stealth
    // INT
    | Arcana
    | History
    | Investigation 
    | Nature 
    | Religion 
    // WIS
    | AnimalHandling 
    | Insight 
    | Medicine 
    | Perception 
    | Survival 
    // CHA
    | Deception
    | Intimidation 
    | Performance 
    | Persuasion 

type Size = 
    | Tiny 
    | Small 
    | Medium
    | Large 
    | Huge 
    | Gargantuan 

type Description = {
    Size: Size
    Level: int
    }