type Dice = 
    | D of Sides : int
    static member (*) (times:int,dice:Dice) = Roll(times,dice)
and Roll = 
    | Roll of int * Dice
    | Value of int
    | Add of Roll list
    static member Average (roll:Roll) =
        let rec average (roll:Roll) =
            match roll with
            | Roll(times,D(sides)) -> (times * (sides + 1)) / 2
            | Value(value) -> value
            | Add(rolls) ->
                rolls |> List.sumBy average
        average roll
    static member Render (roll:Roll) =
        match roll with
        | Roll(times,D(sides)) -> sprintf "%id%i" times sides
        | Value(value) -> sprintf "%i" value
        | Add(rolls) -> 
            rolls 
            |> List.map Roll.Render 
            |> String.concat "+"
    static member (+) (v1:Roll,v2:Roll) = 
        match v1,v2 with
        | Add(rolls1), Add(rolls2) -> Add(rolls1 @ rolls2)
        | Add(rolls1), roll2 -> Add(rolls1 @ [ roll2 ])
        | roll1, Add(rolls2) -> Add(roll1 :: rolls2)
        | roll1, roll2 -> Add [ roll1 ; roll2 ]
    static member (+) (roll:Roll,num:int) = roll + Value num
    static member (+) (num:int,roll:Roll) = Value num + roll

let d4 = D 4
let d6 = D 6
let d8 = D 8
let d10 = D 10
let d12 = D 12
let d20 = D 20

type Ability = 
    | STR
    | DEX
    | CON 
    | INT 
    | WIS
    | CHA
    
type CreatureType = 
    | Aberration 
    | Beast 
    | Celestial 
    | Construct 
    | Dragon 
    | Elemental 
    | Fey 
    | Fiend 
    | Giant 
    | Humanoid 
    | Monstrosity 
    | Ooze 
    | Plant 
    | Undead 

type Size = 
    | Tiny
    | Small 
    | Medium 
    | Large 
    | Huge 
    | Gargantuan

type Social = 
    | Lawful 
    | Neutral 
    | Chaotic 

type Moral = 
    | Good 
    | Neutral 
    | Evil 

type Alignment = Social * Moral 

type Armor = 
    | Padded 
    | Leather 
    | StuddedLeather 
    | Hide 
    | ChainShirt 
    | ScaleMail 
    | BreastPlate 
    | HalfPlate 
    | RingMail 
    | ChainMail 
    | Splint 
    | Plate 

type ProtectiveGear = {
    Armor: Armor
    Shield: bool
    }

let armorClass gear dex =
    let armor = gear.Armor
    match armor with
    | Padded -> 11 + dex
    | Leather -> 11 + dex
    | StuddedLeather -> 12 + dex
    | Hide -> 12 + min 2 dex
    | ChainShirt -> 13 + min 2 dex
    | ScaleMail -> 14 + min 2 dex
    | BreastPlate -> 14 + min 2 dex
    | HalfPlate -> 15 + min 2 dex
    | RingMail -> 14
    | ChainMail -> 16
    | Splint -> 17
    | Plate -> 18
    |> match gear.Shield with
        | true -> (+) 2
        | false -> id

[<AutoOpen>]
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

let hitPointsDice (size:Size) =
    match size with
    | Tiny -> d4
    | Small -> d6 
    | Medium -> d8
    | Large -> d10
    | Huge -> d12
    | Gargantuan -> d20

type Monster = {
    Name: string
    Size: Size
    CreatureType: CreatureType
    Alignment: Alignment
    Armor: ProtectiveGear
    Speed: int
    HitDice: int
    Abilities: Abilities
    }
    with
    static member HitPoints (monster:Monster) = 
        monster.HitDice * hitPointsDice monster.Size
        + monster.HitDice * modifier monster.Abilities CON
    static member AC (monster:Monster) =
        armorClass monster.Armor (modifier monster.Abilities DEX)