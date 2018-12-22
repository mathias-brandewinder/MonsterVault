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

type DamageType = 
    | Acid 
    | Bludgeoning 
    | Cold 
    | Fire 
    | Force 
    | Lightning 
    | Necrotic 
    | Piercing 
    | Poison 
    | Psychic 
    | Radiant 
    | Slashing 
    | Thunder 

let proficiencyBonus level =
    if level <= 4 then 2
    elif level <= 8 then 3
    elif level <= 12 then 4
    elif level <= 16 then 5
    else 6

[<RequireQualifiedAccess>]
module Weapon = 

    type Proficiency = 
        | Simple 
        | Martial 

    type Handling = 
        | Light 
        | Normal 
        | Heavy 

    type Grip = 
        | SingleHanded of Versatile: Roll option
        | TwoHanded

    type MeleeInfo = {
        Range: int
        }

    type RangedInfo = {
        ShortRange: int
        LongRange: int
        }

    type ThrownInfo = {
        Melee: MeleeInfo
        Ranged: RangedInfo
        }

    type Usage = 
        | Melee of MeleeInfo
        | Ranged of RangedInfo
        | Thrown of MeleeInfo * RangedInfo

type Weapon = {
    Name: string
    Proficiency: Weapon.Proficiency
    Handling: Weapon.Handling
    Grip: Weapon.Grip
    Finesse: bool
    Damage: Roll
    DamageType: DamageType
    Usage: Weapon.Usage
    }
    
type AttackGrip = 
    | SingleHanded
    | TwoHanded
    | OffHand
     
type AttackInfo = {
    Weapon: string
    Grip: AttackGrip
    HitBonus: int
    Damage: Roll
    DamageBonus: int
    DamageType: DamageType
    }

type Attack = 
    | Melee of Weapon.MeleeInfo
    | Ranged of Weapon.RangedInfo

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
    Armor: Armor option
    Shield: bool
    }

type Protection = 
    | Natural of Bonus : int
    | Equipment of ProtectiveGear

let armorClass protection dex =
    match protection with
    | Natural(bonus) -> 10 + dex + bonus
    | Equipment(gear) ->
        match gear.Armor with
        | None -> 10 + dex
        | Some(armor) ->
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

let meleeAttacks 
    (abilities:Abilities)
    (proficiency:Weapon.Proficiency) 
    (level:int) 
    (weapon:Weapon) =

        let ability = 
            match weapon.Finesse with
            | true ->  [ STR; DEX ] 
            | false -> [ STR ]
            |> Seq.maxBy (modifier abilities)
            |> modifier abilities
        let proficiency = 
            match weapon.Proficiency, proficiency with
            | Weapon.Martial, Weapon.Simple -> 0
            | _ -> proficiencyBonus level

        match weapon.Usage with
        | Weapon.Thrown(info,_)
        | Weapon.Melee(info) -> 
            [
                match weapon.Grip with
                | Weapon.SingleHanded(versatile) ->
                    yield { 
                        Weapon = weapon.Name
                        Grip = SingleHanded
                        HitBonus = ability + proficiency
                        Damage = weapon.Damage
                        DamageBonus = ability
                        DamageType = weapon.DamageType
                        }
                    match versatile with
                    | None -> ignore ()
                    | Some(versatileRoll) ->
                        yield { 
                            Weapon = weapon.Name
                            Grip = TwoHanded
                            HitBonus = ability + proficiency
                            Damage = versatileRoll
                            DamageBonus = ability
                            DamageType = weapon.DamageType
                            }
                    match weapon.Handling with
                    | Weapon.Light -> 
                        yield { 
                            Weapon = weapon.Name
                            Grip = OffHand
                            HitBonus = ability + proficiency
                            Damage = weapon.Damage
                            DamageBonus = min ability 0
                            DamageType = weapon.DamageType
                            }
                    | _ -> ignore () 
                | Weapon.TwoHanded ->
                    yield { 
                        Weapon = weapon.Name
                        Grip = TwoHanded
                        HitBonus = ability + proficiency
                        Damage = weapon.Damage
                        DamageBonus = ability
                        DamageType = weapon.DamageType
                        }
            ]
            |> List.map (fun attack -> Melee(info), attack)
        | _ -> []

let rangedAttacks 
    (abilities:Abilities)
    (proficiency:Weapon.Proficiency) 
    (level:int) 
    (weapon:Weapon) =

        let ability = 
            match weapon.Finesse with
            | true ->  [ STR; DEX ] 
            | false -> 
                match weapon.Usage with
                | Weapon.Thrown(_) -> [ STR ]
                | _ -> [ DEX ]
            |> Seq.maxBy (modifier abilities)
            |> modifier abilities
        let proficiency = 
            match weapon.Proficiency, proficiency with
            | Weapon.Martial, Weapon.Simple -> 0
            | _ -> proficiencyBonus level
        let attackGrip = 
            match weapon.Grip with
            | Weapon.SingleHanded(_) -> SingleHanded
            | Weapon.TwoHanded -> TwoHanded

        match weapon.Usage with
        | Weapon.Thrown(_,info)
        | Weapon.Ranged(info) -> 
            [
                { 
                    Weapon = weapon.Name
                    Grip = attackGrip
                    HitBonus = ability + proficiency
                    Damage = weapon.Damage
                    DamageBonus = ability
                    DamageType = weapon.DamageType
                }
            ]
            |> List.map (fun attack -> Ranged(info), attack)
        | _ -> []

let attacks 
    (abilities:Abilities)
    (proficiency:Weapon.Proficiency) 
    (level:int) 
    (weapon:Weapon) =
        [
            yield! meleeAttacks abilities proficiency level weapon
            yield! rangedAttacks abilities proficiency level weapon
        ]

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
    Protection: Protection
    Speed: int
    HitDice: int
    Abilities: Abilities
    Equipment: Weapon list
    Proficiency: Weapon.Proficiency
    }
    with
    static member HitPoints (monster:Monster) = 
        monster.HitDice * hitPointsDice monster.Size
        + monster.HitDice * modifier monster.Abilities CON
    static member AC (monster:Monster) =
        armorClass monster.Protection (modifier monster.Abilities DEX)
    static member Attacks (monster:Monster) =
        let attacks = 
            attacks monster.Abilities monster.Proficiency monster.HitDice
        monster.Equipment
        |> List.collect attacks
