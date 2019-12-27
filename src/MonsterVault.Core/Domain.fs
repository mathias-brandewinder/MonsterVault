namespace MonsterVault
open System

[<AutoOpen>]
module Common =
    type CreatureID = | CreatureID of int
    type GroupID = | GroupID of int

[<AutoOpen>]
module DiceRolls =

    open System
    let rng = Random ()

    type Modifier =
        | Advantage
        | Disadvantage

    type Modification =
        | Adv
        | Dis
        | Mix

    type Dice =
        | D of Sides : int
        static member (*) (times: int, dice: Dice) = Roll (times, dice)
    and Roll =
        | Roll of int * Dice
        | Value of int
        | Add of Roll list
        | Modified of Modification * Roll
        static member (+) (v1: Roll, v2: Roll) =
            match v1, v2 with
            | Add (rolls1), Add (rolls2) -> Add (rolls1 @ rolls2)
            | Add (rolls1), roll2 -> Add (rolls1 @ [ roll2 ])
            | roll1, Add (rolls2) -> Add (roll1 :: rolls2)
            | roll1, roll2 -> Add [ roll1; roll2 ]
        static member (+) (roll: Roll, num: int) = roll + Value num
        static member (+) (num: int, roll: Roll) = Value num + roll
        static member roll (roll: Roll) =
            match roll with
            | Roll (times, D (sides)) ->
                Seq.init times (fun _ -> rng.Next(1, sides + 1))
                |> Seq.sum
            | Value (value) -> value
            | Add (rolls) -> rolls |> List.sumBy Roll.roll
            | Modified (modif, roll) ->
                match modif with
                | Mix -> roll |> Roll.roll
                | Adv -> max (roll |> Roll.roll) (roll |> Roll.roll)
                | Dis -> min (roll |> Roll.roll) (roll |> Roll.roll)
        static member With (modif: Modifier) (roll: Roll) =
            match roll with
            | Roll _
            | Value _
            | Add _ ->
                match modif with
                | Advantage -> Modified (Adv, roll)
                | Disadvantage -> Modified (Dis, roll)
            | Modified (modifiers, originalRoll) ->
                match modifiers with
                | Mix -> roll
                | Adv ->
                    match modif with
                    | Advantage -> roll
                    | Disadvantage -> Modified (Mix, originalRoll)
                | Dis ->
                    match modif with
                    | Disadvantage -> roll
                    | Advantage -> Modified (Mix, originalRoll)

    let d4 = D 4
    let d6 = D 6
    let d8 = D 8
    let d10 = D 10
    let d12 = D 12
    let d20 = D 20

module Abilities =

    type Ability =
        | STR
        | DEX
        | CON
        | INT
        | WIS
        | CHA

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

    let score scores ability =
        match ability with
        | STR -> scores.STR
        | DEX -> scores.DEX
        | CON -> scores.CON
        | INT -> scores.INT
        | WIS -> scores.WIS
        | CHA -> scores.CHA

    let modifier abilities ability =
        ability
        |> score abilities
        |> scoreToModifier

[<AutoOpen>]
module Space =

    [<Measure>]type ft

    type Direction =
        | N
        | NW
        | W
        | SW
        | S
        | SE
        | E
        | NE

    let directions = [ N; NW; W; SW; S; SE; E; NE ]

    type Position = {
        North: int
        West: int
        }

    let move (dir: Direction) (pos: Position) =
        match dir with
        | N -> { pos with North = pos.North + 1 }
        | NW ->
            { pos with
                North = pos.North + 1
                West = pos.West + 1
            }
        | W -> { pos with West = pos.West + 1 }
        | SW ->
            { pos with
                North = pos.North - 1
                West = pos.West + 1
            }
        | S -> { pos with North = pos.North - 1 }
        | SE ->
            { pos with
                North = pos.North - 1
                West = pos.West - 1
            }
        | E -> { pos with West = pos.West - 1 }
        | NE ->
            { pos with
                North = pos.North + 1
                West = pos.West - 1
            }

    let cellSize = 5<ft>

    let distance pos1 pos2 =
        max
            (abs (pos1.North - pos2.North))
            (abs (pos1.West - pos2.West))
        |> fun d -> cellSize * d

module Weapons =

    type Grip =
        | SingleHanded
        | TwoHanded

    type Usage = {
        Grip: Grip
        Damage: Roll
        }

    module Melee =

        type Versatile = {
            SingleHandedDamage: Roll
            TwoHandedDamage: Roll
            }

        type Handling =
            | Limited of Usage
            | Versatile of Versatile

        type Attacks = {
            Finesse: bool
            Handling: Handling
            Reach: int<ft>
            }

    module Ranged =

        type Range = {
            Short: int<ft>
            Long: int<ft>
            }

        type Attacks = {
            Range: Range
            Usage: Usage
            }

    module Thrown =

        type Attacks = {
            Melee: int<ft>
            ShortRange: int<ft>
            LongRange: int<ft>
            Handling: Melee.Handling
            }
            with
            member this.MeleeAttacks: Melee.Attacks = {
                Finesse = false
                Handling = this.Handling
                Reach = this.Melee
                }
            member this.RangedAttacks: Ranged.Attacks = {
                Range = {
                    Short = this.ShortRange
                    Long = this.LongRange
                    }
                Usage =
                    match this.Handling with
                    | Melee.Limited usage -> usage
                    | Melee.Versatile usage ->
                        {
                            Grip = SingleHanded
                            Damage = usage.SingleHandedDamage
                        }
                }

    type Attacks =
        | Melee of Melee.Attacks
        | Ranged of Ranged.Attacks
        | Thrown of Thrown.Attacks

    type Proficiency =
        | Simple
        | Martial

    type Weight =
        | Light
        | Medium
        | Heavy

    type Weapon = {
        Name: string
        Proficiency: Proficiency
        Weight: Weight
        Attacks: Attacks
        }

module Attacks =

    open Weapons

    type AttackType =
        | Melee of int<ft>
        | Ranged of Ranged.Range

    type Usage =
        | Natural
        | Equipped of Grip

    type Attack = {
        Weapon: string
        Usage: Usage
        Type: AttackType
        HitBonus: int
        Damage: Roll
        }

    let damage dodging ac (attack: Attack) =
        let baseAttackRoll =
            if dodging
            then
                1 * d20
                |> Roll.With Disadvantage
                |> Roll.roll
            else
                1 * d20 |> Roll.roll
        match baseAttackRoll with
        | 1 -> None // critical fail
        | 20 ->
            // critical hit
            [ attack.Damage; attack.Damage ]
            |> List.sumBy Roll.roll
            |> Some
        | roll ->
            let attackRoll = roll + attack.HitBonus
            if attackRoll < ac
            then None
            else
                attack.Damage
                |> Roll.roll
                |> Some

    [<RequireQualifiedAccess>]
    module Creature =

        open Weapons
        open Abilities

        type Statistics = {
            Description: string
            Abilities: Scores
            ProficiencyBonus: int
            Movement: int<ft>
            HitPoints: int
            ArmorClass: int
            Weapons: List<Weapon>
            Attacks: List<Attack>
            WeaponsProficiency: Weapons.Proficiency
            }
            with
            member this.abilityBonus (weapon: Weapon) =
                let finesse =
                    match weapon.Attacks with
                    | Attacks.Melee info -> info.Finesse
                    | Attacks.Thrown info -> info.MeleeAttacks.Finesse
                    | Attacks.Ranged _ -> false

                match finesse with
                | true ->  [ STR; DEX ]
                | false ->
                    match weapon.Attacks with
                    | Attacks.Melee(_) -> [ STR ]
                    | Attacks.Ranged(_) -> [ DEX ]
                    | Attacks.Thrown(_) -> [ STR ]
                |> Seq.maxBy (modifier this.Abilities)
                |> modifier this.Abilities
            member this.proficiencyBonus (weapon: Weapon) =
                match (weapon.Proficiency, this.WeaponsProficiency) with
                | Martial, Simple -> 0
                | _ -> this.ProficiencyBonus
            member this.AttacksWith (weapon: Weapon) =

                let hitBonus = this.abilityBonus weapon + this.proficiencyBonus weapon
                let damageBonus = this.abilityBonus weapon

                let meleeAttacks (attacks: Melee.Attacks) =
                    match attacks.Handling with
                    | Melee.Limited info ->
                        {
                            Weapon = weapon.Name
                            Type = AttackType.Melee (attacks.Reach)
                            HitBonus = hitBonus
                            Usage = Equipped info.Grip
                            Damage = info.Damage + damageBonus
                        }
                        |> List.singleton
                    | Melee.Versatile info ->
                        [
                            {
                                Weapon = weapon.Name
                                Type = AttackType.Melee (attacks.Reach)
                                HitBonus = hitBonus
                                Usage = Equipped SingleHanded
                                Damage = info.SingleHandedDamage + damageBonus
                            }
                            {
                                Weapon = weapon.Name
                                Type = AttackType.Melee (attacks.Reach)
                                HitBonus = hitBonus
                                Usage = Equipped TwoHanded
                                Damage = info.TwoHandedDamage + damageBonus
                            }
                        ]

                let rangedAttacks (info: Ranged.Attacks) =
                    {
                        Weapon = weapon.Name
                        Type = AttackType.Ranged (info.Range)
                        HitBonus = hitBonus
                        Usage = Equipped info.Usage.Grip
                        Damage = info.Usage.Damage + damageBonus
                    }
                    |> List.singleton

                match weapon.Attacks with
                | Weapons.Melee info -> meleeAttacks info
                | Weapons.Ranged info -> rangedAttacks info
                | Weapons.Thrown info ->
                    [
                        yield! info.MeleeAttacks |> meleeAttacks
                        yield! info.RangedAttacks |> rangedAttacks
                    ]

            member this.AllAttacks () =
                this.Weapons
                |> List.collect (fun weapon -> this.AttacksWith weapon)
                |> List.append this.Attacks

        type State = {
            HasTakenReaction: bool
            Dodging: bool
            HitPoints: int
            Position: Position
            Dead: bool
            Group: GroupID
            }
            with
            member this.CanAct =
                not (this.Dead)
            member this.CanReact =
                this.CanAct && (not this.HasTakenReaction)

        let initialize (stats: Statistics, group: GroupID, pos: Position) =
            {
                HitPoints = stats.HitPoints
                Group = group
                Position = pos
                HasTakenReaction = false
                Dodging = false
                Dead = false
            }
