#load "markdown.fsx"
open Monster
open Monster.Ability

#load "monster.fsx"
open Markdown

let goblin = {
    Name = "Goblin"
    HitDice = 2
    Size = Small
    CreatureType = Humanoid
    Alignment = Social.Neutral, Evil
    Protection = 
        Equipment { 
            Armor = Some Leather 
            Shield = true
            }
    Speed = 30
    Abilities = {
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
    }

score goblin.Abilities STR
modifier goblin.Abilities STR

let goblinBoss = {
    goblin with
        Name = "Goblin Boss"
        HitDice = 6
        Protection = Equipment { 
            Armor = Some ChainShirt 
            Shield = true
            }               
        Abilities = {
            goblin.Abilities with
                Bonuses = [
                    { Ability = STR; Bonus = 2 }
                    { Ability = CHA; Bonus = 2 }
                    ]
        }
    }

score goblinBoss.Abilities STR
modifier goblinBoss.Abilities STR

let abilityBlock = 
    goblinBoss.Abilities 
    |> Markdown.abilities

let hobgoblin = {
    Name = "Hobgoblin"
    HitDice = 2
    Size = Medium       
    CreatureType = Humanoid
    Alignment = Lawful, Evil 
    Protection = 
        Equipment {
            Armor = Some ChainMail
            Shield = true
            }
    Speed = 30
    Abilities = {
        Scores = {
            STR = 13
            DEX = 12
            CON = 12
            INT = 10
            WIS = 10
            CHA = 9
            }
        Bonuses = [ ]
        }
    }

let hobgoblinCaptain = {
    hobgoblin with
        Name = "Hobgoblin Captain"
        HitDice = 6
        Protection = 
            Equipment {
                Armor = Some HalfPlate
                Shield = false
                }
        Abilities = {
            hobgoblin.Abilities with
                Bonuses = [
                    { Ability = STR; Bonus = 2 }
                    { Ability = DEX; Bonus = 2 }
                    { Ability = CON; Bonus = 2 }
                    { Ability = INT; Bonus = 2 }
                    { Ability = CHA; Bonus = 4 }
                    ]
        }
    }

hobgoblin |> Monster.HitPoints
hobgoblin |> Monster.HitPoints |> Roll.Average

hobgoblinCaptain |> Monster.HitPoints
hobgoblinCaptain |> Monster.HitPoints |> Roll.Average

let sheet = hobgoblinCaptain |> Markdown.monsterSheet

let scimitar = {
    Name = "scimitar"
    Proficiency = Weapon.Simple
    Handling = Weapon.Light
    Grip = Weapon.SingleHanded(None)
    Finesse = true
    Damage = Roll (1, d6)
    Usage = Weapon.Melee { Range = 5 }
    }

let shortbow = {
    Name = "shortbow"
    Proficiency = Weapon.Simple
    Handling = Weapon.Light
    Grip = Weapon.SingleHanded(None)
    Finesse = false
    Damage = Roll (1, d6)
    Usage = Weapon.Ranged { ShortRange = 80; LongRange = 320 }
    }

let spear = {
    Name = "spear"
    Proficiency = Weapon.Simple
    Handling = Weapon.Light
    Grip = Weapon.SingleHanded(Some(Roll(1, d8)))
    Finesse = false
    Damage = Roll (1, d6)
    Usage = 
        Weapon.Thrown (
            { Range = 5 }, 
            { ShortRange = 20; LongRange = 60 }
        )
    }

attacks goblin.Abilities (Weapon.Simple, goblin.HitDice) scimitar
attacks goblin.Abilities (Weapon.Simple, goblin.HitDice) shortbow
attacks goblin.Abilities (Weapon.Simple, goblin.HitDice) spear
