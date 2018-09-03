#load "markdown.fsx"
open Monster
open Monster.Ability

#load "monster.fsx"
open Markdown

let scimitar = {
    Name = "scimitar"
    Proficiency = Weapon.Simple
    Handling = Weapon.Light
    Grip = Weapon.SingleHanded(None)
    Finesse = true
    Damage = Roll (1, d6)
    DamageType = Slashing
    Usage = Weapon.Melee { Range = 5 }
    }

let shortbow = {
    Name = "shortbow"
    Proficiency = Weapon.Simple
    Handling = Weapon.Light
    Grip = Weapon.SingleHanded(None)
    Finesse = false
    Damage = Roll (1, d6)
    DamageType = Piercing
    Usage = Weapon.Ranged { ShortRange = 80; LongRange = 320 }
    }

let javelin = {
    Name = "javelin"
    Proficiency = Weapon.Simple
    Handling = Weapon.Normal
    Grip = Weapon.SingleHanded(None)
    Finesse = false
    Damage = Roll (1, d6)
    DamageType = Piercing
    Usage = 
        Weapon.Thrown ( 
            { Range = 5 }, 
            { ShortRange = 30; LongRange = 120 }
        )
    }

let longsword = {
    Name = "longsword"
    Proficiency = Weapon.Martial
    Handling = Weapon.Normal
    Grip = Weapon.SingleHanded(Some(Roll(1, d10)))
    Finesse = false
    Damage = Roll (1, d8)
    DamageType = Slashing
    Usage = Weapon.Melee { Range = 5 }
    }

let longbow = {
    Name = "longbow"
    Proficiency = Weapon.Martial
    Handling = Weapon.Heavy
    Grip = Weapon.TwoHanded
    Finesse = false
    Damage = Roll (1, d8)
    DamageType = Piercing
    Usage = Weapon.Ranged { ShortRange = 150; LongRange = 600 }
    }

let greatsword = {
    Name = "greatsword"
    Proficiency = Weapon.Martial
    Handling = Weapon.Heavy
    Grip = Weapon.TwoHanded
    Finesse = false
    Damage = Roll (2, d6)
    DamageType = Slashing
    Usage = Weapon.Melee { Range = 5 }
    }

let spear = {
    Name = "spear"
    Proficiency = Weapon.Simple
    Handling = Weapon.Light
    Grip = Weapon.SingleHanded(Some(Roll(1, d8)))
    Finesse = false
    Damage = Roll (1, d6)
    DamageType = Piercing
    Usage = 
        Weapon.Thrown (
            { Range = 5 }, 
            { ShortRange = 20; LongRange = 60 }
        )
    }

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
    Equipment = [ scimitar; shortbow ]
    Proficiency = Weapon.Simple
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
        Equipment = [ scimitar; javelin ]
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
    Equipment = [ longsword; longbow ]
    Proficiency = Weapon.Martial
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
        Equipment = [ greatsword; javelin ]
    }

hobgoblin |> Monster.HitPoints
hobgoblin |> Monster.HitPoints |> Roll.Average

hobgoblinCaptain |> Monster.HitPoints
hobgoblinCaptain |> Monster.HitPoints |> Roll.Average

let sheet = hobgoblinCaptain |> Markdown.monsterSheet


goblin |> Monster.Attacks
goblinBoss |> Monster.Attacks
hobgoblin |> Monster.Attacks
// discrepancy: hit bonus computed as 5, MM shows 4
// probably proficiency bonus / level is incorrect?
hobgoblinCaptain |> Monster.Attacks
