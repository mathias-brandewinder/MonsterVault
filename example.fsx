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
    Alignment = Neutral, Evil
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