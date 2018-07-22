#load "monster.fsx"

open Monster
open Monster.Ability

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

score goblin STR
modifier goblin STR

let goblinBoss = {
    goblin with
        Bonuses = [
            { Ability = STR; Bonus = 2 }
            { Ability = CHA; Bonus = 2 }
            ]
    }

score goblinBoss STR
modifier goblinBoss STR