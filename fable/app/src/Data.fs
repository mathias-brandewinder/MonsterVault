namespace MonsterVault

module Data = 

    open Space
    open Weapons
    open Combat

    module Equipment = 

        let scimitar = {
            Name = "scimitar"
            Proficiency = Martial
            Weight = Light
            Attacks = 
                Melee {
                    Handling = Melee.Limited { Grip = SingleHanded; Damage = 1 * d6 }
                    Reach = 5<ft>
                    Finesse = true      
                }
            }

        let shortbow = { 
            Name = "shortbow"
            Proficiency = Simple
            Weight = Medium
            Attacks = 
                Ranged {
                    Range = { Short = 80<ft>; Long = 320<ft> }
                    Usage = { Grip = TwoHanded; Damage = 1 * d6 }
                    }                    
            }

        let spear = {
            Name = "spear"
            Proficiency = Simple
            Weight = Medium
            Attacks = 
                Thrown {
                    Melee = 5<ft>
                    ShortRange = 20<ft>
                    LongRange = 60<ft>
                    Handling = Melee.Versatile {
                        SingleHandedDamage = 1 * d6
                        TwoHandedDamage = 1 * d8
                    }
                }
            }

    module Monsters = 

        open Equipment
        
        let goblin : Creature.Statistics = 
            let stats: Abilities.Scores = {
                STR = 8
                DEX = 14
                CON = 10
                INT = 10
                WIS = 8
                CHA = 8
                }
            {
                Creature.Description = "goblin" 
                Creature.Abilities = stats
                Creature.ProficiencyBonus = 2
                Creature.Statistics.HitPoints = 7
                Creature.Movement = 30<ft>
                Creature.ArmorClass = 15
                Creature.WeaponsProficiency = Weapons.Martial
                Creature.Weapons = [ scimitar; shortbow ]
                Creature.Attacks = [ ]
            }

        let wolf : Creature.Statistics = 
            let stats: Abilities.Scores = {
                STR = 12
                DEX = 15
                CON = 12
                INT = 3
                WIS = 12
                CHA = 6
                }
            { 
                Creature.Description = "wolf" 
                Creature.Abilities = stats
                Creature.ProficiencyBonus = 2
                Creature.Statistics.HitPoints = 11
                Creature.Movement = 40<ft>
                Creature.ArmorClass = 13
                Creature.WeaponsProficiency = Weapons.Simple
                Creature.Weapons = [ ]
                Creature.Attacks = [
                        {
                            Weapon = "bite"
                            Usage = Attacks.Natural
                            Type = Attacks.AttackType.Melee 5<ft>
                            HitBonus = 4
                            Damage = 2 * d4 + 2
                        }
                    ]
            }
