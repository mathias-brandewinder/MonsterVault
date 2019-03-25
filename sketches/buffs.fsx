(*
issues / questions
- how to represent advantage, disadvantage
- how to express and resolve buffs / debuffs

Example: Creature1 Dodges -> any attack against it should be at disadvantage
Example: Creature with Sensitivity, Resistance to specific damage
Example: Pack Tactics, if another creature is melee attacking X, I get advantage attacking X

What are the calculation atoms?
- 6 Ability Scores
- HP
- AC
- Level -> Proficiency Bonus
- Proficiencies
*)

type Modifier = 
    | Advantage
    | Disadvantage

type Dice = 
    | D of int
    static member (*) (times: int, d: Dice) = Roll(times, d)
and DiceRoll = 
    | Roll of int * Dice
    | Value of int 
    | Add of list<DiceRoll>
    | Modified of (Set<Modifier> * DiceRoll)
    static member With (modif: Modifier) (roll: DiceRoll) =
        match roll with 
        | Roll _ 
        | Value _  
        | Add _ -> Modified (set [ modif ], roll)
        | Modified (modifiers, roll) -> 
            Modified (modifiers |> Set.add modif, roll)
    static member (+) (roll1: DiceRoll, roll2: DiceRoll) =
        Add [ roll1; roll2 ]
    static member (+) (roll: DiceRoll, x: int) =
        Add [ roll; Value x ]
    static member (+) (x: int, roll: DiceRoll) =
        Add [ roll; Value x ]

let adv roll = roll |> DiceRoll.With Advantage
let dis roll = roll |> DiceRoll.With Disadvantage

1 + adv (2 * D 10)

Roll(4, D 10) 
|> DiceRoll.With Advantage  
|> DiceRoll.With Disadvantage
|> fun roll -> Add [ Roll(4, D 20); roll ]
|> DiceRoll.With Advantage

let rng = System.Random ()

let rec eval (roll: DiceRoll) = 
    match roll with
    | Roll(times, D sides) -> 
        Seq.init times (fun _ -> rng.Next(1, sides + 1))
        |> Seq.sum
    | Value x -> x
    | Add rolls -> rolls |> Seq.sumBy eval
    | Modified (modifs, roll) ->
        match (modifs |> Set.toList) with
        | [] -> eval roll 
        | [ Advantage ] -> 
            max (eval roll) (eval roll)
        | [ Disadvantage ] ->
            min (eval roll) (eval roll)
        | _ -> eval roll 

Seq.init 1000000 (fun _ ->
    Roll(1, D 10) 
    |> DiceRoll.With Advantage  
    |> eval
    |> float 
    )
|> Seq.average

// Dodge
// description: attack roll against X gets one Disadvantage


type CreatureID = | CreatureID of int

type AttackDescription = {
    Origin: CreatureID
    Target: CreatureID 
    }

type Action = 
    | Attack of AttackDescription 
    | Move

// this should be a predicate over an Action:
// does the action match?
// problem: need to describe also 'pack tactics'
type ActionFilter = Action -> bool

// any attack on the dodging creature
let dodgeTrigger creature action = 
    match action with
    | Attack description -> description.Target = creature
    | _ -> false

type Trigger = 
    | ActionBased of ActionFilter 
    | SituationBased of Action // not correct, but will figure out
    | And of Trigger * Trigger
    | Or of Trigger * Trigger
    // | Xor of Trigger * Trigger

// tuple? 
type EffectModification = 
    | Value of int
    | ModifiedRoll of Modifier

type EffectTarget = 
    | AttackRoll
    | DamageRoll

type Effect = {
    Target: EffectTarget
    Description: EffectModification
    }

type TimeLimit = int // don't care for now, easy
type UsageLimit = int // don't care for now, easy

type Buff = {
    // when does it apply / action
    Trigger: Trigger
    // how does it modify attacks
    Effect: Effect
    // at what point in time should it stop having effect
    TimeLimit: TimeLimit
    // is this one, unlimited, some number of usages
    UsageLimit: UsageLimit
    }

let dodge creature = {
    Trigger = ActionBased (dodgeTrigger creature)
    Effect = {
        Target = AttackRoll
        Description = ModifiedRoll Disadvantage
        }
    TimeLimit = 0 // until X starts its next turn, before any action
    UsageLimit = 0 // no limit
    }

let impact buff action =
    match buff.Trigger with
    | Action =  


let creature1 = CreatureID 1
let creature2 = CreatureID 2

let attack = {
    Origin = creature1
    Target = creature2
    }

