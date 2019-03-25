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

Extended model: can I represent "things" as
Capabilities (action I can take) and 
Effects (how they modify state or resolution)
? how do I handle "multiattack" for instance
? how do I represent Hide -> gives advantage, but invisible
? how do I represent Cover, Partial Cover
*)

type Modifier = 
    | Advantage
    | Disadvantage

type Modification =
    | Adv
    | Dis
    | Mix 

type Dice = 
    | D of int
    static member (*) (times: int, d: Dice) = Roll(times, d)
and DiceRoll = 
    | Roll of int * Dice
    | Value of int 
    | Add of list<DiceRoll>
    | Modified of (Modification * DiceRoll)
    static member With (modif: Modifier) (roll: DiceRoll) =
        match roll with 
        | Roll _ 
        | Value _  
        | Add _ -> 
            match modif with 
            | Advantage -> Modified (Adv, roll)
            | Disadvantage -> Modified (Dis, roll)
            //Modified (set [ modif ], roll)
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
    static member (+) (roll1: DiceRoll, roll2: DiceRoll) =
        Add [ roll1; roll2 ]
    static member (+) (roll: DiceRoll, x: int) =
        Add [ roll; Value x ]
    static member (+) (x: int, roll: DiceRoll) =
        Add [ roll; Value x ]

let d4 = D 4
let d6 = D 6
let d8 = D 8
let d10 = D 10
let d12 = D 12
let d20 = D 20

let adv roll = roll |> DiceRoll.With Advantage
let dis roll = roll |> DiceRoll.With Disadvantage

1 + adv (2 * D 10 + 4)
(1 * d6 + 4) + 4 * d4
adv (1 * d6) + adv (2 * d10)

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
        match modifs with
        | Mix -> eval roll 
        | Adv -> 
            max (eval roll) (eval roll)
        | Disadv ->
            min (eval roll) (eval roll)

Seq.init 1000000 (fun _ ->
    Roll(1, D 10) 
    |> DiceRoll.With Disadvantage  
    |> DiceRoll.With Advantage  
    |> eval
    |> float 
    )
|> Seq.average

(*
attack roll: 
    d20
    use DEX or STR (or maybe even something else for spellcasting?)
    proficiency (simple, martial)
    advantage, disadvantage
    potential extra dice (guidance, buffs...)
    Hide, Dodge
defense
    AC
    advantage, disadvantage
    proficiency (type of armor)
    cover
damage roll:
    given by the attack itself
    modifier: STR or DEX
    proficiency (simple, martial)
    vulnerability, resistance to types of damage
    feats/abilities, ex: Sneak Attack, +1d6 if advantage
*)

type CreatureID = | CreatureID of int

type Reach = 
    | Melee of int 
    | Ranged of int * int

type Damage = 
    | Slashing
    | Bludgeoning
    | Poison

// need to distinguish natural (ex: claw) vs weapon?
type Attack = {
    Damages: list<Damage * DiceRoll>
    Reach: Reach
    }

type Command = {
    Attacker: CreatureID
    Target: CreatureID
    Attack: Attack
    }

type CreatureStats = {
    STR: int
    DEX: int
    }

// type EffectOn = 
//     | AttackRoll 
//     | DamageRoll 
//     | Defense 

type State = {
    Stats: Map<CreatureID, CreatureStats> 
    Effects: list<Effect> 
    }
and Effect = {
    Description: string
    Condition: State * Command -> bool
    Impact: DiceRoll -> DiceRoll
    }

// resolution: need attack roll vs AC or equivalent
// need damage roll in case of success
// in other words, need to construct 3 expressions
// that will be evaluated:
// attack strength
// defense strength
// damage 

// more general representation?
// action can be opposed, with effect on something
// can fail or succeed

// can I represent EVERY effect that way?
// for instance, creature is Prone, or creature abilities

// extract "metrics" from state?

let modifier x = (x - 10) / 2

let modifierFor state (cmd: Command) = 
    let stats = state.Stats.[cmd.Attacker]
    match cmd.Attack.Reach with
    | Melee _ -> stats.STR
    | Ranged _ -> stats.DEX
    |> modifier
    
let proficiencyFor state cmd = 0

let strength (state: State) (cmd: Command) = 
    let baseRoll = 1 * d20
    let ability = modifierFor state cmd
    let proficiency = proficiencyFor state cmd
    let effects = 
        state.Effects 
        |> List.filter (fun effect -> effect.Condition (state, cmd))
        |> List.map (fun effect -> effect.Impact)            
    let baseValue = baseRoll + ability + proficiency
    List.fold (fun roll eff -> eff roll) baseValue effects

(1 * d20 + 4) + (1 * d4 + 2) |> adv 

let dodge (creature: CreatureID) = {
    Description = "Dodge"
    Condition = 
        fun (state, cmd) ->
            cmd.Target = creature
    Impact = dis
    }

let packTactics (creature: CreatureID) = {
    Description = "Pack Tactics"
    Condition = 
        fun (state, cmd) ->
            cmd.Attacker = creature
            // && cmd.Target has enemy in contact that is not Attacker
    Impact = adv
    }

let state = {
    Stats = [
        CreatureID 1, { DEX = 15; STR = 8 }
        CreatureID 2, { DEX = 10; STR = 13 }
        ]
        |> Map.ofList
    Effects = [
        dodge (CreatureID 2)
        ]
    }

let cmd = {
    Attacker = CreatureID 1
    Target = CreatureID 2
    Attack = { 
        Damages = [ (Slashing, 1 * d6) ] 
        Reach = Melee 5 
        }
    }

[ 1 .. 1000 ] 
|> List.averageBy (fun _ ->
    strength state cmd 
    |> eval 
    |> float
    )

// model: unresolved command -> modify until resolvable -> resolve?
