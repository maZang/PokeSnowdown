(* Info contains all the data types needed for the handling of the game *)
type battlemove = Poke of string | UseAttack of string | NoMove | FaintPoke of string | Preprocess | TurnEnd | AIMove

type stat = Attack | Defense | SpecialAttack | SpecialDefense | Speed | Accuracy | Evasion

type guiattack = NormMove of string | Crit of guiattack |
                  SEff of guiattack | NoEff of guiattack | HitMult of int * guiattack | BurnMove of guiattack | FreezeMove of guiattack | ParaMove of guiattack | MissMove of string | FrozenSolid |
                  Thaw of guiattack | NoFreeze of guiattack | NoBurn of guiattack | NoPara of guiattack | Para | OHKill of guiattack | ChargingMove of string * string | FlinchA |
                  Recoil of guiattack | PoisonMove of guiattack | Asleep | Wake of guiattack | Confused | BreakConfuse of guiattack | StatAttackA of stat * int * guiattack
                  | ConfuseMoveA of guiattack | Recharging of guiattack | DrainA of guiattack | UserFaintA of guiattack | NoEffAll of string | DrainSleepFail of string

type guistatus = StatBoost of stat * int * guistatus | NormStatus of string | ThawS of guistatus | FrozenSolidS | MissStatus of string | NoFreezeS of guistatus | NoBurnS of guistatus |
                  NoParaS of guistatus | ParaS | SwitchOut of guistatus | FlinchS | StatAttack of stat * int * guistatus | AsleepS | WakeS of guistatus | MakeSleep of guistatus
                  | ConfusedS | BreakConfuseS of guistatus | ConfuseMove of guistatus | LeechS of guistatus | PoisonStatus of guistatus | ParaStatus of guistatus
                  | BadPoisonStatus of guistatus | HealHealth of guistatus | LightScreenS of guistatus | HazeS of guistatus | ReflectS of guistatus | RestS of guistatus

type endMove = BurnDmg | BreakBurn | BreakFreeze  | BreakPara  | BreakPoison | PoisonDmg | LeechDmg of endMove | LeechHeal of endMove | Base | LightScreenFade of endMove |
               ReflectFade of endMove

type guimove = SPoke of string | AttackMove of guiattack | Faint | NoAction | Continue | Next | Status of guistatus | EndMove of endMove | FaintNext

type playerMove = Pl1 of guimove | Pl2 of guimove

type target = SpecificPoke | SelectedPokeMeFirst | Ally | UsersField |
    UserOrAlly | OpponentsFields | User | RandomOpp | AllOthers |
    SelectedPoke | AllOpp | EntireField | UserAndAlly | All

type dmg_class = Physical | Special | Status

type element = Fire | Water | Grass | Rock | Ground | Fairy | Dark | Electric |
  Ghost | Steel | Normal | Bug | Flying | Psychic | Ice | Dragon | Fighting |
  Poison

type flag = Contact | Charge | Protect | Recharge | Reflectable | Snatch |
            Mirror | Punch | Sound | Gravity | Defrost | Distance | Heal |
            Authentic | Powder | Bite | Pulse | Ballistics | Mental |
            NonSkyBattle

type evs = {attack:int; defense:int; special_attack: int; special_defense: int;
            hp:int; speed:int}

(* variants containing all secondary effects of a given move *)
type secondary_effects = MultHit of int | StageBoost of (stat * int) list | IncCrit of int | RandMultHit | BurnChance | FreezeChance | ParaChance | OHKO | ChargeMove of string |
                         ForceSwitch | FlinchMove | StageAttack of (stat * int) list | RecoilMove | PoisonChance | PutToSleep | ConfuseOpp | ConstantDmg of int | RechargeMove |
                         WeightDamage | DrainMove | LeechSeed | ChargeInSunlight of string | ToxicChance | StageBoostSunlight of (stat * int) list | Recovery | LightScreenMake
                         | Haze | ReflectMake | UserFaint | NeverMiss | DrainMoveSleep | VariableDamage | Rest | SuperFang

type move = {name:string; priority: int; target: target; dmg_class: dmg_class;
    mutable power:int; effect_chance: int; accuracy: int; element: element;
    description: string; secondary: secondary_effects list}

type item = Nothing | Leftovers | ChoiceBand | LifeOrb | CharizarditeX |
            ChoiceSpecs

type terrain_element = LightScreen of int | Reflect of int

type terrain = {side1: terrain_element list ref; side2: terrain_element list ref}

type weather = HarshSun | Hail | Rain | SandStorm
  | HeavyRain | Sun | AirCurrent | ClearSkies

(* We hope to implement all of these below but we will see *)
type weather_terrain = {mutable weather: weather; terrain: terrain}

type non_volatile_status = Burn | Freeze | Paralysis | Poisoned | Toxic of int | Sleep of int |
                          NoNon

type volatile_status =  Confusion of int | Curse | Embargo | Encore | Flinch | HealBlock
	| Identification | Infatuation | Nightmare | Trapped | PerishSong | Leeched
	| Taunt | Levitate | Torment | Charge

type status = non_volatile_status * volatile_status list

type nature = Hardy | Lonely | Adamant | Naughty | Brave | Bold | Docile |
              Impish | Lax | Relaxed | Modest | Mild | Bashful | Rash | Quiet
              | Calm | Gentle | Careful | Quirky | Sassy | Timid | Hasty |
              Jolly | Naive | Serious

type pokemon = {name: string; element: element list; move1: move; move2: move;
  move3: move; move4: move; hp: int; attack: int; defense:int;
  special_defense:int; special_attack:int; speed:int; ability:string; evs: evs;
  nature: nature; item: item}

type battle_poke = {pokeinfo: pokemon; mutable curr_hp:int; mutable curr_status: status;
  mutable curr_item: item; bhp:int; battack:int; bdefense:int; bspecial_attack:int;
  bspecial_defense:int; bspeed:int}

(* stat modifier represents number of stages followed by multiplier *)
type stat_modifier = int * float

type pokemon_stat_modifier = {mutable attack: stat_modifier; mutable defense: stat_modifier;
    mutable speed: stat_modifier; mutable special_attack: stat_modifier;
    mutable special_defense: stat_modifier; mutable evasion: stat_modifier;
    mutable accuracy: stat_modifier}

type trainer_team = {mutable current: battle_poke; mutable alive: battle_poke list; mutable dead:
                        battle_poke list; mutable stat_enhance: pokemon_stat_modifier}

(* As in competitive Pokemon, there will be no ties. If all Poke die in
one turn, the one with the Poke standing the latest wins *)
type outcome = WinnerP1 | WinnerP2

type battle_mode = Random1p

type screen = SwitchPoke | ChooseMove | Faint | BothFaint

type battle_state = InGame of trainer_team * trainer_team * weather_terrain * playerMove ref * playerMove ref | Loading | P1 of screen| P2 of screen | Processing

type game_state = MainMenu | Menu1P | Quit | Battle of battle_state