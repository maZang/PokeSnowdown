(* Info contains all the data types needed for the handling of the game *)
type battlemove = Poke of string | UseAttack of string | NoMove | FaintPoke of string | Preprocess | TurnEnd

type guiattack = NormMove of string | Crit of string | SEffCrit of string |
                  SEff of string | NoEffCrit of string | NoEff of string

type guimove = SPoke of string | Attack of guiattack | Flinch | Faint | NoAction | Continue | Next

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

type move = {name:string; priority: int; target: target; dmg_class: dmg_class;
    power:int; effect_chance: int; accuracy: int; element: element;
    description: string}

type item = Nothing | Leftovers | ChoiceBand | LifeOrb | CharizarditeX |
            ChoiceSpecs

(* variants containing all secondary effects of a given move *)
type secondary_effects

(* We hope to implement all of these below but we will see *)
type weather_terrain = HarshSun | Hail | Rain | SandStorm
	| HeavyRain | Sun | AirCurrent | ClearSkies

type non_volatile_status = Burn | Freeze | Paralysis | Poison | Toxic | Sleep |
                          NoNon

type volatile_status =  Confusion | Curse | Embargo | Encore | Flinch | HealBlock
	| Identification | Infatuation | Nightmare | Trapped | PerishSong | Leeched
	| Taunt | Levitate | Torment | NoVola

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
                        battle_poke list; stat_enhance: pokemon_stat_modifier}

(* As in competitive Pokemon, there will be no ties. If all Poke die in
one turn, the one with the Poke standing the latest wins *)
type outcome = WinnerP1 | WinnerP2

type battle_mode = Random1p

type screen = SwitchPoke | ChooseMove | Faint

type battle_state = InGame of trainer_team * trainer_team * weather_terrain ref * playerMove ref * playerMove ref | Loading | P1 of screen| P2 of screen | Processing

type game_state = MainMenu | Menu1P | Quit | Battle of battle_state