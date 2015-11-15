(* Info contains all the data types needed for the handling of the game *)

type game_state = MainMenu | Menu1P | Quit | Battle

type battlemove = SwitchPoke of int | UseAttack of int

type target = SpecificPoke | SelectedPokeMeFirst | Ally | UsersField |
    UserOrAlly | OpponentsFields | User | RandomOpp | AllOthers |
    SelectedPoke | AllOpp | EntireField | UserAndAlly | All

type dmg_class = Physical | Special | Status

type element = Fire | Water | Grass | Rock | Ground | Fairy | Dark | Electric |
  Ghost | Steel | Normal | Bug | Flying | Psychic | Ice | Dragon | Fighting |
  Poison

type move = {name:string; priority: int; target: target; dmg_class: dmg_class;
    power:int; effect_chance: int; accuracy: int; element: element;
    description: string}

type pokemon = {element: element list; moves: move list; name: string;
  hp: int; attack: int; defense:int; special_defense:int; special_attack:int;
  speed:int; ability:string}

(* variants containing all secondary effects of a given move *)
type secondary_effects

(* We hope to implement all of these below but we will see *)
type weather_terrain = HarshSun | Hail | Rain | SandStorm
	| HeavyRain | Sun | AirCurrent | ClearSkies

type non_volatile_status = Burn | Freeze | Paralysis | Poison | Toxic | Sleep

type volatile_status =  Confusion | Curse | Embargo | Flinch | HealBlock
	| Identification | Infatuation | Nightmare | Trapped | PerishSong | Leeched
	| Taunt | Levitate | Torment

type status = non_volatile_status * volatile_status list

(* stat modifier represents number of stages followed by multiplier *)
type stat_modifier = int * float

type pokemon_stat_modifier = {attack: stat_modifier; defense: stat_modifier;
    speed: stat_modifier; special_attack: stat_modifier;
    special_defense: stat_modifier; evasion: stat_modifier;
    accuracy: stat_modifier}

type trainer_team = {current: pokemon; alive: pokemon list; dead: pokemon list}

type battle_state = trainer_team * trainer_team * weather_terrain

(* As in competitive Pokemon, there will be no ties. If all Poke die in
one turn, the one with the Poke standing the latest wins *)
type outcome = WinnerP1 | WinnerP2