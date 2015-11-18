(* Info contains all the data types needed for the handling of the game *)
type battlemove = SwitchPoke of int | UseAttack of int

type target = SpecificPoke | SelectedPokeMeFirst | Ally | UsersField |
    UserOrAlly | OpponentsFields | User | RandomOpp | AllOthers |
    SelectedPoke | AllOpp | EntireField | UserAndAlly | All

type dmg_class = Physical | Special | Status

type element = Fire | Water | Grass | Rock | Ground | Fairy | Dark | Electric |
  Ghost | Steel | Normal | Bug | Flying | Psychic | Ice | Dragon | Fighting |
  Poison

type evs = {attack:int; defense:int; special_attack: int; special_defense: int;
            hp:int; speed:int}

type move = {name:string; priority: int; target: target; dmg_class: dmg_class;
    power:int; effect_chance: int; accuracy: int; element: element;
    description: string}

type item = None | Leftovers | ChoiceBand | LifeOrb | CharizarditeX |
            ChoiceSpecs

(* variants containing all secondary effects of a given move *)
type secondary_effects

(* We hope to implement all of these below but we will see *)
type weather_terrain = HarshSun | Hail | Rain | SandStorm
	| HeavyRain | Sun | AirCurrent | ClearSkies

type non_volatile_status = Burn | Freeze | Paralysis | Poison | Toxic | Sleep

type volatile_status =  Confusion | Curse | Embargo | Encore | Flinch | HealBlock
	| Identification | Infatuation | Nightmare | Trapped | PerishSong | Leeched
	| Taunt | Levitate | Torment

type status = non_volatile_status * volatile_status list

type nature = Adamant | Modest | Timid | Careful

type pokemon = {name: string; element: element list; move1: string; move2: string;
  move3: string; move4: string; hp: int; attack: int; defense:int;
  special_defense:int; special_attack:int; speed:int; ability:string; evs: evs;
  nature: nature; item: item}

type battle_poke = {pokeinfo: pokemon; curr_hp:int ref; curr_status: status ref;
  curr_item: item ref}

(* stat modifier represents number of stages followed by multiplier *)
type stat_modifier = int * float

type pokemon_stat_modifier = {attack: stat_modifier; defense: stat_modifier;
    speed: stat_modifier; special_attack: stat_modifier;
    special_defense: stat_modifier; evasion: stat_modifier;
    accuracy: stat_modifier}

type trainer_team = int
(*  {current: pokemon; alive: pokemon list; dead: pokemon list} *)

(* As in competitive Pokemon, there will be no ties. If all Poke die in
one turn, the one with the Poke standing the latest wins *)
type outcome = WinnerP1 | WinnerP2

type battle_mode = Random1p

type battle_state = InGame of trainer_team * trainer_team * weather_terrain | Loading

type game_state = MainMenu | Menu1P | Quit | Battle of battle_state