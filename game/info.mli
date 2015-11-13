(* Info contains all the data types needed for the handling of the game *)

type game_state = MainMenu | Menu1P | Quit 

type battlemove = SwitchPoke of int | UseAttack of int 

(* *)
type move = string 

type pokemon 

(* variants containing all secondary effects of a given move *)
type secondary_effects 

(* most likely also a record with also a list of secondary_effects*)
type attack 

(* We hope to implement all of these below but we will see *)
type weather_terrain = HarshSun | Hail | Rain | SandStorm 
	| HeavyRain | Sun | AirCurrent | ClearSkies 

type non_volatile_status = Burn | Freeze | Paralysis | Poison | Toxic | Sleep 

type volatile_status =  Confusion | Curse | Embargo | Flinch | HealBlock 
	| Identification | Infatuation | Nightmare | Trapped | PerishSong | Leeched 
	| Taunt | Levitate | Torment 

type status = non_volatile_status * volatile_status list 

type pokemon_type 

type item 

type pokemon_stat_modifier 

type trainer_team

type battle_state = trainer_team * trainer_team * weather_terrain 

(* As in competitive Pokemon, there will be no ties. If all Poke die in 
one turn, the one with the Poke standing the latest wins *)
type outcome = WinnerP1 | WinnerP2