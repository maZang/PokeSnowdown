open Info

(* This module is responsible for getting valid Pokemon. *)

(* item list is a list of valid items for a pokemon *)
val item_list: string list

(* nature list is a list of valid natures for a pokemon *)
val nature_list: string list

(* gets the unlocked pokemon json save file *)
val unlocked_pokemon: unit -> Yojson.Basic.json

(* [unlocked_poke_string_list ()] generates a string list of Pokemon by reading
 *  data from a JSON file.
 *)
val unlocked_poke_string_list : unit -> string list

(* [generatePokemon str] generates the pokemon [str] with random attributes*)
val generatePokemon: string -> pokemon

(* [getRandomPokemon ()] returns a random Pokemon, filled with its own abilities
 *  and initialized with the appropriate characteristics (moves, etc.).
 *)
val getRandomPokemon : unit -> pokemon

(* [getPresetPokemon str] returns an unlocked pokemon, filled with its own
 *   abilities and initialized with the appropriate characteristics
 * (moves, etc.), loading from a json file
 *)
val getPresetPokemon : ?pjson:Yojson.Basic.json -> string -> pokemon

(* [getRandomPreset str] returns a random unlocked pokemon, filled with its own
 *   abilities and initialized with the appropriate characteristics
 * (moves, etc.), loading from a json file
 *)

val getRandomPreset : ?pjson:Yojson.Basic.json -> unit -> pokemon

(* Turns the nature variant to a string
*)
val string_of_nature: nature -> string

(* Turns the item variant to a string
*)
val string_of_item: item -> string

(* [getPokeToolTip t] returns a string containing relevant tool-tip information
 *  (stats, etc.) for team t's current Pokemon.
 *
 *  - [t] is the trainer team whose current Pokemon's information is returned.
 *)
val getPokeToolTip : trainer_team -> string

(* [getMoveToolTip move] returns a string containing relevant tool-tip
 *  information (what the move does, etc.) for the specified Pokemon move.
 *
 *  - [move] is the name of the Pokemon move whose help information is returned.
 *)
val getMoveToolTip : move -> string

(* [string_of_weather w] returns a string describing the weather state w.
 *
 *  - [w] is the current weather state.
 *)
val string_of_weather : weather -> string

(* [getElementEffect elm1 elm2] returns a float describing the effect of elm1 on
 *  on a Pokemon of element type elm2. This could be a standard effect or a
 *  super effect (e.g. fire against water, etc.).
 *
 *  - [elm1] is the element type of the move made by the Pokemon.
 *  - [elm2] is the element type of the Pokemon itself.
 *)
val getElementEffect : element -> element -> float

(* [getTestPoke ()] returns a test Pokemon used as a reference implementation
 *  when trying out new features.
 *)
val getTestPoke : unit -> pokemon

(* [getTestOpp ()] returns a test opponent Pokemon used as a reference
 *  implementation when trying out new features. Similar to the test Pokemon but
 *  with different moves/abilities to keep things interesting.
 *)
val getTestOpp : unit -> pokemon

(* [getMoveFromString str] converts the string str to a corresponding Pokemon
 *  move.
 *
 *  - [str] is the name of the Pokemon move, specified as a string.
 *)
val getMoveFromString : string -> move

(* [string_of_element elm] returns a string describing the Pokemon element type.
 *
 *  - [elm] is a Pokemon element type (e.g. fire, grass, etc.).
 *)
val string_of_element : element -> string

(* [getAllMoves poke] returns a move list containing all the moves a Pokemon
*  can learn
*)
val getAllMoves : string -> string list

(* [getAllAbilities] returns a string list containing all the abilities a
* Pokemon can have *)
val getAllAbilities: string -> string list