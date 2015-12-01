open Info

(* This module is responsible for getting valid Pokemon. *)

(* [unlocked_poke_string_list ()] generates a string list of Pokemon by reading
 *  data from a JSON file.
 *)
val unlocked_poke_string_list : unit -> string list

(* [getRandomPokemon ()] returns a random Pokemon, filled with its own abilities
 *  and initialized with the appropriate characteristics (moves, etc.).
 *)
val getRandomPokemon : unit -> pokemon

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

(*  *)
val string_of_weather : weather -> string

(*  *)
val getElementEffect : element -> element -> float

(*  *)
val getTestPoke : unit -> pokemon

(*  *)
val getTestOpp : unit -> pokemon

(*  *)
val getMoveFromString : string -> move

(*  *)
val string_of_element : element -> string