open Info

(*This module is responsible for getting valid pokemon *)
val unlocked_poke_string_list: unit -> string list

val getRandomPokemon: unit -> pokemon

val getPokeToolTip: trainer_team -> string

val getMoveToolTip: move -> string

val string_of_weather: weather -> string

val getElementEffect: element -> element -> float

val getTestPoke: unit -> pokemon

val getTestOpp: unit -> pokemon

val getMoveFromString: string -> move

val string_of_element: element -> string