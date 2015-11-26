open Info

(*This module is responsible for getting valid pokemon *)

val getRandomPokemon: unit -> pokemon

val getPokeToolTip: trainer_team -> string

val getMoveToolTip: move -> string

val getElementEffect: element -> element -> float

val getTestPoke: unit -> pokemon