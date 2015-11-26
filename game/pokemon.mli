open Info

(*This module is responsible for getting valid pokemon *)

val getRandomPokemon: unit -> pokemon

val getPokeToolTip: battle_poke -> string

val getMoveToolTip: move -> string

val getElementEffect: element -> element -> float