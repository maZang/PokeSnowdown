(* We predict most of our Pokemon data will be held in Json files. To improve
modularity, we are going to make this module turn Pokemon into data usable
by the game. We are not quite sure what other methods are needed yet, but 
we know that this module will be pretty useful. 
*)

val get_pokemon: string -> Info.pokemon 