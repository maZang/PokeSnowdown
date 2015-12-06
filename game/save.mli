open Pokemon
open Info

exception FaultyGameSave
exception BadFieldOption
exception BadEVInput
exception OwnPokemonAlready

(* [updatePokemonFields pokename move1 move2 move3 move4 ability nature] updates
 *  the pokemon fields for the save file and Pokemon editor.
 *)
val updatePokemonFields :

(* [createSavePokeEdit pokename move1 move2 move3 move4 ability nature item]
 *  creates the save file for pokedit. *)
val createSavePokeEdit : 

(* [addToUnlocked pokename lst] adds a pokemon to the unlocked list. *)
val addToUnlocked :

(* [incPrevSave key lst] is used for incrementing your saves (such as number of
 *  times beat Professor Oak, etc...). *)
val incPrevSave :

(* [incPlayOak ()] increments the number of times you play Professor Oak. *)
val incPlayOak :

(* [addPoke beat str] adds a Pokemon to your save file and increments the amount
 *  of times you beat someone. *)
val addPoke :

(* [getFileMessage ()] gets the stat message to be printed out. *)
val getFileMessage :

(* [beat_game ()] returns true if you beat the game. *)
val beat_game :
