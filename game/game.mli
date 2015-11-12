open Info
open Async.Std
(* 
	The main game has two main functions. It has to accept action requests from 
	the players and then return the current game status. It has to process the
	current state of the game and handle the preprocessing before the end of a
	turn (poison damage, weather damage, burn damage, 
	defrost, etc...). Besides handling the state of the game (which is the main
	function), it's secondary function is sending updates to the GUI to display
	the current state of the game.
*)

(* This is preprocessing done at the start of the game (waking Pokemon up,
breaking confusion, ending weather effects *)
val current_state: game_state Ivar.t ref 

val handle_preprocessing: game_state -> game_state

val handle_action: game_state -> cmd -> cmd -> game_state 

val quit: Thread.t list -> unit 