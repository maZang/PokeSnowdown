open Info
open Async.Std
(* 
	The main game has two main functions. It has to accept action requests from 
	the players and then return the current game status. 
	. Besides handling the state of the game (which is the main
	function), it's secondary function is sending updates to the GUI to display
	the current state of the game. Not much of game.ml has to be visible, since 
	nothing depends on it. 
*)

val current_state: game_state Ivar.t ref 

val quit: Thread.t list -> unit 