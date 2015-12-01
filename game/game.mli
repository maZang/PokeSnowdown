open Info
open Async.Std

(* The game has two main functions: (i) it has to accept action requests from
 *  the players and then return the current game status (i.e. handle the state
 *  of the game), and (ii) sending updates to the GUI to display the current
 *  state of the game. Not much of game.ml has to be visible, since nothing
 *  actually depends on it. As a result, the interface is minimal.
 *)

(* See [game.ml] for a more complete documentation. *)

(* [current_state] is eponymous; it holds the current state of the game. *)
val current_state : game_state Ivar.t ref
