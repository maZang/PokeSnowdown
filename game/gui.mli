(* This is the mli file for the gui. Only one function is visible to the
outside world and that is the initialization of the gui by the
game controller. Since the game controller and the gui cannot
cross communicate by accessing each other's methods (since that results
in a circular dependency, we use the filling of an Ivar reference for
communication. Gui.ml fills up an Ivar after an action/callback has taken
place, and Game.ml reads that Ivar after it has been filled. During the battle
phase, communication between the game controller and gui is paused and
communication between the battle controller and gui is started. The gui is
written using Lablgtk2*)

open Async.Std

(* The main_gui function takes in an extra unit argument since the Gtk
package does this a lot to separate full function application from
partial function application due to a multitude of optional
arguments *)
val main_gui: Info.game_state Ivar.t ref -> Info.battle_mode Ivar.t ref * (Info.battlemove option * Info.battlemove option) Ivar.t ref * bool Ivar.t ref * bool Ivar.t ref -> unit -> unit