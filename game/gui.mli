open Async.Std
(* This is the interface for the GUI. Like [game.mli], only one function needs
 *  to be visible to the outside world; the initialization of the GUI by the
 *  game controller. Since the game controller and the gui cannot cross-
 *  communicate by accessing each other's methods due to our modular MVC-style
 *  design (it also results in a forbidden circular dependency), we use the
 *  filling of an Ivar reference for communication. The GUI fills up an Ivar
 *  after an action/callback has taken place, and the game engine reads that
 *  Ivar after it has been filled. During the battle phase, communication
 *  between the game controller and GUI is paused and communication between the
 *  battle controller and GUI is started. The GUI is written using labgtk2.
 *)

(* See [gui.ml] for a more complete documentation. *)

(* [main_gui engine battle_engine ()] is the main GUI for the game.
 *
 *  - [engine] is the game's engine and underlying state.
 *  - [battle_engine] is the current battle mode/commands.
 *
 *  It takes in an extra unit argument since the gtk+ package does this a lot to
 *  separate full function application from partial function application due to
 *  many optional arguments.
 *)
val main_gui : Info.game_state Ivar.t ref -> Info.battle_mode Ivar.t ref *
  (Info.battlemove option * Info.battlemove option) Ivar.t ref * bool Ivar.t ref
   * bool Ivar.t ref -> unit -> unit