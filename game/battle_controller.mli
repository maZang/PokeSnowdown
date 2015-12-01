open Async.Std
(* The game controller passes control to the battle controller during the battle
 *  phases of the game. This will contain all the methods that involves damage
 *  calculations/checking win condition/checking for legality of moves/etc... It
 *  will most likely contain its own Ivar for communication between the GUI and
 *  the controller. This is for an MVC-style interface between modules handling
 *  the user experience.
 *)

(* See [battle_controller.ml] for a more complete documentation. *)

(* [initialize_controller (engine, battle_engine)] initializes the battle
 *  controller.
 *
 *  - [engine] is the game's engine and underlying state.
 *  - [battle_engine] is the current battle mode/commands.
 *)
val initialize_controller : Info.game_state Ivar.t ref *
  (Info.battle_mode Ivar.t ref * (Info.battlemove option*Info.battlemove option)
   Ivar.t ref * bool Ivar.t ref * bool Ivar.t ref) -> unit

(* [initialize_battle team1 team2] initializes the battle. The game asynchronous
 *  variables interact with the asynchronous variables that handle the battle
 *  controller and suspend until battle completes.
 *
 *  - [team1] is the first trainer team engaging in the battle.
 *  - [team2] is the second trainer team engaging in the battle.
 *)
val initialize_battle: Info.trainer_team -> Info.trainer_team -> Info.game_state

(* [handle_preprocessing t1 t2 w m1 m2] handles the processing at the end of
 *  every turn and calculates burn/poison/weather damage before the next battle
 *  turn begins.
 *
 *  - [t1] is the first trainer team.
 *  - [t2] is the second trainer team.
 *  - [w] contains the weather and terrain of the battle scene.
 *  - [m1] is Team 1's current Pokemon's move.
 *  - [m2] is Team 2's current Pokemon's move.
 *)
val handle_preprocessing : Info.trainer_team -> Info.trainer_team ->
  Info.weather_terrain -> Info.playerMove ref -> Info.playerMove ref -> unit

(* [handle_action state action1 action2] handles the game after two players have
 *  moved. This is the primary action handler of the entire game.
 *
 *  - [state] is the current game state.
 *  - [action1] is the action performed by Team 1 in the GUI.
 *  - [action2] is the action performed by Team 2 in the GUI.
 *)
val handle_action : Info.game_state Ivar.t ref -> Info.battlemove ->
  Info.battlemove -> unit