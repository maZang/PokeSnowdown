open Async.Std
(* The game controller passes control to the battle controller during the
battle phases of the game. This will contain all the methods that involves
damage calculations/checking win condition/checking for legality of moves/
etc... It will most likely contain its own Ivar for communication between
the gui and the controller. *)

(* Initialize the battle controller *)
val initialize_controller: Info.game_state Ivar.t ref * (Info.battle_mode Ivar.t ref * (Info.battlemove option * Info.battlemove option) Ivar.t ref * bool Ivar.t ref) -> unit
(* Initializes the battle; game thread calls battle controller thread and suspends
until battle thread completes *)
val initialize_battle: Info.trainer_team -> Info.trainer_team -> Info.game_state

(* Handles the processing at the end of every turn and calculates burn/poision/
weather damage before the next battle turn begins *)
val handle_preprocessing: Info.game_state -> Info.game_state

(* This handles the game after two players have moved *)
val handle_action: Info.game_state -> Info.battlemove -> Info.battlemove -> Info.game_state