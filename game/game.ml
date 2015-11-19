(*
COMPILE WITH:
ocamlfind ocamlc -g -thread -package lablgtk2 -package async -package yojson -linkpkg info.mli data_loader.mli gui.mli pokemon.mli battle_controller.mli game.mli gui.ml pokemon.ml battle_controller.ml game.ml  -o game
*)

open Async.Std
open Info

let current_state = ref (Ivar.create ())
let battle_mode = (ref (Ivar.create ()), ref (Ivar.create ()))
let number_loops = ref 0

let quit thread_lst =
	let _ = List.map Thread.kill thread_lst in Thread.exit ()

let give_gui_permission () = current_state := Ivar.create ()

let wait_for_command () =
  Printf.printf "Currently waiting for command\n%!";
	while Ivar.is_empty !current_state do
		()
  done

let wait_for_empty () =
  Printf.printf "Currently waiting for empty\n%!";
	while (Ivar.is_full !current_state) do
		()
	done

let main () =
	Printf.printf "Starting game\n%!";
	let scheduler_thread = Thread.create Scheduler.go () in
	let gui_thread = Thread.create (Gui.main_gui current_state battle_mode) () in
	let rec game_loop () =
		incr number_loops;
	  	Printf.printf "Number game loops: %d\n%!" !number_loops;
		wait_for_command ();
		upon (Ivar.read !current_state) (fun state ->
			match state with
			| MainMenu -> give_gui_permission ()
			| Menu1P -> give_gui_permission ()
      | Battle _ ->   Printf.printf "Initializing battle controller\n%!"; let battle_controller = Thread.create
                    Battle_controller.initialize_controller
                    (current_state, battle_mode) in Thread.join
                    battle_controller; ()
			| Quit -> Printf.printf "Quitting\n%!"; quit [gui_thread; scheduler_thread]
		); wait_for_empty (); game_loop ()
	in game_loop ()

let _ = main ()