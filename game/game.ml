(*
COMPILE WITH:
ocamlfind ocamlc -g -thread -package lablgtk2 -package async -linkpkg info.mli data_loader.mli gui.mli battle_controller.mli game.mli gui.ml game.ml  -o game
*)

open Async.Std
open Info

let current_state = ref (Ivar.create ())
let number_loops = ref 0

let quit thread_list =
	let _ = List.map Thread.kill thread_list in ignore (exit 0)

let give_gui_permission () = current_state := Ivar.create ()

let wait_for_command () =
	while Ivar.is_empty !current_state do
		()
	done

let wait_for_empty () =
	while (Ivar.is_full !current_state) do
		()
	done

let main () =
	let scheduler_thread = Thread.create Scheduler.go () in
	let gui_thread = Thread.create (Gui.main_gui current_state) () in
	let rec game_loop () =
		incr number_loops;
	  Print.printf "Number game loops: %d\n%!" !number_loops;
		wait_for_command ();
		upon (Ivar.read !current_state) (fun state ->
			match state with
			| MainMenu -> give_gui_permission ()
			| Menu1P -> give_gui_permission ()
			| Quit -> quit [gui_thread; scheduler_thread]
		); wait_for_empty (); game_loop ()
	in game_loop ()

let _ = main ()