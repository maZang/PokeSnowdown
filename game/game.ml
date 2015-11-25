(*
COMPILE WITH:
ocamlfind ocamlc -g -thread -package lablgtk2 -package async -package yojson -linkpkg info.mli data_loader.mli gui.mli pokemon.mli battle_controller.mli game.mli gui.ml pokemon.ml battle_controller.ml game.ml  -o game
*)

open Async.Std
open Info
let current_state = ref (Ivar.create ())
let battle_mode = (ref (Ivar.create ()), ref (Ivar.create ()), ref (Ivar.create ()), ref (Ivar.create ()))
let number_loops = ref 0

let quit thread_lst =
	let _ = List.map Thread.kill thread_lst in Thread.exit ()

let give_gui_permission () = current_state := Ivar.create ()

let wait_for_command () =
  Printf.printf "Currently waiting for command\n%!";
	while Ivar.is_empty !current_state do
		Thread.yield ()
  done

let wait_for_empty () =
  Printf.printf "Currently waiting for empty\n%!";
	while (Ivar.is_full !current_state) do
		Thread.yield ()
	done

let main () =
	Printf.printf "Starting game\n%!";
	let _ = Gui.main_gui current_state battle_mode () in
	let rec game_loop () =
		incr number_loops;
	  	Printf.printf "Number game loops: %d\n%!" !number_loops;
		wait_for_command ();
		upon (Ivar.read !current_state) (fun state ->
			match state with
			| MainMenu -> Printf.printf "Loading Main Menu\n%!"; give_gui_permission (); game_loop ()
			| Menu1P -> Printf.printf "Loading One Player \n%!"; give_gui_permission (); game_loop ()
      | Battle Loading -> Printf.printf "Initializing battle controller\n%!";
                    Battle_controller.initialize_controller
                    (current_state, battle_mode); let _, gui_ready, ready, _ = battle_mode in upon
                    (Ivar.read !ready) (fun _ -> ready := Ivar.create (); game_loop ())

			| Quit -> Printf.printf "Quitting\n%!"
      | _ -> failwith "WTF"
		)
	in game_loop ()

let _ = main ()

let _ = Scheduler.go ()