(* COMPILE COMMANDS (bottom one is for executable file):
ocamlfind ocamlc -g -thread -package lablgtk2 -package async -package yojson -package str -linkpkg info.mli pokemon.mli gui.mli battle_controller.mli game.mli pokemon.ml ai.ml save.ml tournament.ml gui.ml battle_controller.ml game.ml -o game

OR

cs3110 compile -thread -p lablgtk2,async,yojson,str game.ml


ocamlfind ocamlc -g -thread
 -package lablgtk2 -package async -package yojson -package str
 -linkpkg info.mli pokemon.mli gui.mli battle_controller.mli game.mli pokemon.ml
          ai.ml gui.ml battle_controller.ml game.ml
 -custom -o game.exe
*)
open Async.Std
open Info

(* current state holds the current state of the game *)
let current_state = ref (Ivar.create ())
(* battle mode is of type (a, b, c, d) of four different ivars.
  -- a is the Ivar that contains the current game mode
  -- b is the Ivar that contains the state of the gui
  -- c is the Ivar that tells whether or not the battle is over
  -- d is the Ivar that says if the gui is ready after a battle "animation"
*)
let battle_mode = (ref (Ivar.create ()), ref (Ivar.create ()),
  ref (Ivar.create ()), ref (Ivar.create ()))

(* Number loops is the number of game loops. A full battle counts as one loop *)
let number_loops = ref 0

(* Called to allow gui to process another comand *)
let give_gui_permission () = current_state := Ivar.create ()

(* The main game function. [main ()] initializes the gui and passes the
   current_state reference as well as the battle_mode reference to the gui. It
   then processess commands based upon gui input.
*)
let main () =
	Printf.printf "Starting game\n%!";
  (* Creates the gui *)
	let _ = Gui.main_gui current_state battle_mode () in
  (* Main game loop as well as call-back function *)
	let rec game_loop () =
		incr number_loops;
	  Printf.printf "Number game loops: %d\n%!" !number_loops;
    (* Callback used to call the main game loop when command is registered *)
		upon (Ivar.read !current_state) (fun state ->
			match state with
			| MainMenu -> Printf.printf "Loading Main Menu\n%!";
                    give_gui_permission (); game_loop ()
			| Menu1P -> Printf.printf "Loading One Player \n%!";
                    give_gui_permission (); game_loop ()
      | Menu2P -> Printf.printf "Loading Two Player \n%!";
                    give_gui_permission (); game_loop ()
      | Menu0P -> Printf.printf "Loading No Player \n%!";
                    give_gui_permission (); game_loop ()
      | Battle Loading -> Printf.printf "Initializing battle controller\n%!";
                    (* Initialize battle controller *)
                    Battle_controller.initialize_controller
                    (current_state, battle_mode);
                    (* Only call game loop when battle ends *)
                    let _, _, ready, _ = battle_mode in
                    upon (Ivar.read !ready)
                      (fun _ -> ready := Ivar.create (); game_loop ())
      (* Quit Code *)
			| Quit -> Printf.printf "Quitting\n%!"; ignore (exit 0)
      | _ -> failwith "Faulty Game Logic: Debug 51"
		)
	in game_loop ()

(* Call Main Function *)
let _ = main ()

(* Allow Scheduler to Run Call Backs*)
let _ = Scheduler.go ()
