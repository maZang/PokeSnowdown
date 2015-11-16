open Async.Std
open Info

let number_turns = ref 0

let initialize_battle team1 team2 = Battle (InGame (team1, team2, ClearSkies))

let handle_preprocessing state = state

let handle_action state action1 action2 = state

let rec main_controller_random1p engine gui_ready =
  Thread.delay 5.;
  let battle = initialize_battle 0 0 in
  engine := Ivar.create (); Ivar.fill !engine battle;
  let rec main_loop () =
    incr number_turns;
    while (Ivar.is_full !gui_ready) do
      ()
    done in
  ()

let initialize_controller (engine, battle_engine) =
  let battle_status, gui_ready = battle_engine in
  upon (Ivar.read !battle_status) (fun s -> match s with
    | Random1p -> (main_controller_random1p engine gui_ready));
  ()