open Async.Std
open Info
open Pokemon

let number_turns = ref 0

let initialize_battle team1 team2 = Battle (InGame (team1, team2, ClearSkies))

let getBattlePoke poke = {pokeinfo = poke; curr_hp = ref poke.hp; curr_status =
                          ref (NoNon, [NoVola]); curr_item = ref poke.item}

let getRandomTeam () = {current = (getBattlePoke(getRandomPokemon ()));
                       dead =[]; alive = (List.map getBattlePoke
                       (List.map getRandomPokemon [();();();();()]))}

let handle_preprocessing state = state

let handle_action state action1 action2 = state

let rec main_controller_random1p engine gui_ready =
  let () = Thread.delay 1. in
  let team1 = getRandomTeam () in
  let team2 = getRandomTeam () in
  let battle = initialize_battle team1 team2 in
  engine := Ivar.create (); Ivar.fill !engine battle;
  let rec main_loop () =
    incr number_turns;
    while (Ivar.is_full !gui_ready) do
      ()
    done in
  ()

let initialize_controller (engine, battle_engine) =
  let battle_status, gui_ready = battle_engine in
    Printf.printf "Initializing battle\n%!";
  upon (Ivar.read !battle_status) (fun s -> match s with
    | Random1p -> (main_controller_random1p engine gui_ready));
  ()

