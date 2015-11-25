open Async.Std
open Info
open Pokemon

let get_game_status engine =
  match Deferred.peek (Ivar.read !engine) with
  | Some v -> v
  | None -> failwith "Faultly game logic"

let unpack opt =
  match opt with
  | Some v -> v
  | None -> failwith "Revisit game logic"

let initialize_battle team1 team2 = Battle (InGame (team1, team2, ref ClearSkies, ref (Pl1 NoMove), ref (Pl2 NoMove)))

let getBattlePoke poke = {pokeinfo = poke; curr_hp = ref poke.hp; curr_status =
                          ref (NoNon, [NoVola]); curr_item = ref poke.item}

let getRandomTeam () = {current = (getBattlePoke(getRandomPokemon ()));
                       dead =[]; alive = (List.map getBattlePoke
                       (List.map getRandomPokemon [();();();();()]))}

let findBattlePoke lst name =
  let rec helper acc lst =
    match lst with
    | [] -> failwith "Faulty Game Logic"
    | h::t -> if h.pokeinfo.name = name then h, (acc @ t) else helper (h::acc) t in
  helper [] lst

let handle_preprocessing state = state

let handle_action state action1 action2 =
  let t1, t2, w, m1, m2 = match get_game_status state with
    | Battle InGame (t1, t2, w, m1, m2) -> t1, t2, w, m1, m2
    | _ -> failwith "Fauly Game Logic" in
  match action1 with
  | Poke p ->
      (match action2 with
      | Poke p -> failwith "unimplemented"
      | UseAttack a -> failwith "unimplemented"
      (* Later change it so that None becomes a bot move *)
      | NoMove -> let prevPoke = t1.current in
                  let switchPoke, restPoke = findBattlePoke t1.alive p in
                  t1.current <- switchPoke; t1.alive <- prevPoke::restPoke;
                  m1 := Pl1 (Poke p); m2:= Pl2 NoMove )
  | UseAttack a -> failwith "unimplemented"
  | NoMove -> failwith "unimplemented"

let rec main_loop_1p engine gui_ready ready ready_gui () =
  upon (Ivar.read !gui_ready) (* Replace NoMove with ai move later *)
    (fun (cmd1, cmd2) -> let c1 = unpack cmd1 in let c2 = NoMove in
                         let () = handle_action engine c1 c2 in
                         gui_ready := Ivar.create ();
                         Ivar.fill !ready_gui true;
                         (main_loop_1p engine gui_ready ready ready_gui ()));
   Printf.printf "Debug %d \n%!" (Scheduler.cycle_count ())

let rec main_controller_random1p engine gui_ready ready ready_gui=
  let team1 = getRandomTeam () in
  let team2 = getRandomTeam () in
  let battle = initialize_battle team1 team2 in
  let () = engine := Ivar.create (); Ivar.fill !engine battle in
  main_loop_1p engine gui_ready ready ready_gui ()

let initialize_controller (engine, battle_engine) =
  let battle_status, gui_ready, ready, ready_gui = battle_engine in
    Printf.printf "Initializing battle\n%!";
  upon (Ivar.read !battle_status) (fun s -> match s with
    | Random1p -> (main_controller_random1p engine gui_ready ready ready_gui));
  ()