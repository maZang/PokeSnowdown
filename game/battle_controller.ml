open Async.Std
open Info
open Pokemon

let pokeIV = 31

let get_game_status engine =
  match Deferred.peek (Ivar.read !engine) with
  | Some v -> v
  | None -> failwith "Faultly game logic"

let unpack opt =
  match opt with
  | Some v -> v
  | None -> failwith "Revisit game logic"

let initialize_battle team1 team2 = Battle (InGame (team1, team2, ref ClearSkies, ref (Pl1 NoMove), ref (Pl2 NoMove)))

let getBattlePoke poke =
  let bhp = (2 * poke.hp + pokeIV + poke.evs.hp / 4) + 100 + 10 in
  let battack = int_of_float ((match poke.nature with
    | Lonely | Adamant | Naughty | Brave -> 1.1
    | Bold | Modest | Calm | Timid -> 0.9
    | _ -> 1.0) *.  float_of_int (2 * poke.attack + pokeIV + poke.evs.attack / 4 + 5)) in
  let bdefense = int_of_float ((match poke.nature with
    | Bold | Impish | Lax | Relaxed -> 1.1
    | Lonely | Mild | Gentle | Hasty -> 0.9
    | _ -> 1.0) *.  float_of_int (2 * poke.defense + pokeIV + poke.evs.defense / 4 + 5)) in
  let bspecial_attack = int_of_float ((match poke.nature with
    | Modest | Mild | Rash | Quiet -> 1.1
    | Adamant | Impish | Careful | Jolly -> 0.9
    | _ -> 1.0) *.  float_of_int (2 * poke.special_attack + pokeIV + poke.evs.special_attack / 4 + 5)) in
  let bspecial_defense = int_of_float ((match poke.nature with
    | Calm | Gentle | Careful | Sassy -> 1.1
    | Naughty | Lax | Rash | Naive -> 0.9
    | _ -> 1.0) *.  float_of_int (2 * poke.special_defense + pokeIV + poke.evs.special_defense / 4 + 5)) in
  let bspeed = int_of_float ((match poke.nature with
    | Timid | Hasty | Jolly | Naive -> 1.1
    | Brave | Relaxed | Quiet | Sassy -> 0.9
    | _ -> 1.0) *.  float_of_int (2 * poke.speed + pokeIV + poke.evs.speed / 4 + 5)) in
  {pokeinfo = poke; curr_hp = bhp; curr_status = (NoNon, [NoVola]); curr_item = poke.item; bhp; battack; bdefense; bspecial_attack; bspecial_defense; bspeed}

let getRandomTeam () =
  let stat_enhance = {attack=(0,1.); defense=(0,1.); speed=(0,1.);
      special_attack=(0,1.); special_defense=(0,1.); evasion=(0,1.);
      accuracy=(0,1.)} in
  {current = (getBattlePoke(getRandomPokemon ()));
  dead =[]; alive = (List.map getBattlePoke
  (List.map getRandomPokemon [();();();();()])); stat_enhance}

(* let damageCalculation t1 t2 move =
  let defense = match move.dmg_class with
    | Physical -> t2.current.bdefense
    | Special -> t2.current.bspecial_defense in *)

let findBattlePoke lst name =
  let rec helper acc lst =
    match lst with
    | [] -> failwith "Faulty Game Logic"
    | h::t -> if h.pokeinfo.name = name then h, (acc @ t) else helper (h::acc) t in
  helper [] lst

let findBattleMove poke move =
  if (poke.move1.name = move) then
    poke.move1
  else if (poke.move2.name = move) then
    poke.move2
  else if (poke.move3.name = move) then
    poke.move3
  else if (poke.move4.name = move) then
    poke.move4
  else
    failwith "Faulty Game Logic: Debug 16"


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
                  m1 := Pl1 (Poke p); m2:= Pl2 NoMove)
  | UseAttack a ->
      (match action2 with
      | Poke p -> failwith "unimplemented"
      | UseAttack a -> failwith "unimplemented"
      (* Later change it so that None becomes a bot move *)
      | NoMove -> let curr_poke = t1.current in
                  let curr_move = findBattleMove curr_poke.pokeinfo a in
                  failwith "unimplemented")
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