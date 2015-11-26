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
  | None -> NoMove

let initialize_battle team1 team2 = Battle (InGame (team1, team2, ref ClearSkies, ref (Pl1 NoAction), ref (Pl2 NoAction)))

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

(* This function returns the accuracy/evasion bonus given by the stages.
   Pre-condition: num is between -6 and 6
   Equation given by Bulbapedia on Statistics article *)
let getStageEvasion num =
  if abs(num) > 6 then failwith "Faulty Game Logic: Debug 43";
  if num <= 0 then
    3. /. float_of_int (num + 3)
  else
    float_of_int (num + 3) /. 3.

(* This function returns the accuracy/evasion bonus given by the stages.
   Pre-condition: num is between -6 and 6
   Equation given by Bulbapedia on Statistics article *)
let getStageAD num =
  if abs(num) > 6 then failwith "Faulty Game Logic: Debug 42";
  if num <= 0 then
    2. /. float_of_int (num + 2)
  else
    float_of_int (num + 2) /. 2.

let getCrit poke move = (false, 1.)

let damageCalculation t1 t2 move =
  let defense = match move.dmg_class with
    | Physical ->
      float_of_int t2.current.bdefense *.
      getStageAD (fst t2.stat_enhance.defense) *.
      (snd t2.stat_enhance.defense)
    | Special ->
      float_of_int t2.current.bspecial_defense *.
      getStageAD (fst t2.stat_enhance.special_defense) *.
      (snd t2.stat_enhance.special_defense)
    | _ -> failwith "Faulty Game Logic: Debug 44" in
  let attack = match move.dmg_class with
    | Physical ->
      float_of_int t1.current.battack *.
      getStageAD (fst t1.stat_enhance.attack) *.
      (snd t1.stat_enhance.attack)
    | Special ->
      float_of_int t1.current.bspecial_attack *.
      getStageAD (fst t1.stat_enhance.special_attack) *.
      (snd t1.stat_enhance.special_attack) in
  let crit_bool, crit  = getCrit t1.current move in
  let type_mod = List.fold_left (fun acc x -> acc *. getElementEffect
      move.element x) 1. t2.current.pokeinfo.element in
  let modifier =
      (* type effectiveness *)
      type_mod *.
      (* STAB bonus *)
      if (List.mem move.element t1.current.pokeinfo.element) then 1.5 else 1. *.
      (* Crit bonus *)
      crit *.
      (* Random fluctuation in power *)
      (Random.float 15. +. 85.) /. 100. in
  let newMove =
    if (crit_bool) then
      if (type_mod > 1.) then
        SEffCrit move.name
      else if (type_mod < 1.) then
        NoEffCrit move.name
      else
        Crit move.name
    else
      if (type_mod > 1.) then
        SEff move.name
      else if (type_mod < 1.) then
        NoEff move.name
      else
        NormMove move.name in
  ( newMove, (210. /. 250. *. attack /. defense*. float_of_int move.power
    +. 2.)*. modifier)

let findBattlePoke lst name =
  let rec helper acc lst =
    match lst with
    | [] -> failwith ("Faulty Game Logic: Debug " ^ name)
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

let handle_next_turn t1 t2 w m1 m2 =
  match t1.current.curr_hp with
  | 0 -> if (t2.current.curr_hp = 0) then
            (m1 := Pl1 Faint; m2 := Pl2 Faint)
          else
            (m1 := Pl2 Next; m2 := Pl1 Faint)
  | _ -> if (t2.current.curr_hp = 0) then
            (m1 := Pl1 Next; m2 := Pl2 Faint)
          else
            (m1 := Pl1 Next; m2 := Pl2 Next)

let handle_preprocessing t1 t2 w m1 m2 =
  m1 := Pl1 Continue; m2 := Pl2 Continue

let handle_two_moves t1 t2 w m1 m2 a1 a2 =
  let p1poke = t1.current in
  let p2poke = t2.current in
  let p1speed = float_of_int t1.current.bspeed *.
    getStageAD (fst t1.stat_enhance.speed) *. (snd t1.stat_enhance.speed) in
  let p2speed = float_of_int t2.current.bspeed *.
    getStageAD (fst t2.stat_enhance.speed) *. (snd t2.stat_enhance.speed) in
  if p1speed > p2speed then (
    let curr_move = findBattleMove p1poke.pokeinfo a1 in
    let curr_move' = findBattleMove p2poke.pokeinfo a2 in
    match curr_move.dmg_class with
    | Status ->
      (match curr_move'.dmg_class with
      | Status -> m1 := Pl1 NoAction; m2 := Pl2 NoAction
      | _ -> let newmove, fdamage = damageCalculation t2 t1 curr_move' in
             let damage = int_of_float fdamage in
             p1poke.curr_hp <- max 0 (p1poke.curr_hp - damage);
             m1 := Pl1 NoAction; m2 := Pl2 (Attack newmove))
    | _ -> let newmove, fdamage = damageCalculation t1 t2 curr_move in
           let damage = int_of_float fdamage in
           p2poke.curr_hp <- max 0 (p2poke.curr_hp - damage);
           if (p2poke.curr_hp = 0) then
              (m1 := Pl1 (Attack newmove); m2 := Pl2 NoAction)
           else
              (match curr_move'.dmg_class with
              | Status -> m1 := Pl1 (Attack newmove); m2 := Pl2 NoAction
              | _      -> let newmove', fdamage' = damageCalculation t2 t1
                              curr_move' in
                          let damage' = int_of_float fdamage' in
                          p1poke.curr_hp<-max 0 (p1poke.curr_hp - damage');
                          m1 := Pl1 (Attack newmove);
                          m2 := Pl2 (Attack newmove')
              )
    )
  else (
    let curr_move = findBattleMove p2poke.pokeinfo a2 in
    let curr_move' = findBattleMove p1poke.pokeinfo a1 in
    match curr_move.dmg_class with
    | Status ->
      (match curr_move'.dmg_class with
      | Status -> m1 := Pl2 NoAction; m2 := Pl1 NoAction
      | _ -> let newmove, fdamage = damageCalculation t1 t2 curr_move' in
             let damage = int_of_float fdamage in
             p2poke.curr_hp <- max 0 (p2poke.curr_hp - damage);
             m1 := Pl2 NoAction; m2 := Pl1 (Attack newmove))
    | _ -> let newmove, fdamage = damageCalculation t2 t1 curr_move in
           let damage = int_of_float fdamage in
           p1poke.curr_hp <- max 0 (p1poke.curr_hp - damage);
           if (p1poke.curr_hp = 0) then
              (m1 := Pl2 (Attack newmove); m2 := Pl1 NoAction)
           else
              (match curr_move'.dmg_class with
              | Status -> m1 := Pl2 (Attack newmove); m2 := Pl1 NoAction
              | _      -> let newmove', fdamage' = damageCalculation t1 t2
                              curr_move' in
                          let damage' = int_of_float fdamage' in
                          p2poke.curr_hp<-max 0 (p2poke.curr_hp - damage');
                          m1 := Pl2 (Attack newmove);
                          m2 := Pl1 (Attack newmove')
              )
    )

let handle_action state action1 action2 =
  let t1, t2, w, m1, m2 = match get_game_status state with
    | Battle InGame (t1, t2, w, m1, m2) -> t1, t2, w, m1, m2
    | _ -> failwith "Fauly Game Logic" in
  match action1 with
  | Poke p ->
      (match action2 with
      | Poke p -> failwith "unimplemented"
      | UseAttack a -> let prevPoke = t1.current in
                       let switchPoke, restPoke = findBattlePoke t1.alive p in
                       t1.current <- switchPoke; t1.alive <- prevPoke::restPoke;
                       let curr_move = findBattleMove t2.current.pokeinfo a in
                       if curr_move.dmg_class = Status then
                          (m1 := Pl1 (SPoke p); m2 := Pl2 NoAction)
                       else (
                        let newmove, fdamage = damageCalculation t2 t1 curr_move in
                        let damage = int_of_float fdamage in
                        t1.current.curr_hp <- max 0 (t1.current.curr_hp - damage);
                        m1 := Pl1 (SPoke p); m2 := Pl2 (Attack newmove))
      (* Later change it so that None becomes a bot move *)
      | NoMove -> let prevPoke = t1.current in
                  let switchPoke, restPoke = findBattlePoke t1.alive p in
                  t1.current <- switchPoke; t1.alive <- prevPoke::restPoke;
                  m1 := Pl1 (SPoke p); m2:= Pl2 NoAction)
  | UseAttack a ->
      (match action2 with
      | Poke p -> failwith "unimplemented"
      | UseAttack a' -> handle_two_moves t1 t2 w m1 m2 a a'
      (* Later change it so that None becomes a bot move *)
      | NoMove -> let curr_poke = t1.current in
                  let curr_move = findBattleMove curr_poke.pokeinfo a in
                  if curr_move.dmg_class = Status then
                    (m1 := Pl1 NoAction; m2 := Pl2 NoAction)
                  else
                    (let newmove, fdamage = damageCalculation t1 t2 curr_move in
                    let damage = int_of_float fdamage in
                    t2.current.curr_hp <- max 0 (t2.current.curr_hp - damage);
                    m1 := Pl1 (Attack newmove); m2 := Pl2 NoAction)
                  )
  | NoMove -> (match action2 with
              | FaintPoke p -> let prevPoke = t2.current in
                               let switchPoke, restPoke = findBattlePoke t2.alive p in
                               t2.current <- switchPoke; t2.dead <- prevPoke::t2.dead;
                               t2.alive <- restPoke; m1 := Pl2 (SPoke p);
                               m2 := Pl1 Next
              | _ -> failwith "Faulty Game Logic: Debug 177"
              )
  | FaintPoke p -> (match action2 with
                    | _ -> let prevPoke = t1.current in
                                let switchPoke, restPoke = findBattlePoke t1.alive p in
                                t1.current <- switchPoke; t1.dead <- prevPoke::t1.dead;
                                t1.alive <- restPoke; m1 := Pl1 (SPoke p);
                                m2 := Pl2 Next)
  | Preprocess -> (match action2 with
                  | Preprocess -> handle_preprocessing t1 t2 w m1 m2
                  | _ -> failwith "Faulty Game Logic: Debug 211")
  | TurnEnd -> (match action2 with
                  | TurnEnd -> handle_next_turn t1 t2 w m1 m2
                  | _ -> failwith "Faulty Game Logic: Debug 276")

let rec main_loop_1p engine gui_ready ready ready_gui () =
  let t2 = match get_game_status engine with
    | Battle InGame (_, t2, _, _, _) -> t2
    | _ -> failwith "Fauly Game Logic" in
  upon (Ivar.read !gui_ready) (* Replace NoMove with ai move later *)
    (fun (cmd1, cmd2) -> let c1 = unpack cmd1 in let c2 = match (unpack cmd2) with
                          | NoMove -> UseAttack (Ai.getRandomMove t2.current)
                          | Preprocess -> Preprocess
                          | FaintPoke _ -> FaintPoke (Ai.replaceDead t2.alive)
                          | TurnEnd -> TurnEnd in
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