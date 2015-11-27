open Async.Std
open Info
open Pokemon

(* All pokemon IVs in this game will be max at 31 *)
let pokeIV = 31

(* Peeks into the Ivar for the game state *)
let get_game_status engine =
  match Deferred.peek (Ivar.read !engine) with
  | Some v -> v
  | None -> failwith "Faultly game logic"

(* Decode the instructions sent by the gui *)
let unpack opt =
  match opt with
  | Some v -> v
  | None -> NoMove

(* Status of Pokemon is changed after switching out *)
let switchOutStatus bpoke =
  match bpoke.curr_status with
  | (x, _) -> (x, [])

(* Stat enhancements are lost after switching out (except for some edge cases)*)
let switchOutStatEnhancements t =
  {attack=(0,1.); defense=(0,1.); speed=(0,1.); special_attack=(0,1.);
  special_defense=(0,1.); evasion=(0,1.); accuracy=(0,1.)}

(* Turns a pokemon into a battle poke *)
let getBattlePoke poke =
  let bhp = (2 * poke.hp + pokeIV + poke.evs.hp / 4) + 100 + 10 in
  (* Changes value of stat based upon Nature of Pokemon *)
  let battack = int_of_float ((match poke.nature with
    | Lonely | Adamant | Naughty | Brave -> 1.1
    | Bold | Modest | Calm | Timid -> 0.9
    | _ -> 1.0) *. float_of_int
            (2 * poke.attack + pokeIV + poke.evs.attack / 4 + 5)) in
  let bdefense = int_of_float ((match poke.nature with
    | Bold | Impish | Lax | Relaxed -> 1.1
    | Lonely | Mild | Gentle | Hasty -> 0.9
    | _ -> 1.0) *.  float_of_int
            (2 * poke.defense + pokeIV + poke.evs.defense / 4 + 5)) in
  let bspecial_attack = int_of_float ((match poke.nature with
    | Modest | Mild | Rash | Quiet -> 1.1
    | Adamant | Impish | Careful | Jolly -> 0.9
    | _ -> 1.0) *.  float_of_int (2 * poke.special_attack +
              pokeIV + poke.evs.special_attack / 4 + 5)) in
  let bspecial_defense = int_of_float ((match poke.nature with
    | Calm | Gentle | Careful | Sassy -> 1.1
    | Naughty | Lax | Rash | Naive -> 0.9
    | _ -> 1.0) *.  float_of_int (2 * poke.special_defense +
              pokeIV + poke.evs.special_defense / 4 + 5)) in
  let bspeed = int_of_float ((match poke.nature with
    | Timid | Hasty | Jolly | Naive -> 1.1
    | Brave | Relaxed | Quiet | Sassy -> 0.9
    | _ -> 1.0) *.  float_of_int (2 * poke.speed + pokeIV +
              poke.evs.speed / 4 + 5)) in
  (* Returns the new battle pokemon as a record *)
  {pokeinfo = poke; curr_hp = bhp; curr_status = (NoNon, []);
  curr_item = poke.item; bhp; battack; bdefense; bspecial_attack;
  bspecial_defense; bspeed}

(* Initializes the game state *)
let initialize_battle team1 team2 =
  team1.current <- getBattlePoke (getTestPoke ()); Battle (InGame
    (team1, team2, ref ClearSkies, ref (Pl1 NoAction), ref (Pl2 NoAction)))

(* Gets a random team of pokemon for initialization *)
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

(* Gets the crit chance based upon the stages.
   Pre-condition: num is greater than 0
   Equation given by Bulbapedia on Critical Strikes article *)
let getCrit poke move =
  let rec helper_crit acc eff = match eff with
  | IncCrit n -> n + acc
  | _ -> 0 in
  let stage = List.fold_left helper_crit 0 move.secondary in
  match stage with
  | 0 -> if (0.0625 > Random.float 1.) then
            (true, 1.5)
          else
            (false, 1.)
  | 1 -> if (0.125 > Random.float 1.) then
            (true, 1.5)
          else
            (false, 1.)
  | 2 -> if (0.50 > Random.float 1.) then
            (true, 1.5)
          else
            (false, 1.)
  | n -> if n >= 3 then (true, 1.5) else (false, 1.)

(* Damage calculation following the equation given by Bulbapedia.
   Stat boosts are taken into account in the beginning *)
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
        SEff (Crit (NormMove move.name))
      else if (type_mod < 1.) then
        NoEff (Crit (NormMove move.name))
      else
        Crit (NormMove move.name)
    else
      if (type_mod > 1.) then
        SEff (NormMove move.name)
      else if (type_mod < 1.) then
        NoEff (NormMove move.name)
      else
        NormMove move.name in
  ( newMove, (210. /. 250. *. attack /. defense*. float_of_int move.power
    +. 2.)*. modifier)

(* Gets the attack multiplier based on current team conditions *)
let findAttackMult t =
  if fst (t.current.curr_status) = Burn then
    0.5
  else
    1.

(* Gets the stat multipliers based on current conditions -- recomputed before
  every move is made *)
let recomputeStat t =
  let stats = t.stat_enhance in
  let attack_mult = findAttackMult t in
  stats.attack <- (fst stats.attack, attack_mult)

(* Finds the Pokemon within a list of Battle Pokemon. Typically used to select
  a Pokemon within the list of alive Pokemon for switching out. Returns the
  switched out pokemon as well as the rest of the list. *)
let findBattlePoke lst name =
  let rec helper acc lst =
    match lst with
    | [] -> failwith ("Faulty Game Logic: Debug " ^ name)
    | h::t -> if h.pokeinfo.name = name then h, (acc @ t) else helper (h::acc)
              t in
  helper [] lst

(* Finds the move of the pokemon based upon a string that is the move's name*)
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

(* Returns true if Pokemon moves, false if it doesn't as well as some value
  describing why it failed (has to do with some status) *)
let hitAttackDueToStatus atk moveDescript =
  let rec helperVolaStatus lst moveDescript' =
    match lst with
    | [] -> (true, moveDescript')
    | _ -> failwith "unimplemented" in
  let nvola, vola = atk.current.curr_status in
  match nvola with
  | Freeze -> if 20 > Random.int 100 then (
                atk.current.curr_status <-(NoNon, snd atk.current.curr_status);
                helperVolaStatus vola (Thaw moveDescript))
              else
                (false, FrozenSolid)
  | _ -> helperVolaStatus vola moveDescript

(* Returns true if Pokemon moves, otherwise returns false as well as some value
   describing why the move failed *)
let hitAttack atk def (move : move) moveDescript =
  let hit, reason = hitAttackDueToStatus atk moveDescript in
  if hit then
    let accStage, accMult = atk.stat_enhance.accuracy in
    let evStage, evMult = def.stat_enhance.evasion in
    let probability = float_of_int move.accuracy *. getStageEvasion  accStage
                    *. accMult /. (getStageEvasion evStage *. evMult) in
    let randnum = Random.float 100. in
    if probability > randnum then
      (true, reason)
    else
      (false, MissMove move.name)
  else
    (hit, reason)
(* Returns true if Pokemon moves, false if it doesn't as well as some value
  describing why it failed (has to do with some status) *)
let hitStatusDueToStatus atk moveDescript =
  let rec helperVolaStatus lst moveDescript' =
    match lst with
    | [] -> (true, moveDescript')
    | _ -> failwith "unimplemented" in
  let nvola, vola = atk.current.curr_status in
  match nvola with
  | Freeze -> if 20 > Random.int 100 then (
                atk.current.curr_status <-(NoNon, snd atk.current.curr_status);
                helperVolaStatus vola (ThawS moveDescript))
              else
                (false, FrozenSolidS)
  | _ -> helperVolaStatus vola moveDescript

(* Returns true if Pokemon moves, false if it doesn't as well as some value
  describing why it failed (has to do with some status) *)
let hitStatus atk def (move: move) moveDescript =
  let hit, reason = hitStatusDueToStatus atk moveDescript in
  if hit then
    match move.target with
    | UserOrAlly | User | UsersField | OpponentsFields
    | Ally | EntireField | UserAndAlly -> (true, reason)
    | _ ->
      let accStage, accMult = atk.stat_enhance.accuracy in
      let evStage, evMult = def.stat_enhance.evasion in
      let probability = float_of_int move.accuracy *. getStageEvasion  accStage
                  *. accMult /. (getStageEvasion evStage *. evMult) in
      let randnum = Random.float 100. in
      if probability > randnum then
        (true, reason)
      else
        (false, MissStatus move.name)
  else
    (hit, reason)

(* Used for writing out the multi move description *)
let rec link_multmove_descript m1 m2 =
  match m2 with
  | HitMult (n, x) ->
      (match m1 with
      | NormMove s -> HitMult (n+1, x)
      | Crit v -> link_multmove_descript v (HitMult(n, Crit x))
      | SEff v -> link_multmove_descript v (HitMult(n, SEff v))
      | NoEff v -> link_multmove_descript v (HitMult (n, NoEff x)))
  | x -> link_multmove_descript m1 (HitMult (1, x))

(* Handles the moves that deal damage *)
let move_handler atk def move =
  (* Recomputes stats before a move is made -- this happens because a burn
    or some status can occur right before a move is made. *)
  let () = recomputeStat atk in
  let () = recomputeStat def in
  (* Call damage calculation to get preliminary move description and damage
      -- ignores secondary effects *)
  let moveDescript, fdamage = damageCalculation atk def move in
  (* creates a reference to move description to allow mutation *)
  let newmove = ref moveDescript in
  (* damage does not need to be mutated *)
  let damage = int_of_float fdamage in
  (* helper function to deal with secondary effects *)
  let rec secondary_effects lst = match lst with
    (* MultiHit is any move that can occur more than once e.g. Double Slap;
    n is the number of times the move has left to hit *)
    | (MultHit n)::t ->
      (* Once MultHit is done calculating, it calculates rest of effects *)
      if n <= 1 then secondary_effects t else
        (let moveDescript', fdamage' = damageCalculation atk def move in
         let damage' = int_of_float fdamage' in
         let newmove' = link_multmove_descript moveDescript' !newmove in
         newmove := newmove';
         def.current.curr_hp <- max 0 (def.current.curr_hp - damage');
         secondary_effects ((MultHit (n-1))::t))
      (* Increase Crit Chance is taken into account during damage dealt -- not
        a real secondary effect so nothing to do here *)
    | (IncCrit _)::t -> secondary_effects t
    (* Calls MultHit after determining how many times to hit consecutively *)
    | RandMultHit::t ->
        let randnum = Random.int 6 + 1 in
        if randnum < 3 then
          secondary_effects ((MultHit 2)::t)
        else if randnum < 5 then
          secondary_effects ((MultHit 3)::t)
        else if randnum < 6 then
          secondary_effects ((MultHit 4)::t)
        else
          secondary_effects ((MultHit 5)::t)
    (* Burns opponent if chance exceeds a certain threshold *)
    | BurnChance::t ->
        let randum = Random.int 100 in
        (if move.effect_chance > randum then
          match def.current.curr_status with
          | (NoNon, x) -> def.current.curr_status <- (Burn, x);
                           newmove := BurnMove !newmove
          | _ -> ()
        else
          ()); secondary_effects t
    (* Freezes opponent if chance exceeds a certain threshold *)
    | FreezeChance::t ->
        let randnum = Random.int 100 in
        (if move.effect_chance < randnum then
           match def.current.curr_status with
           | (NoNon, x) -> def.current.curr_status <- (Freeze, x);
                            newmove := FreezeMove !newmove
            | _ -> ()
          else
            ()); secondary_effects t
    (* Base case *)
    | [] -> ()
    in
  let hit, reason = hitAttack atk def move !newmove in
  if hit then (
    (* damage is always dealt before secondary effects calculated *)
    newmove := reason;
    def.current.curr_hp <- max 0 (def.current.curr_hp - damage);
    secondary_effects move.secondary;
    (* returns a move description *)
    !newmove)
  else
    reason

(* Deals with the status moves that are essentially all secondary effects *)
let rec status_move_handler atk def (move: move) =
  (* stats recomputed -- mainly for accuracy/evasion reasons *)
  let () = recomputeStat atk in
  let () = recomputeStat def in
  (* Similar code to move_handler *)
  let newmove = ref (NormStatus move.name) in
  let rec secondary_effects lst = match lst with
  (* StageBoost is any status move that boosts stats *)
    | (StageBoost l)::t ->
        (match l with
          | [] -> secondary_effects t
          | (s,n)::t' ->
            (match s with
              | Attack ->
                  let stage, multiplier = atk.stat_enhance.attack in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.attack <- (boost, multiplier);
                  newmove := StatBoost (Attack, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | Defense ->
                  let stage, multiplier = atk.stat_enhance.defense in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.defense <- (boost, multiplier);
                  newmove := StatBoost (Defense, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | SpecialAttack ->
                  let stage, multiplier = atk.stat_enhance.special_attack in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.special_attack <- (boost, multiplier);
                  newmove := StatBoost
                                    (SpecialAttack, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | SpecialDefense ->
                  let stage, multiplier = atk.stat_enhance.special_defense in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.special_defense <- (boost, multiplier);
                  newmove := StatBoost
                                    (SpecialDefense, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | Speed ->
                  let stage, multiplier = atk.stat_enhance.speed in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.speed <- (boost, multiplier);
                  newmove := StatBoost (Speed, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | Accuracy ->
                  let stage, multiplier = atk.stat_enhance.accuracy in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.accuracy <- (boost, multiplier);
                  newmove := StatBoost (Accuracy, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | Evasion ->
                  let stage, multiplier = atk.stat_enhance.evasion in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.evasion <- (boost, multiplier);
                  newmove := StatBoost (Evasion, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              )
          )
    (* Base case*)
    | [] -> ()
  in
  let hit, reason = hitStatus atk def move !newmove in
  if hit then (
    newmove := reason;
    (* Returns a description of the status *)
    secondary_effects move.secondary; !newmove)
    (* returns a move description *)
  else
    reason


(* Called after the turn ends; Decrements sleep counter; checks if Pokemon
   faints; etc... *)
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

(* Handles Preprocessing -- Burn damage, weather damage, healing from Leech
    Seed etc... after every move to prepare for next turn *)
let handle_preprocessing t1 t2 w m1 m2 =
  let nstatus, vstatus = t1.current.curr_status in
  (match nstatus with
  | Burn -> t1.current.curr_hp <- max 0 (t1.current.curr_hp - 1 * t1.current.bhp / 8);
            m1 := Pl1 (IncDmg (BurnDmg Base))
  | _ -> m1 := Pl1 Continue);
  let nstatus', vstatus' = t2.current.curr_status in
  (match nstatus' with
  | Burn -> t2.current.curr_hp <- max 0 (t2.current.curr_hp - 1 * t2.current.bhp / 8);
             m2 := Pl2 (IncDmg (BurnDmg Base))
  | _ -> m2 := Pl2 Continue)

(* Handle the case when both Pokemon use a move *)
let handle_two_moves t1 t2 w m1 m2 a1 a2 =
  let p1poke = t1.current in
  let p2poke = t2.current in
    (* Gets speed of both Pokemon with modifiers *)
  let p1speed = float_of_int t1.current.bspeed *.
    getStageAD (fst t1.stat_enhance.speed) *. (snd t1.stat_enhance.speed) in
  let p2speed = float_of_int t2.current.bspeed *.
    getStageAD (fst t2.stat_enhance.speed) *. (snd t2.stat_enhance.speed) in
  (* Case for where Player 1 is faster *)
  if p1speed > p2speed then (
    (* Gets the moves both pokemon used *)
    let curr_move = findBattleMove p1poke.pokeinfo a1 in
    let curr_move' = findBattleMove p2poke.pokeinfo a2 in
    match curr_move.dmg_class with
    (* case where Player 1 uses a Status move *)
    | Status -> let newmove = status_move_handler t1 t2 curr_move in
      (match curr_move'.dmg_class with
      (* case where Player 2 uses a Status Move *)
      | Status -> let newmove' = status_move_handler t2 t1 curr_move' in
                  m1 := Pl1 (Status newmove); m2 := Pl2 (Status newmove')
      (* case where Player 2 uses a Special/Physical Move *)
      | _ -> let newmove' = move_handler t2 t1 curr_move' in
             m1 := Pl1 (Status newmove); m2 := Pl2 (AttackMove newmove'))
    (* Case where Player 1 uses a Physical/Special Move *)
    | _ -> let newmove = move_handler t1 t2 curr_move in
           (* Case where second pokemon faints before getting to move *)
           if (p2poke.curr_hp = 0) then
              (m1 := Pl1 (AttackMove newmove); m2 := Pl2 NoAction)
           (* Case where second pokemon is still alive *)
           else
              (match curr_move'.dmg_class with
              (* Case where Player 2 uses a Status Move *)
              | Status -> let newmove' = status_move_handler t2 t1 curr_move' in
                          m1 := Pl1 (AttackMove newmove);
                          m2 := Pl2 (Status newmove')
              (* Case where Player 2 Uses a Physical/Special Move *)
              | _      -> let newmove' = move_handler t2 t1 curr_move' in
                          m1 := Pl1 (AttackMove newmove);
                          m2 := Pl2 (AttackMove newmove')
              )
    )
  (* Case for where Player 2 is faster *)
  else (
    let curr_move = findBattleMove p2poke.pokeinfo a2 in
    let curr_move' = findBattleMove p1poke.pokeinfo a1 in
    match curr_move.dmg_class with
    | Status -> let newmove = status_move_handler t2 t1 curr_move in
      (match curr_move'.dmg_class with
      | Status -> let newmove' = status_move_handler t1 t2 curr_move' in
                  m1 := Pl2 (Status newmove); m2 := Pl1 (Status newmove')
      | _ -> let newmove' = move_handler t1 t2 curr_move' in
             m1 := Pl2 (Status newmove); m2 := Pl1 (AttackMove newmove'))
    | _ -> let newmove = move_handler t2 t1 curr_move in
           if (p1poke.curr_hp = 0) then
              (m1 := Pl2 (AttackMove newmove); m2 := Pl1 NoAction)
           else
              (match curr_move'.dmg_class with
              | Status -> let newmove' = status_move_handler t1 t2 curr_move' in
                          m1 := Pl2 (AttackMove newmove);
                          m2 := Pl1 (Status newmove')
              | _      -> let newmove'= move_handler t1 t2 curr_move' in
                          m1 := Pl2 (AttackMove newmove);
                          m2 := Pl1 (AttackMove newmove')
              )
    )

(* The main action handler for the game *)
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
                       t1.stat_enhance <- switchOutStatEnhancements t1;
                       t1.current.curr_status <- switchOutStatus t1.current;
                       t1.current <- switchPoke; t1.alive <- prevPoke::restPoke;
                       let curr_move = findBattleMove t2.current.pokeinfo a in
                       if curr_move.dmg_class = Status then
                          ( let newmove = status_move_handler t2 t1 curr_move in
                            m1 := Pl1 (SPoke p); m2 := Pl2 (Status newmove))
                       else (
                        let newmove = move_handler t2 t1 curr_move in
                        m1 := Pl1 (SPoke p); m2 := Pl2 (AttackMove newmove))
      | _ -> failwith "Faulty Game Logic: Debug 444")
  | UseAttack a ->
      (match action2 with
      | Poke p -> failwith "unimplemented"
      | UseAttack a' -> handle_two_moves t1 t2 w m1 m2 a a'
      | _ -> failwith "Faulty Game Logic: Debug 449")
  | NoMove -> (match action2 with
              | FaintPoke p ->
                  let prevPoke = t2.current in
                  let switchPoke, restPoke = findBattlePoke t2.alive p in
                  t2.current <- switchPoke; t2.dead <- prevPoke::t2.dead;
                  t2.alive <- restPoke; m1 := Pl2 (SPoke p);
                  m2 := Pl1 Next
              | _ -> failwith "Faulty Game Logic: Debug 177"
              )
  | FaintPoke p -> (match action2 with
                    | _ ->
                        let prevPoke = t1.current in
                        let switchPoke, restPoke = findBattlePoke t1.alive p in
                        t1.current <- switchPoke; t1.dead <- prevPoke::t1.dead;
                        t1.alive <- restPoke;
                        m1 := Pl1 (SPoke p); m2 := Pl2 Next)
  | Preprocess -> (match action2 with
                  | Preprocess -> handle_preprocessing t1 t2 w m1 m2
                  | _ -> failwith "Faulty Game Logic: Debug 211")
  | TurnEnd -> (match action2 with
                  | TurnEnd -> handle_next_turn t1 t2 w m1 m2
                  | _ -> failwith "Faulty Game Logic: Debug 276")

(* Main loop for 1 player -- gets input from AI *)
let rec main_loop_1p engine gui_ready ready ready_gui () =
  let t2 = match get_game_status engine with
    | Battle InGame (_, t2, _, _, _) -> t2
    | _ -> failwith "Fauly Game Logic" in
  upon (Ivar.read !gui_ready) (* Replace NoMove with ai move later *)
    (fun (cmd1, cmd2) -> let c1 = unpack cmd1 in
                         let c2 = match (unpack cmd2) with
                          | NoMove -> UseAttack (Ai.getRandomMove t2.current)
                          | Preprocess -> Preprocess
                          | FaintPoke _ -> FaintPoke (Ai.replaceDead t2.alive)
                          | TurnEnd -> TurnEnd in
                         let () = handle_action engine c1 c2 in
                         gui_ready := Ivar.create ();
                         Ivar.fill !ready_gui true;
                         (main_loop_1p engine gui_ready ready ready_gui ()));
   Printf.printf "Debug %d \n%!" (Scheduler.cycle_count ())

(* Main controller for random one player *)
let rec main_controller_random1p engine gui_ready ready ready_gui=
  let team1 = getRandomTeam () in
  let team2 = getRandomTeam () in
  let battle = initialize_battle team1 team2 in
  let () = engine := Ivar.create (); Ivar.fill !engine battle in
  main_loop_1p engine gui_ready ready ready_gui ()

(* Initialize controller -- called by Game.ml *)
let initialize_controller (engine, battle_engine) =
  let battle_status, gui_ready, ready, ready_gui = battle_engine in
    Printf.printf "Initializing battle\n%!";
  upon (Ivar.read !battle_status) (fun s -> match s with
    | Random1p -> (main_controller_random1p engine gui_ready ready ready_gui));
  ()