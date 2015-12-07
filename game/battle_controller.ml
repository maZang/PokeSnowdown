open Async.Std
open Info
open Pokemon
open MutableQ

(* All pokemon IVs in this game will be max at 31 *)
let pokeIV = 31

(* A mutable queue to process the battle commands *)
let command_queue = MutableQ.empty ()

(* Peeks into the Ivar for the game state *)
let get_game_status engine =
  match Deferred.peek (Ivar.read !engine) with
  | Some v -> v
  | None -> failwith "Faulty game logic"

(* Decode the instructions sent by the gui *)
let unpack opt =
  match opt with
  | Some v -> v
  | None -> AIMove

(* Status of Pokemon is changed after switching out depending on Pokemon's ability
 *)
let switchOutStatus bpoke =
  if bpoke.curr_abil = "natural-cure" then
    (NoNon, [])
  else
    (match bpoke.curr_status with
    | (x, _) -> (match x with
              | Toxic _ -> (Toxic 0, [])
              | _ -> (x, [])))

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
  bspecial_defense; bspeed; curr_abil = poke.ability;
  curr_type = poke.element}

(* Used for some secondary conditions *)
let prevmove1 = ref ""
let prevmove2 = ref ""
let prevpoke1 = ref (getBattlePoke (getTestPoke ()))
let prevpoke2 = ref (getBattlePoke (getTestPoke ()))

let convertToMega t =
  let found = ref false in
  let helper bpoke =
    match bpoke.pokeinfo.item with
    | MegaStone ->
        if findMega bpoke.pokeinfo.name then
        (found := true; getBattlePoke (convertToMega bpoke.pokeinfo "-mega"))
        else
          bpoke
    | MegaStoneY ->
        if findMegaX bpoke.pokeinfo.name then
        (found := true; getBattlePoke (convertToMega bpoke.pokeinfo "-mega-y"))
        else
          bpoke
    | MegaStoneX ->
        if findMegaX bpoke.pokeinfo.name then
        (found := true; getBattlePoke (convertToMega bpoke.pokeinfo "-mega-x"))
        else
          bpoke
    | _ -> bpoke  in
    t.current <- helper (t.current);
    if !found then ()
    else
      (t.alive <- List.map (fun x -> if !found then x else helper x) t.alive)


(* Initializes the game state *)
let initialize_battle team1 team2 =
  convertToMega team1;
  convertToMega team2;
   Battle (InGame
    (team1, team2, {weather = ClearSkies;
      terrain = {side1= ref []; side2= ref []}},
      ref (Pl1 NoAction)))

(* Gets a random team of pokemon for initialization *)
let getRandomTeam mode =
  let randFunc = match mode with
  | `Random -> getRandomPokemon
  | `Preset -> fun () -> getRandomPreset ()
  | `Tournament -> getRandomPreset ~pjson:(Tournament.getJson ()) in
  let stat_enhance = {attack=(0,1.); defense=(0,1.); speed=(0,1.);
      special_attack=(0,1.); special_defense=(0,1.); evasion=(0,1.);
      accuracy=(0,1.)} in
  {current = (getBattlePoke(randFunc ()));
  dead =[]; alive = (List.map getBattlePoke
  (List.map randFunc [();();();();()])); stat_enhance}

(* This function returns the accuracy/evasion bonus given by the stages.
   Pre-condition: num is between -6 and 6
   Equation given by Bulbapedia on Statistics article *)
let getStageEvasion num =
  if abs(num) > 6 then failwith "Faulty Game Logic: Debug 43";
  if num <= 0 then
    3. /. float_of_int (- 1 * num + 3)
  else
    float_of_int (num + 3) /. 3.

(* This function returns the accuracy/evasion bonus given by the stages.
   Pre-condition: num is between -6 and 6
   Equation given by Bulbapedia on Statistics article *)
let getStageAD num =
  if abs(num) > 6 then failwith "Faulty Game Logic: Debug 42";
  if num <= 0 then
    2. /. float_of_int (-1 * num + 2)
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

let get_weather_amplifier w (move : move) =
  match w with
  | Sun _ -> (match move.element with
                        | Water -> 0.5
                        | Fire -> 1.5
                        | _ -> 1.0)
  | Rain _ -> (match move.element with
                        | Water -> 1.5
                        | Fire -> 0.5
                        | _ -> 1.0)
  | _ -> 1.0

(* Damage calculation following the equation given by Bulbapedia.
   Stat boosts are taken into account in the beginning *)
let damageCalculation t1 t2 (w,ter1, ter2) (move : move) =
  let abil_modifier, move_type = match t1.current.curr_abil with
      | "pixilate" ->
         if move.element = Normal then (1.3, Fairy) else (1., move.element)
      | "refrigerate" ->
         if move.element = Normal then (1.3, Ice) else (1., move.element)
      | "aerilate" ->
         if move.element = Normal then (1.3, Ice) else (1., move.element)
      | _ -> (1., move.element) in
  let defense = match move.dmg_class with
    | Physical ->
      let rec findReflect ter = match ter with
      | [] -> false
      | (Reflect _)::t -> true
      | h::t -> findReflect t in
      (match findReflect !ter2 with
      | true -> 2.0
      | false -> 1.0) *.
      float_of_int t2.current.bdefense *.
      getStageAD (fst t2.stat_enhance.defense) *.
      (snd t2.stat_enhance.defense)
    | Special ->
      let rec findLightScreen ter = match ter with
      | [] -> false
      | (LightScreen _)::t -> true
      | h::t -> findLightScreen t in
      (match findLightScreen !ter2 with
      | true -> 2.0
      | false -> 1.0) *.
      (match w with
      | SandStorm _ -> if (List.mem Rock t2.current.curr_type) then 1.5 else 1.0
      | _ -> 1.0) *.
      float_of_int t2.current.bspecial_defense *.
      getStageAD (fst t2.stat_enhance.special_defense) *.
      (snd t2.stat_enhance.special_defense)
    | Status -> failwith "Faulty Game Logic: Debug 44" in
  let attack =
    (match t1.current.curr_abil with
    | "swarm" ->
    if move.element = Bug && t1.current.curr_hp * 3 <= t1.current.bhp then 1.5
    else 1.0
    | "blaze" ->
    if move.element = Fire && t1.current.curr_hp * 3 <= t1.current.bhp then 1.5
    else 1.0
    | "overgrow" ->
    if move.element = Grass && t1.current.curr_hp * 3 <= t1.current.bhp then 1.5
    else 1.0
    | "torrent" ->
    if move.element = Water && t1.current.curr_hp * 3 <= t1.current.bhp then 1.5
    else 1.0
    | "sand-force" ->
    if move.element = Rock || move.element = Ground || move.element = Steel
    then 1.3 else 1.0
    | "technician" -> if move.power <= 60 then 1.5 else 1.0
    | _ -> 1.0 ) *.
    (match t1.current.curr_item with
    | LifeOrb -> 1.3
    | _ -> 1.0 ) *.
    (match move.dmg_class with
    | Physical ->
      (match t1.current.curr_abil with
      | "huge-power" | "pure-power" -> 2.0
      | "guts" -> if fst t1.current.curr_status <> NoNon then 1.5 else 1.0
      | _ -> 1.0 ) *.
      ( match t1.current.curr_item with
      | ChoiceBand -> 1.5
      | LightBall -> if t1.current.pokeinfo.name = "pikachu" then 2.0 else 1.0
      | _ -> 1.0) *.
      float_of_int t1.current.battack *.
      getStageAD (fst t1.stat_enhance.attack) *.
      (snd t1.stat_enhance.attack)
    | Special ->
      ( match t1.current.curr_item with
      | ChoiceSpecs -> 1.5
      | LightBall -> if t1.current.pokeinfo.name = "pikachu" then 2.0 else 1.0
      | _ -> 1.0) *.
      float_of_int t1.current.bspecial_attack *.
      getStageAD (fst t1.stat_enhance.special_attack) *.
      (snd t1.stat_enhance.special_attack)
    | Status -> failwith "Faulty Game Logic: Debug 178") in
  let crit_bool, crit  = getCrit t1.current move in
  let type_mod' = List.fold_left (fun acc x -> acc *. getElementEffect
      move_type x) 1. t2.current.curr_type  *. (if
      t2.current.curr_abil = "levitate" && move.element = Ground then
      0. else 1.) in
  let type_mod = (if type_mod' = 1. && t2.current.curr_abil = "wonder-guard"
                 then 0. else type_mod') in
  let weather_amplifier = get_weather_amplifier w move in
  let modifier =
      (* type effectiveness *)
      type_mod *. abil_modifier *.
      (* STAB bonus *)
      if (List.mem move.element t1.current.curr_type) then 1.5 else 1. *.
      (* Crit bonus *)
      crit *.
      (* weather bonus *)
      weather_amplifier *.
      (* Random fluctuation in power *)
      (Random.float 15. +. 85.) /. 100. in
  let newMove =
    if (crit_bool) then
      if (type_mod > 1.) then
        SEff (Crit (NormMove move.name))
      else if (type_mod < 1. && type_mod > 0.) then
        NoEff (Crit (NormMove move.name))
      else if (type_mod = 0.) then
        NoEffAll (move.name)
      else
        Crit (NormMove move.name)
    else
      if (type_mod > 1.) then
        SEff (NormMove move.name)
      else if (type_mod < 1. && type_mod > 0.) then
        NoEff (NormMove move.name)
      else if (type_mod = 0.) then
        NoEffAll (move.name)
      else
        NormMove move.name in
  Printf.printf "%d\n%!" (int_of_float type_mod);
  ( newMove, (210. /. 250. *. attack /. defense*. float_of_int move.power
    +. 2.)*. modifier)

(* Gets the speed multiplier based on current team conditions *)
let findSpeedMult t =
  if fst (t.current.curr_status) = Paralysis then
    0.25
  else
    1.

(* Gets the attack multiplier based on current team conditions *)
let findAttackMult t =
  if fst (t.current.curr_status) = Burn && (t.current.curr_abil <> "guts") then
    0.5
  else
    1.

(* Gets the stat multipliers based on current conditions -- recomputed before
  every move is made *)
let recomputeStat t =
  let stats = t.stat_enhance in
  let attack_mult = findAttackMult t in
  let speed_mult = findSpeedMult t in
  stats.attack <- (fst stats.attack, attack_mult);
  stats.speed <- (fst stats.speed, speed_mult)

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

(* Finds a random pokemon and returns name of Pokemon*)
let getRandomPoke t =
  let n = List.length t.alive in
  let num = Random.int n in
  (List.nth t.alive num).pokeinfo.name

(* Finds the move of the pokemon based upon a string that is the move's name*)
let findBattleMove poke move =
  Printf.printf "%s %s\n%!" poke.name move;
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

(* Helper function to fix confusion *)
let decrementConfusion atk =
  let nonvola, vola = atk.curr_status in
  let rec helper acc lst = match lst with
  | (Confusion n)::t when n <= 0 -> acc @ t
  | (Confusion n)::t -> acc @ ((Confusion(n-1))::t)
  | h::t -> helper (h::acc) t
  | [] -> acc in
  let newvola = helper [] vola in
  atk.curr_status <- (nonvola, newvola)

let rec filter_substitute n lst =
  match lst with
  | (Substitute _)::t -> if n = 0 then t else (Substitute n)::t
  | h::t -> h::(filter_substitute n t)
  | [] -> []

(* Helper function to see if pokemon can move; also called to break a Pokemon
  out of a status condition *)
let hitMoveDueToStatus atk moveDescript move =
  let rec helperVolaStatus lst moveDescript' =
    match lst with
      | [] -> (true, moveDescript')
      | Charge::t -> helperVolaStatus t moveDescript'
      | Flinch::t -> (false, FlinchA)
      | (Confusion n)::t ->
            (* n cannot be less than 0 -- invariant *)
            decrementConfusion atk.current;
            (if n = 0 then
                helperVolaStatus t (BreakConfuse moveDescript')
            else
              (if Random.int 100 > 50 then
                helperVolaStatus t moveDescript'
              else
                (let confuse_damage = int_of_float
                    (42. *. float_of_int atk.current.battack *.
                     getStageAD (fst atk.stat_enhance.attack) *.
                    (snd atk.stat_enhance.attack) *. 0.8 /.
                    (float_of_int atk.current.bdefense *.
                    getStageAD (fst atk.stat_enhance.defense) *.
                    (snd atk.stat_enhance.defense)) +. 2.) in
                atk.current.curr_hp <- atk.current.curr_hp - confuse_damage;
                (false, Confused))
              ))
      | Leeched::t -> helperVolaStatus t moveDescript'
      | (Substitute _)::t -> helperVolaStatus t moveDescript'
      | Protected::t -> helperVolaStatus t moveDescript'
      | UsedProtect::t -> helperVolaStatus t moveDescript'
      | (ForcedMoveNoSwitch _)::t -> helperVolaStatus t moveDescript'
      | (ForcedMove _)::t -> helperVolaStatus t moveDescript'
      | (Taunt _)::t -> helperVolaStatus t moveDescript'
      | (PartialTrapping _)::t -> helperVolaStatus t moveDescript'
      | (RechargingStatus)::t -> helperVolaStatus t moveDescript' in
  let nvola, vola = atk.current.curr_status in
  match nvola with
  | Freeze -> if List.mem Ice atk.current.curr_type then (
                atk.current.curr_status <- (NoNon, snd atk.current.curr_status);
                helperVolaStatus vola (NoFreeze moveDescript))
              else if 20 > Random.int 100 then (
                atk.current.curr_status <- (NoNon, snd atk.current.curr_status);
                helperVolaStatus vola (Thaw moveDescript))
              else
                (false, FrozenSolid)
  | Burn -> if List.mem Fire atk.current.curr_type then (
              atk.current.curr_status <- (NoNon, snd atk.current.curr_status);
              helperVolaStatus vola (NoBurn moveDescript))
            else
              helperVolaStatus vola (moveDescript)
  | Paralysis ->
      if List.mem Electric atk.current.curr_type ||
         atk.current.curr_abil = "limber"
      then (atk.current.curr_status <- (NoNon, snd atk.current.curr_status);
           helperVolaStatus vola (NoPara moveDescript))
      else if 75 > Random.int 100
      then (helperVolaStatus vola (moveDescript))
      else (false, Para)
  | Sleep n -> if n <= 0 || atk.current.curr_abil = "insomnia" then
               (atk.current.curr_status <- (NoNon, snd atk.current.curr_status);
                helperVolaStatus vola (Wake moveDescript))
               else if List.mem SleepEffect move.secondary then
                helperVolaStatus vola (SleepAttack moveDescript)
              else
                (false ,Asleep)
  |_ -> helperVolaStatus vola (moveDescript)

(* Helper function for finding protect *)
let rec find_protect lst =
  match lst with
  | Protected::t -> true
  | h::t -> find_protect t
  | [] -> false

(* Returns true if Pokemon moves, otherwise returns false as well as some value
   describing why the move failed *)
let hitAttack atk def (w,t1,t2) (move : move) damage moveDescript =
  let accStage, accMult = atk.stat_enhance.accuracy in
  let evStage, evMult = def.stat_enhance.evasion in
  let probability = float_of_int move.accuracy *. getStageEvasion  accStage
                    *. accMult /. (getStageEvasion evStage *. evMult) in
  let randnum = Random.float 100. in
  let rec need_charge_attack = function
  | (ChargeMove s)::t -> (true, s)
  | (ChargeInSunlight s)::t ->
                (match w with
                | Sun _| HarshSun _-> need_charge_attack t
                | _ -> (true, s))
  | h::t -> need_charge_attack t
  | [] -> (false, "") in
  let rec get_substitute_health = function
  | [] -> None
  | (Substitute n)::_  -> Some n
  | h::t -> get_substitute_health t in
  let hit_move () =
    if probability > randnum || List.mem NeverMiss move.secondary
    || atk.current.curr_abil = "no-guard" || def.current.curr_abil= "no-guard"
    then
      match get_substitute_health (snd def.current.curr_status) with
      | None -> (if find_protect (snd def.current.curr_status) then
                  (false, ProtectedA move.name)
                else
                  (true, moveDescript))
      | Some n -> let sub_damage = max 0 (n - damage) in
                  let newvola = filter_substitute sub_damage
                                (snd def.current.curr_status) in
              def.current.curr_status <- (fst def.current.curr_status, newvola);
                  if sub_damage = 0 then
                    (false, BreakSub moveDescript)
                  else
                    (false, SubDmg moveDescript)
    else
      (false, MissMove move.name) in
  let need_charge, charge_string = need_charge_attack move.secondary in
    if need_charge then
      if List.mem (Charge) (snd atk.current.curr_status) then
        let volatile_list =
          List.filter (fun s -> not (s = Charge)) (snd atk.current.curr_status)
          in
          atk.current.curr_status<-(fst atk.current.curr_status, volatile_list);
          hit_move ()
      else
        (atk.current.curr_status <-
        (fst atk.current.curr_status, (ForcedMoveNoSwitch (1, move.name))
                                     ::(Charge)::(snd atk.current.curr_status));
        (false, ChargingMove (charge_string, move.name)))
    else
      hit_move ()

(* Helper function for finding substitutes *)
let rec find_substitute lst =
  match lst with
  | (Substitute _)::t -> true
  | h::t -> find_substitute t
  | [] -> false
(* Returns true if Pokemon moves, false if it doesn't as well as some value
  describing why it failed (has to do with some status) *)
let hitStatus atk def (move: move) moveDescript =
  let rec findTaunt lst = match lst with
  | (Taunt _)::_ -> true
  | h::t -> findTaunt t
  | [] -> false in
  if findTaunt (snd atk.current.curr_status) then
    (false, Taunted move.name)
  else
    (match move.target with
    | UserOrAlly | User | UsersField | OpponentsFields
    | Ally | EntireField | UserAndAlly -> (true, moveDescript)
    | _ ->
      let accStage, accMult = atk.stat_enhance.accuracy in
      let evStage, evMult = def.stat_enhance.evasion in
      let probability = float_of_int move.accuracy *. getStageEvasion  accStage
                *. accMult /. (getStageEvasion evStage *. evMult) in
      let randnum = Random.float 100. in
      if probability > randnum then
        if find_substitute (snd def.current.curr_status) then
          (false, SubBlock moveDescript)
        else if find_protect (snd def.current.curr_status) then
          (false, ProtectedA move.name)
        else
          (true, moveDescript)
      else
        (false, MissMove move.name))

(* Used for writing out the multi move description *)
let rec link_multmove_descript m1 m2 =
  match m2 with
  | HitMult (n, x) ->
      (match m1 with
      | NormMove s -> HitMult (n+1, x)
      | Crit v -> link_multmove_descript v (HitMult(n, Crit x))
      | SEff v -> link_multmove_descript v (HitMult(n, SEff v))
      | NoEff v -> link_multmove_descript v (HitMult (n, NoEff x))
      | _ -> failwith "Faulty Game Logic: Debug 444")
  | x -> link_multmove_descript m1 (HitMult (1, x))

(* Handles the moves that deal damage *)
let move_handler atk def wt (move : move) =
  let wt', weather, ter1, ter2 =
  match wt with
  | (wt', ter1, ter2) -> wt', (wt'.weather, ter1, ter2), ter1, ter2 in

  (* Recomputes stats before a move is made -- this happens because a burn
    or some status can occur right before a move is made. *)
  let () = recomputeStat atk in
  let () = recomputeStat def in
  (* Call damage calculation to get preliminary move description and damage
      -- ignores secondary effects *)
  (if atk.current.curr_abil = "protean" then
    atk.current.curr_type <- [move.element]
  else
    ());
  let moveDescript, fdamage = damageCalculation atk def weather move in
  (* creates a reference to move description to allow mutation *)
  let newmove = ref moveDescript in
  (* damage does not need to be mutated *)
  let damage = ref (int_of_float fdamage) in
  let effect_chance = move.effect_chance *
                    (if atk.current.curr_abil = "serene-grace" then 2 else 1) in
  (* helper function to deal with secondary effects *)
  let rec secondary_effects lst = match lst with
    (* MultiHit is any move that can occur more than once e.g. Double Slap;
    n is the number of times the move has left to hit *)
    | (MultHit n)::t ->
      (* Once MultHit is done calculating, it calculates rest of effects *)
      if n <= 1 then secondary_effects t else
        (let moveDescript', fdamage' = damageCalculation atk def weather move in
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
        let randnum =
          if atk.current.curr_abil = "skill-link" then
            5
          else
            Random.int 6 + 1 in
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
        (if effect_chance > randum then
          match def.current.curr_status with
          | (NoNon, x) -> def.current.curr_status <- (Burn, x);
                           newmove := BurnMove !newmove
          | _ -> ()
        else
          ()); secondary_effects t
    (* Freezes opponent if chance exceeds a certain threshold *)
    | FreezeChance::t ->
        let randnum = Random.int 100 in
        (if effect_chance > randnum then
           match def.current.curr_status with
           | (NoNon, x) -> def.current.curr_status <- (Freeze, x);
                            newmove := FreezeMove !newmove
            | _ -> ()
          else
            ()); secondary_effects t
    (* Paralyzed opponent if chance exceeds a certain threshold*)
    | ParaChance::t ->
        let randnum = Random.int 100 in
        (if effect_chance > randnum then
          match def.current.curr_status with
          | (NoNon, x) -> def.current.curr_status <- (Paralysis, x);
                            newmove := ParaMove !newmove
          | _ -> ()
        else
          ()); secondary_effects t
    (* Paralyzed opponent if chance exceeds a certain threshold*)
    | PutToSleep::t ->
        let randnum = Random.int 100 in
        (if effect_chance > randnum then
          let sleep_turns = Random.int 3 + 2 in
          match def.current.curr_status with
          | (NoNon, x) -> def.current.curr_status <- (Sleep sleep_turns, x);
                            newmove := SleepMove !newmove
          | _ -> ()
        else
          ()); secondary_effects t
    (* For the move super fang *)
    | SuperFang::t ->
          (let type_mod = List.fold_left (fun acc x -> acc *. getElementEffect
              move.element x) 1. def.current.curr_type in
            if type_mod > 0. then
              (def.current.curr_hp <- (def.current.curr_hp + !damage)/2;
              secondary_effects t)
            else
              newmove := NoEffAll move.name)
    (* One hit KO moves *)
    | OHKO::t ->
        (let type_mod = List.fold_left (fun acc x -> acc *. getElementEffect
        move.element x) 1. def.current.curr_type in
        if type_mod > 0. then
        (def.current.curr_hp <- 0; newmove := OHKill !newmove;
        secondary_effects t)
        else
          newmove := NoEffAll move.name)
    (* Charging Moves dealt with in hit moves-- nothing to do here *)
    | (ChargeMove _)::t | (ChargeInSunlight _)::t -> secondary_effects t
    (* Flinch Moves have a certain chance to make target flinch *)
    | FlinchMove::t ->
        let randnum = Random.int 100 in
        (if effect_chance > randnum then
          def.current.curr_status <- (fst def.current.curr_status,
                Flinch::(snd def.current.curr_status))
        else
          ()); secondary_effects t
    (* Recoil moves deal certain damage to the user *)
    | RecoilMove::t ->
        if atk.current.curr_abil = "rock-head" then
          newmove := NoRecoil !newmove
        else
          (newmove := Recoil !newmove;
          atk.current.curr_hp <- max 0 (atk.current.curr_hp - !damage / 3))
    (* Chance of poisoning the opponent *)
    | PoisonChance::t ->
        let randnum = Random.int 100 in
        (if effect_chance > randnum then
          match def.current.curr_status with
          | (NoNon, x) -> def.current.curr_status <- (Poisoned, x);
                            newmove := PoisonMove !newmove
          | _ -> ()
        else
          ()); secondary_effects t
    (* Constant damage moves -- note these moves have a power level of 0 *)
    | (ConstantDmg n)::t ->
          let type_mod = List.fold_left (fun acc x -> acc *. getElementEffect
            move.element x) 1. def.current.curr_type in
          if type_mod > 0. then
            (def.current.curr_hp <- max 0 (def.current.curr_hp - n + !damage);
            secondary_effects t)
          else
            newmove := NoEffAll move.name
    (* ChanceStageBoost has a 10% chance of boosting all stats
     * except accuracy and evasion *)
    | ChanceStageBoost::t ->
        (match Random.int 10 with
        | 0 ->
          (let stage1, multiplier1 = atk.stat_enhance.attack in
          let boost1 = max (min 6 (stage1 + 1)) (-6) in
          atk.stat_enhance.attack <- (boost1, multiplier1);
          newmove := StatBoostA (Attack, (boost1 - stage1), !newmove);
          let stage2, multiplier2 = atk.stat_enhance.defense in
          let boost2 = max (min 6 (stage2 + 1)) (-6) in
          atk.stat_enhance.defense <- (boost2, multiplier2);
          newmove := StatBoostA (Defense, (boost2 - stage2), !newmove);
          let stage3, multiplier3 = atk.stat_enhance.special_attack in
          let boost3 = max (min 6 (stage3 + 1)) (-6) in
          atk.stat_enhance.special_attack <- (boost3, multiplier3);
          newmove := StatBoostA (SpecialAttack, (boost3 - stage3), !newmove);
          let stage4, multiplier4 = atk.stat_enhance.special_defense in
          let boost4 = max (min 6 (stage4 + 1)) (-6) in
          atk.stat_enhance.special_defense <- (boost4, multiplier4);
          newmove := StatBoostA (SpecialDefense, (boost4 - stage4), !newmove);
          let stage5, multiplier5 = atk.stat_enhance.speed in
          let boost5 = max (min 6 (stage5 + 1)) (-6) in
          atk.stat_enhance.speed <- (boost5, multiplier5);
          newmove := StatBoostA (Speed, (boost5 - stage5), !newmove);
          secondary_effects t)
        | _ -> secondary_effects t)
    (* StageBoost is any status move that boosts stats *)
    | (StageBoost l)::t ->
        (match l with
          | [] -> secondary_effects t
          | (s,n')::t' ->
          let randnum = Random.int 100 in
          if (effect_chance > randnum) then
            (let n = if atk.current.curr_abil = "contrary" then -n' else n' in
              match s with
              | Attack ->
                  let stage, multiplier = atk.stat_enhance.attack in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.attack <- (boost, multiplier);
                  newmove := StatBoostA (Attack, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | Defense ->
                  let stage, multiplier = atk.stat_enhance.defense in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.defense <- (boost, multiplier);
                  newmove := StatBoostA (Defense, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | SpecialAttack ->
                  let stage, multiplier = atk.stat_enhance.special_attack in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.special_attack <- (boost, multiplier);
                  newmove := StatBoostA
                                    (SpecialAttack, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | SpecialDefense ->
                  let stage, multiplier = atk.stat_enhance.special_defense in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.special_defense <- (boost, multiplier);
                  newmove := StatBoostA
                                    (SpecialDefense, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | Speed ->
                  let stage, multiplier = atk.stat_enhance.speed in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.speed <- (boost, multiplier);
                  newmove := StatBoostA (Speed, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | Accuracy ->
                  let stage, multiplier = atk.stat_enhance.accuracy in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.accuracy <- (boost, multiplier);
                  newmove := StatBoostA (Accuracy, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | Evasion ->
                  let stage, multiplier = atk.stat_enhance.evasion in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.evasion <- (boost, multiplier);
                  newmove := StatBoostA (Evasion, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              ) else secondary_effects ((StageAttack t')::t)
          )
    (* Moves that have a chance of lowering a Pokemon's stat *)
    | (StageAttack l)::t ->
        (match l with
          | [] -> secondary_effects t
          | (s,n')::t' ->
            let randnum = Random.int 100 in
            if (effect_chance > randnum) then
            (let n = if def.current.curr_abil = "contrary" then -n' else n' in
              match s with
              | Attack ->
                  let stage, multiplier = def.stat_enhance.attack in
                  let boost = max (min 6 (stage - n)) (-6) in
                  def.stat_enhance.attack <- (boost, multiplier);
                  newmove := StatAttackA (Attack, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              | Defense ->
                  let stage, multiplier = def.stat_enhance.defense in
                  let boost = max (min 6 (stage - n)) (-6) in
                  Printf.printf "%d\n%!" boost;
                  def.stat_enhance.defense <- (boost, multiplier);
                  newmove := StatAttackA (Defense, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              | SpecialAttack ->
                  let stage, multiplier = def.stat_enhance.special_attack in
                  let boost = max (min 6 (stage - n)) (-6) in
                  def.stat_enhance.special_attack <- (boost, multiplier);
                  newmove := StatAttackA
                                    (SpecialAttack, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              | SpecialDefense ->
                  let stage, multiplier = def.stat_enhance.special_defense in
                  let boost = max (min 6 (stage - n)) (-6) in
                  def.stat_enhance.special_defense <- (boost, multiplier);
                  newmove := StatAttackA
                                    (SpecialDefense, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              | Speed ->
                  let stage, multiplier = def.stat_enhance.speed in
                  let boost = max (min 6 (stage - n)) (-6) in
                  def.stat_enhance.speed <- (boost, multiplier);
                  newmove := StatAttackA (Speed, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              | Accuracy ->
                  let stage, multiplier = def.stat_enhance.accuracy in
                  let boost = max (min 6 (stage - n)) (-6) in
                  def.stat_enhance.accuracy <- (boost, multiplier);
                  newmove := StatAttackA (Accuracy, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              | Evasion ->
                  let stage, multiplier = def.stat_enhance.evasion in
                  let boost = max (min 6 (stage - n)) (-6) in
                  def.stat_enhance.evasion <- (boost, multiplier);
                  newmove := StatAttackA (Evasion, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              ) else secondary_effects ((StageAttack t')::t)
          )

    (* Moves that have a chance of confusing the opponent *)
    | ConfuseOpp::t ->let randnum = Random.int 100 in
                          (if (effect_chance > randnum) then
                            (let confuse_turns = Random.int 4 + 1 in
                            let novola , x = def.current.curr_status in
                            let rec check_for_confusion = function
                            | [] -> false
                            | (Confusion _)::t -> true
                            | h::t -> check_for_confusion t in
                            (if check_for_confusion x then
                              ()
                            else
                            (def.current.curr_status <-
                              (novola, (Confusion confuse_turns)::x);
                            newmove := ConfuseMoveA !newmove)))
                          else
                           ()); secondary_effects t
    (* Moves that confuse the user *)
    | ConfuseUser::t -> (let confuse_turns = Random.int 4 + 1 in
                            let novola , x = atk.current.curr_status in
                            let rec check_for_confusion = function
                            | [] -> false
                            | (Confusion _)::t -> true
                            | h::t -> check_for_confusion t in
                            (if check_for_confusion x then
                              ()
                            else
                            (atk.current.curr_status <-
                              (novola, (Confusion confuse_turns)::x);
                            newmove :=
                                  ConfuseUserA !newmove))); secondary_effects t
    (* Moves that take a turn of recharge after use e.g. hyperbeam *)
    | RechargeMove::t ->
        (atk.current.curr_status <- (fst atk.current.curr_status,
                               RechargingStatus::(snd atk.current.curr_status));
                          newmove := Recharging !newmove; secondary_effects t)
    (* Moves based upon weight are instead based on current health *)
    | WeightDamage::t ->
      (let base_power = max 20
                    ((def.current.curr_hp + !damage) * 120 / def.current.bhp) in
      move.power <- base_power;
      let moveDescript', fdamage' = damageCalculation atk def weather move in
      let damage' = int_of_float fdamage' in
      def.current.curr_hp <- max 0 (def.current.curr_hp - damage' + !damage));
      secondary_effects t
    (* Moves based upon weight are instead based on current health *)
    | GyroBall::t ->
      (let base_power = min 150 (int_of_float
          (25. *. (float_of_int def.current.bspeed
          /. float_of_int atk.current.bspeed))) in
      move.power <- base_power;
      let moveDescript', fdamage' = damageCalculation atk def weather move in
      let damage' = int_of_float fdamage' in
      def.current.curr_hp <- max 0 (def.current.curr_hp - damage' + !damage));
      secondary_effects t
    (* Damage that varies *)
    | VariableDamage::t ->
      (let base_power = int_of_float ((Random.float 1. +. 0.5) *. 100.) in
        move.power <- base_power;
        let moveDescript', fdamage' = damageCalculation atk def weather move in
        let damage' = int_of_float fdamage' in
        def.current.curr_hp <- max 0 (def.current.curr_hp - damage' + !damage));
        secondary_effects t
    (* for the move flail *)
    | Flail::t ->
        (let base_power = max 20 ((atk.current.bhp - atk.current.curr_hp) * 200
                                                          / atk.current.bhp) in
        move.power <- base_power;
        let moveDescript', fdamage' = damageCalculation atk def weather move in
        let damage' = int_of_float fdamage' in
        def.current.curr_hp <- max 0 (def.current.curr_hp - damage' + !damage));
        secondary_effects t
    (* Max health damage *)
    | MaxHealthDmg::t ->
         (let base_power = max 20 (atk.current.curr_hp * 150
                                       / atk.current.bhp) in
        move.power <- base_power;
        let moveDescript', fdamage' = damageCalculation atk def weather move in
        let damage' = int_of_float fdamage' in
        def.current.curr_hp <- max 0 (def.current.curr_hp - damage' + !damage));
        secondary_effects t
    (* Beat Up *)
    | BeatUp::t ->
      (let base_power =  (List.length atk.alive  + 1 ) * 20 in
        move.power <- base_power;
        let moveDescript', fdamage' = damageCalculation atk def weather move in
        let damage' = int_of_float fdamage' in
        def.current.curr_hp <- max 0 (def.current.curr_hp - damage' + !damage));
        secondary_effects t
    (* Stored Power *)
    | StoredPower::t ->
      (let base_power =
          (fst atk.stat_enhance.attack + fst atk.stat_enhance.defense +
          fst atk.stat_enhance.special_attack +
          fst atk.stat_enhance.special_defense +
          fst atk.stat_enhance.speed + fst atk.stat_enhance.accuracy +
          fst atk.stat_enhance.evasion) * 20 + 20 in
        move.power <- base_power;
        let moveDescript', fdamage' = damageCalculation atk def weather move in
        let damage' = int_of_float fdamage' in
        def.current.curr_hp <- max 0 (def.current.curr_hp - damage' + !damage));
        secondary_effects t
    (* Moves that drain health *)
    | DrainMove::t ->
      let heal = !damage / 2 in
      atk.current.curr_hp <- min atk.current.bhp (atk.current.curr_hp + heal);
      newmove := DrainA !newmove
      (* Moves that drain health if opponent is asleep *)
    | DrainMoveSleep::t ->
      (match fst def.current.curr_status with
      | Sleep _ -> secondary_effects (DrainMove::t)
      | _ -> newmove := DrainSleepFail move.name;
            def.current.curr_hp <- def.current.curr_hp + !damage)
            ;secondary_effects t
    (* Moves that cause user to faint *)
    | UserFaint::t -> atk.current.curr_hp <- 0; newmove := UserFaintA !newmove;
                      secondary_effects t
    (* Moves that never miss are handled elsewhere *)
    | NeverMiss::t -> secondary_effects t
    (* Move that leaves the opponent with 1 HP *)
    | FalseSwipe::t -> (if (def.current.curr_hp = 0) then
                          (def.current.curr_hp <- 1;
                          newmove := (FalseSwipeA !newmove))
                       else (); secondary_effects t)
    (* Moves that force a switch out *)
    | ForceSwitch::t ->
                  if List.length def.alive > 0 then
                    (newmove := SwitchOutA !newmove; secondary_effects t)
                  else
                    newmove := FailA move.name
    (* Base case *)
    | SelfEncore::t ->  let rec findForcedMove = function
                        | (ForcedMoveNoSwitch (n,_))::t ->(true, n)
                        | h::t -> findForcedMove t
                        | [] -> (false,0) in
                        let found, num = findForcedMove
                                                (snd atk.current.curr_status) in
                        if found then
                          (if num = 0 then
                            (secondary_effects (ConfuseUser::t))
                          else
                            ())
                        else
                          (let n = Random.int 2 + 1 in
                            atk.current.curr_status <-
                            (fst atk.current.curr_status,
                            (ForcedMoveNoSwitch (n, move.name))
                            ::(snd atk.current.curr_status)))
    (* For the move knock off *)
    | KnockOff::t ->
        (match def.current.curr_item with
        | Nothing -> secondary_effects t
        | _ ->
          def.current.curr_hp <- max 0 (def.current.curr_hp - !damage/2);
          newmove := KnockedOff (def.current.curr_item, !newmove);
          def.current.curr_item <- Nothing; secondary_effects t)
    (* Volt Switch and Bug Buzz Dealt with Elsewhere *)
    | SelfSwitch::t -> secondary_effects t
    (* For the move Foul Play *)
    | FoulPlay::t ->
        (let moveDescript', fdamage' = damageCalculation def def weather move in
        let damage' = int_of_float fdamage' in def.current.curr_hp <- max 0
                                  (def.current.curr_hp - damage' + !damage);
                      secondary_effects t)
    (* for the move snore *)
    | SleepEffect::t -> let findSleep x = match x with
                        | (Sleep _, _) -> true
                        | _ -> false in
                      if findSleep (atk.current.curr_status) then
                        secondary_effects t
                      else
                        (def.current.curr_hp <- def.current.curr_hp + !damage;
                          newmove := SleepAttackFail move.name)
    | ChancePower::t ->
        (let randum = Random.int 100 in
        if (randum < 5) then move.power <- 10
        else if (randum >= 5 && randum < 15) then move.power <- 30
        else if (randum >= 15 && randum < 35) then move.power <- 50
        else if (randum >= 35 && randum < 65) then move.power <- 70
        else if (randum >= 65 && randum < 85) then move.power <- 90
        else if (randum >= 85 && randum < 95) then move.power <- 110
        else move.power <- 150;
        let moveDescript', fdamage' = damageCalculation atk def weather move in
        let damage' = int_of_float fdamage' in
        def.current.curr_hp <- max 0 (def.current.curr_hp - damage' + !damage);
        secondary_effects t)
    (* for the trapping moves *)
    | CausePartialTrapping::t ->
        let rec findPartialTrapping = function
        | (PartialTrapping (s, _))::t -> if s = move.name then true
                                         else findPartialTrapping t
        | h::t -> findPartialTrapping t
        | [] -> false in
        if findPartialTrapping (snd def.current.curr_status) then ()
        else
          (let turns = Random.int 3 + 2 in
          def.current.curr_status <- (fst def.current.curr_status,
           (PartialTrapping (move.name, turns))::(snd def.current.curr_status));
            newmove := TrappingMove !newmove)
    (* moves that double in power *)
    | DoublePower::t ->
          (match move.name with
          | "brine" ->
              if def.current.curr_hp * 2 <= def.current.bhp
              then def.current.curr_hp <- max 0 (def.current.curr_hp - !damage)
              else ()
          | "hex" ->
              if fst atk.current.curr_status = NoNon then ()
              else def.current.curr_hp <- max 0 (def.current.curr_hp - !damage)
          | "venoshock" -> (match (fst atk.current.curr_status) with
                           | Poisoned | Toxic _ ->
                            def.current.curr_hp <- max 0
                                                (def.current.curr_hp - !damage)
                           | _ -> ())
                | _ -> ())
    (* for the move electro ball *)
    | ElectroBall::t ->
      (let speed1 = float_of_int atk.current.bspeed in
      let speed2 = float_of_int def.current.bspeed in
      let sratio = speed2 /. speed1 in
      let base_power = if (sratio >= 0.5) then 60
                       else if (sratio >= 0.34 && sratio < 0.5) then 80
                       else if (sratio >= 0.25 && sratio < 0.34) then 120
                       else 150 in
      move.power <- base_power;
      let moveDescript', fdamage' = damageCalculation atk def weather move in
      let damage' = int_of_float fdamage' in
      def.current.curr_hp <- max 0 (def.current.curr_hp - damage' + !damage));
      secondary_effects t
    (* for the move rapid spin *)
    | RapidSpin::t -> let rec filter_nonvola = function
                      | (PartialTrapping _)::t -> filter_nonvola t
                      | Leeched::t -> filter_nonvola t
                      | h::t -> h::(filter_nonvola t)
                      | [] -> [] in
                      let rec filter_terrain = function
                      | StickyWeb::t | StealthRock::t-> filter_terrain t
                      | ToxicSpikes _::t | Spikes _::t -> filter_terrain t
                      | h::t -> h::(filter_terrain t)
                      | [] -> [] in
                      (atk.current.curr_status <- (fst atk.current.curr_status,
                                  filter_nonvola (snd atk.current.curr_status));
                      ter1 := filter_terrain (!ter1);
                      newmove := RapidSpinA !newmove)
    | FinalGambit::t ->
        (def.current.curr_hp <- max 0 (def.current.curr_hp -
                                       atk.current.curr_hp);
        atk.current.curr_hp <- 0; secondary_effects t)
    | FakeOut::t ->
        let current = float_of_int def.current.curr_hp in
        let base = float_of_int def.current.bhp in
        if (current /. base >= 0.85) then secondary_effects t
        else
          (def.current.curr_hp <- (def.current.curr_hp + !damage);
          newmove := FailA "Fake Out")
    | Facade::t ->
        (match fst atk.current.curr_status with
        | NoNon -> secondary_effects t
        | _ -> (def.current.curr_hp <- max 0 (def.current.curr_hp - !damage);
               secondary_effects t))
    | SmellingSalts::t ->
        (match def.current.curr_status with
        | (Paralysis, x) ->
            def.current.curr_hp <- max 0 (def.current.curr_hp - !damage);
            def.current.curr_status <- (NoNon, x); secondary_effects t
        | _ -> secondary_effects t)
    | Endeavor::t ->
        (let tmp = atk.current.curr_hp in
        if tmp < def.current.curr_hp then
        def.current.curr_hp <- tmp; secondary_effects t)
    | Counter::t ->
        (let prevstring, prevpoke = if wt'.terrain.side1 == ter1 then
                                  (!prevmove2, !prevpoke1)
                                 else
                                  if wt'.terrain.side1 == ter2 then
                                    (!prevmove1, !prevpoke2)
                                else failwith "Faulty Game Logic: Debug 1044" in
        Printf.printf "%s\n%!" prevpoke.pokeinfo.name;
        (match move.name with
       | "counter"->(try (let prevmove = Pokemon.getMoveFromString prevstring in
                        match prevmove.dmg_class with
                        | Physical -> (let hpdamage = prevpoke.curr_hp -
                            atk.current.curr_hp in
                                      def.current.curr_hp <- max 0
                                        (def.current.curr_hp -
                                            (2*hpdamage) + !damage);
                                      secondary_effects t)
                        | _ -> def.current.curr_hp <- def.current.curr_hp +
                          !damage; newmove := FailA "Counter") with | _ ->
                            newmove := FailA "Counter")
        | "mirror-coat" -> (try (let prevmove = Pokemon.getMoveFromString
                            prevstring in
                          match prevmove.dmg_class with
                            | Special -> (let hpdamage = prevpoke.curr_hp -
                                atk.current.curr_hp in
                                         def.current.curr_hp <- max 0
                                          (def.current.curr_hp - (2*hpdamage) +
                                            !damage);
                                         secondary_effects t)
                        | _ -> def.current.curr_hp <- def.current.curr_hp +
                          !damage; newmove := FailA "Mirror Coat") with | _ ->
                              newmove := FailA "Mirror Coat")
        | "metal-burst" -> (let hpdamage = float_of_int (prevpoke.curr_hp -
                              atk.current.curr_hp) in
                           let newhp = (float_of_int def.current.curr_hp) -.
                            1.5*.hpdamage +. float_of_int (!damage) in
                           def.current.curr_hp <- max 0 (int_of_float newhp);
                           secondary_effects t)
        | "revenge" | "avalanche" | "payback" ->
                           (let hpdamage = prevpoke.curr_hp -
                              atk.current.curr_hp in
                           if (hpdamage > 0) then
                              (def.current.curr_hp <- max 0 (def.current.curr_hp
                                - !damage);
                              secondary_effects t)
                           else ())

        | _ -> ()))
    | [] -> ()
    | _ -> failwith "Faulty Game Logic: Debug 783"
    in
  let hit, reason' = hitMoveDueToStatus atk (!newmove) move in
  if hit && (!damage > 0 || move.power = 0) then (
    let hit', newreason = hitAttack atk def weather move !damage reason' in
    (* damage is always dealt before secondary effects calculated *)
    if hit' then (
      newmove := newreason;
      (match def.current.curr_abil with
      | "lightning-rod" when move.element = Electric ->
          (newmove := StatAttackA (SpecialAttack, 1,(FailA move.name));
            def.stat_enhance.special_attack <- (min 6
              (fst def.stat_enhance.special_attack + 1),
              snd def.stat_enhance.special_attack))
      | "volt-absorb" when move.element = Electric ->
          (newmove := HealOppA (FailA move.name);
          def.current.curr_hp <- min def.current.bhp (def.current.curr_hp +
          def.current.bhp/4))
      | _ ->
          (damage := min def.current.curr_hp !damage;
          def.current.curr_hp <-def.current.curr_hp - !damage;
          (* returns a move description *)
          secondary_effects move.secondary;
          (match atk.current.curr_item with
          | LifeOrb -> atk.current.curr_hp <- max 0 (atk.current.curr_hp -
              atk.current.bhp/10); newmove := LifeOrbA !newmove
          | ChoiceBand | ChoiceSpecs | ChoiceScarf -> atk.current.curr_status <-
            (fst atk.current.curr_status, ForcedMove (1, move.name)::
              (snd atk.current.curr_status) )
          | _ -> ())));
          !newmove)
   else
      (newmove := newreason;
      if (!newmove = MissMove move.name && (move.name = "high-jump-kick" ||
          move.name = "jump-kick")) then
        (newmove := HitSelf !newmove;
        (atk.current.curr_hp <- max 0 (atk.current.curr_hp -
          atk.current.bhp/2)));
      !newmove))
  else
    reason'

(* Deals with the status moves that are essentially all secondary effects *)
let rec status_move_handler atk def (wt, t1, t2) (move: move) =
  let w = wt.weather in
  (* stats recomputed -- mainly for accuracy/evasion reasons *)
  let () = recomputeStat atk in
  let () = recomputeStat def in
  (* Similar code to move_handler *)
  let newmove = ref (NormStatus move.name) in
  (if atk.current.curr_abil = "protean" then
    atk.current.curr_type <- [move.element]
  else
    ());
  let rec secondary_effects lst = match lst with
    | (StageBoostSunlight l)::t ->
        (match w with
        | HarshSun _ | Sun _ ->
          (secondary_effects
            ((StageBoost (List.map (fun (stat, n) -> (stat, 2 * n)) l))::t))
        | _ -> secondary_effects ((StageBoost l)::t))
    (* StageBoost is any status move that boosts stats *)
    | (StageBoost l)::t ->
        (match l with
          | [] -> secondary_effects t
          | (s,n')::t' ->
            ( let n = if atk.current.curr_abil = "contrary" then -n' else n' in
              match s with
              | Attack ->
                  let stage, multiplier = atk.stat_enhance.attack in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.attack <- (boost, multiplier);
                  newmove := StatBoostA (Attack, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | Defense ->
                  let stage, multiplier = atk.stat_enhance.defense in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.defense <- (boost, multiplier);
                  newmove := StatBoostA (Defense, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | SpecialAttack ->
                  let stage, multiplier = atk.stat_enhance.special_attack in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.special_attack <- (boost, multiplier);
                  newmove := StatBoostA
                                    (SpecialAttack, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | SpecialDefense ->
                  let stage, multiplier = atk.stat_enhance.special_defense in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.special_defense <- (boost, multiplier);
                  newmove := StatBoostA
                                    (SpecialDefense, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | Speed ->
                  let stage, multiplier = atk.stat_enhance.speed in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.speed <- (boost, multiplier);
                  newmove := StatBoostA (Speed, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | Accuracy ->
                  let stage, multiplier = atk.stat_enhance.accuracy in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.accuracy <- (boost, multiplier);
                  newmove := StatBoostA (Accuracy, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              | Evasion ->
                  let stage, multiplier = atk.stat_enhance.evasion in
                  let boost = max (min 6 (stage + n)) (-6) in
                  atk.stat_enhance.evasion <- (boost, multiplier);
                  newmove := StatBoostA (Evasion, (boost - stage), !newmove);
                  secondary_effects ((StageBoost t')::t)
              )
          )
    (* RandStageBoost randomly boosts a stat *)
    | RandStageBoost::t ->
        (match Random.int 7 with
        | 0 -> secondary_effects (StageBoost[(Attack,2)]::t)
        | 1 -> secondary_effects (StageBoost[(Defense,2)]::t)
        | 2 -> secondary_effects (StageBoost[(SpecialAttack,2)]::t)
        | 3 -> secondary_effects (StageBoost[(SpecialDefense,2)]::t)
        | 4 -> secondary_effects (StageBoost[(Speed,2)]::t)
        | 5 -> secondary_effects (StageBoost[(Accuracy,2)]::t)
        | 6 -> secondary_effects (StageBoost[(Evasion,2)]::t)
        | _ -> failwith "Does Not Happen"); secondary_effects t
    (* Move that forces a switch out *)
    | ForceSwitch::t ->
                  if List.length def.alive > 0 then
                    (newmove := SwitchOutA !newmove; secondary_effects t)
                  else
                    newmove := FailA move.name
    (* Move that lowers stat of opponent *)
    | StageAttack l::t ->
        (match l with
          | [] -> secondary_effects t
          | (s,n')::t' ->
            (let n = if def.current.curr_abil = "contrary" then -n' else n' in
              match s with
              | Attack ->
                  let stage, multiplier = def.stat_enhance.attack in
                  let boost = max (min 6 (stage - n)) (-6) in
                  def.stat_enhance.attack <- (boost, multiplier);
                  newmove := StatAttackA (Attack, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              | Defense ->
                  let stage, multiplier = def.stat_enhance.defense in
                  let boost = max (min 6 (stage - n)) (-6) in
                  Printf.printf "%d\n%!" boost;
                  def.stat_enhance.defense <- (boost, multiplier);
                  newmove := StatAttackA (Defense, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              | SpecialAttack ->
                  let stage, multiplier = def.stat_enhance.special_attack in
                  let boost = max (min 6 (stage - n)) (-6) in
                  def.stat_enhance.special_attack <- (boost, multiplier);
                  newmove := StatAttackA
                                    (SpecialAttack, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              | SpecialDefense ->
                  let stage, multiplier = def.stat_enhance.special_defense in
                  let boost = max (min 6 (stage - n)) (-6) in
                  def.stat_enhance.special_defense <- (boost, multiplier);
                  newmove := StatAttackA
                                    (SpecialDefense, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              | Speed ->
                  let stage, multiplier = def.stat_enhance.speed in
                  let boost = max (min 6 (stage - n)) (-6) in
                  def.stat_enhance.speed <- (boost, multiplier);
                  newmove := StatAttackA (Speed, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              | Accuracy ->
                  let stage, multiplier = def.stat_enhance.accuracy in
                  let boost = max (min 6 (stage - n)) (-6) in
                  def.stat_enhance.accuracy <- (boost, multiplier);
                  newmove := StatAttackA (Accuracy, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              | Evasion ->
                  let stage, multiplier = def.stat_enhance.evasion in
                  let boost = max (min 6 (stage - n)) (-6) in
                  def.stat_enhance.evasion <- (boost, multiplier);
                  newmove := StatAttackA (Evasion, (boost - stage), !newmove);
                  secondary_effects ((StageAttack t')::t)
              )
          )
    (* Moves that put opponent to sleep *)
    | PutToSleep::t -> let sleep_turns = Random.int 3 + 2 in
                      (match def.current.curr_status with
                      | (NoNon, x) ->
                         def.current.curr_status <- (Sleep sleep_turns, x);
                        newmove := MakeSleep !newmove
                      | _ -> ()); secondary_effects t
    (* Moves that confuse opponent *)
    | ConfuseOpp::t -> let confuse_turns = Random.int 4 + 1 in
                       let novola , x = def.current.curr_status in
                       let rec check_for_confusion = function
                       | [] -> false
                       | (Confusion _)::t -> true
                       | h::t -> check_for_confusion t in
                       (if check_for_confusion x then
                          ()
                        else
                          (def.current.curr_status <-
                            (novola, (Confusion confuse_turns)::x);
                          newmove := ConfuseMoveA !newmove)); secondary_effects t
    (* Essentially for Leech Seed *)
    | LeechSeed::t -> let novola, x = def.current.curr_status in
                      let rec check_for_leech = function
                      | [] -> false
                      | Leeched::t -> true
                      | h::t -> check_for_leech t in
                      (if check_for_leech x then
                        ()
                      else
                        (def.current.curr_status <- (novola, Leeched::x);
                        newmove := LeechA !newmove)); secondary_effects t
    (* Burn status moves *)
    | BurnChance::t -> (match def.current.curr_status with
          | (NoNon, x) -> def.current.curr_status <- (Burn, x);
                            newmove := BurnMove !newmove
          | _ -> ()); secondary_effects t
    (* Poison status moves *)
    | PoisonChance::t -> (match def.current.curr_status with
          | (NoNon, x) -> def.current.curr_status <- (Poisoned, x);
                            newmove := PoisonMove !newmove
          | _ -> ()); secondary_effects t
    (* Paralysis status moves *)
    | ParaChance::t -> (match def.current.curr_status with
          | (NoNon, x) -> def.current.curr_status <- (Paralysis, x);
                            newmove := ParaMove !newmove
          | _ -> ()); secondary_effects t
    (* status moves that badly poison opponents *)
    | ToxicChance::t -> (match def.current.curr_status with
          | (NoNon, x) -> def.current.curr_status <- (Toxic 0, x);
                          newmove := BadPoisonMove !newmove
          | _ -> ()); secondary_effects t
    (* Variable heal depending on weather *)
    | SunHeal::t -> let heal = match w with
                    | ClearSkies -> atk.current.bhp / 2
                    | Sun _| HarshSun _-> 2 * atk.current.bhp / 3
                    | _ -> atk.current.bhp / 4 in
                    atk.current.curr_hp <-
                      min atk.current.bhp (atk.current.curr_hp + heal);
                    newmove := HealHealth !newmove; secondary_effects t
    (* moves that heal the user *)
    | Recovery::t -> let heal = atk.current.bhp / 2 in
                  atk.current.curr_hp <-
                      min atk.current.bhp (atk.current.curr_hp + heal);
                  newmove := HealHealth !newmove; secondary_effects t
    | UserFaint::t -> atk.current.curr_hp <- 0; newmove := UserFaintA !newmove;
                      secondary_effects t
    (* moves that make light screen *)
    | LightScreenMake::t -> let rec findLightScreen ter = match ter with
                            | (LightScreen _)::t -> true
                            | h::t -> findLightScreen t
                            | [] -> false in
                            (if findLightScreen !t1 then
                              ()
                            else
                              (t1 := ((LightScreen 4)::!t1);
                              newmove := LightScreenA !newmove));
                            secondary_effects t
    (* move that resets stat boosts/drops *)
    | Haze::t -> atk.stat_enhance <- switchOutStatEnhancements atk;
                 def.stat_enhance <- switchOutStatEnhancements def;
                 newmove := HazeA !newmove;
                 secondary_effects t
    (* for the move Reflect *)
    | ReflectMake::t -> let rec findReflect ter = match ter with
                        | (Reflect _)::t -> true
                        | h::t -> findReflect t
                        | [] -> false in
                        (if findReflect !t1 then
                          ()
                        else
                          (t1 := ((Reflect 4)::!t1);
                          newmove := ReflectA !newmove));
                        secondary_effects t
    (* for the move rest *)
    | Rest::t -> (match atk.current.curr_status with
                  | (Sleep _, _) -> ()
                  | (_, x) -> atk.current.curr_status <- (Sleep 4, x);
                              atk.stat_enhance <- switchOutStatEnhancements atk;
                              atk.current.curr_hp <- atk.current.bhp;
                              newmove := RestA !newmove); secondary_effects t
    (* for the move substitute *)
    | SubstituteMake::t -> (if find_substitute (snd atk.current.curr_status) ||
                        atk.current.curr_hp <= atk.current.bhp / 4 then
                        newmove := SubFail !newmove
                      else
                        (atk.current.curr_hp <- atk.current.curr_hp -
                            atk.current.bhp / 4;
                        atk.current.curr_status <- (fst atk.current.curr_status,
                            (Substitute (atk.current.bhp/4))::
                              (snd atk.current.curr_status));
                        newmove := SubMake !newmove)); secondary_effects t
    (* protect has 1/4 chance of working on subsequent use  *)
    | Protect::t -> let rec find = function
                    | [] -> false
                    | UsedProtect::_ -> true
                    | h::t -> find t in
                   (if find (snd atk.current.curr_status) then
                    (if 1 > Random.int 4 then
                      (atk.current.curr_status <- (fst atk.current.curr_status,
                          Protected::(snd atk.current.curr_status));
                      newmove := ProtectA !newmove)
                    else
                      newmove := ProtectFail !newmove)
                    else
                      (atk.current.curr_status <- (fst atk.current.curr_status,
                          Protected::(snd atk.current.curr_status));
                      newmove := ProtectA !newmove)); secondary_effects t
    (* For the move belly drum *)
    | BellyDrum::t -> if atk.current.curr_hp > atk.current.bhp / 2 then
                        (atk.current.curr_hp <- atk.current.curr_hp -
                            atk.current.bhp / 2;
                          secondary_effects ((StageBoost [(Attack,6)])::t))
                      else
                        newmove := FailA "Belly Drum"
    (* for the spikes *)
    | Spikes::t -> let rec addSpikes acc1 acc2 ter = match ter with
                        | (Spikes n)::t ->if n >= 3 then
                                            (false, acc2 @ (Spikes 3)::t)
                                          else
                                            (true, acc2 @ (Spikes (n+1))::t)
                        | h::t -> addSpikes acc1 (h::acc2) t
                        | [] -> (acc1, (Spikes 1)::acc2) in
                  let success, newter = addSpikes true [] !t2 in
                  if success then
                    (t2 := newter;
                    newmove := SpikesA !newmove;
                    secondary_effects t)
                  else
                    newmove := FailA move.name
    (* for the toxic spikes *)
    | TSpikes::t -> let rec addSpikes acc1 acc2 ter = match ter with
                        | (ToxicSpikes n)::t ->if n >= 2 then
                                            (false, acc2 @ (ToxicSpikes 2)::t)
                                          else
                                            (true, acc2 @
                                                (ToxicSpikes (n+1))::t)
                        | h::t -> addSpikes acc1 (h::acc2) t
                        | [] -> (acc1, (ToxicSpikes 1)::acc2) in
                  let success, newter = addSpikes true [] !t2 in
                  if success then
                    (t2 := newter;
                    newmove := ToxicSpikesA !newmove;
                    secondary_effects t)
                  else
                    newmove := FailA move.name
    (* for heal bell and aromatherapy *)
    | HealBell::t -> let helper_heal poke =
                      poke.curr_status <- (NoNon, snd poke.curr_status) in
                     List.iter helper_heal atk.alive;
                     helper_heal atk.current;
                     newmove := HealBellA !newmove;
                     secondary_effects t
    (* Sunny Day*)
    | SunnyDay::t -> (match w with
                    | Sun _ | HarshSun _ -> ()
                    | _ -> wt.weather <- Sun 5; newmove := SunnyDayA !newmove;
                    secondary_effects t)
    (* Cures burns, paralysis, poison*)
    | Refresh::t -> (match atk.current.curr_status with
                    | (Poisoned, x) -> atk.current.curr_status <- (NoNon, x)
                    | (Toxic _, x) -> atk.current.curr_status <- (NoNon, x)
                    | (Paralysis, x) -> atk.current.curr_status <- (NoNon, x)
                    | (Burn, x) -> atk.current.curr_status <- (NoNon, x)
                    | _ -> ()); newmove := RefreshA !newmove;
                        secondary_effects t
    | PsychoShift::t ->
        (let tmp = ref NoNon in
        (match (atk.current.curr_status, def.current.curr_status) with
        | (Burn, x), (NoNon, _) ->
            tmp := Burn; atk.current.curr_status <- (NoNon, x);
            def.current.curr_status <- (!tmp, snd (def.current.curr_status))
        | (Paralysis, x), (NoNon, _) ->
            tmp := Paralysis; atk.current.curr_status <- (NoNon, x);
            def.current.curr_status <- (!tmp, snd (def.current.curr_status))
        | (Poisoned, x), (NoNon, _) ->
            tmp := Poisoned; atk.current.curr_status <- (NoNon, x);
            def.current.curr_status <- (!tmp, snd (def.current.curr_status))
        | (Toxic y, x), (NoNon, _)->
            tmp := Toxic 0; atk.current.curr_status <- (NoNon, x);
            def.current.curr_status <- (!tmp, snd (def.current.curr_status))
        | _ -> ()); secondary_effects t)
    (* Copies changes to target's stats and replicate to user *)
    | PsychUp::t -> (let i1 = fst def.stat_enhance.attack in
                    let i2 = fst def.stat_enhance.defense in
                    let i3 = fst def.stat_enhance.speed in
                    let i4 = fst def.stat_enhance.special_attack in
                    let i5 = fst def.stat_enhance.special_defense in
                    let i6 = fst def.stat_enhance.evasion in
                    let i7 = fst def.stat_enhance.accuracy in
                    let f1 = snd atk.stat_enhance.attack in
                    let f2 = snd atk.stat_enhance.defense in
                    let f3 = snd atk.stat_enhance.speed in
                    let f4 = snd atk.stat_enhance.special_attack in
                    let f5 = snd atk.stat_enhance.special_defense in
                    let f6 = snd atk.stat_enhance.evasion in
                    let f7 = snd atk.stat_enhance.accuracy in
                    atk.stat_enhance.attack <- (i1,f1);
                    atk.stat_enhance.defense <- (i2,f2);
                    atk.stat_enhance.speed <- (i3,f3);
                    atk.stat_enhance.special_attack <- (i4,f4);
                    atk.stat_enhance.special_defense <- (i5,f5);
                    atk.stat_enhance.evasion <- (i6,f6);
                    atk.stat_enhance.accuracy <- (i7,f7);
                    newmove := PsychUpA !newmove; secondary_effects t)
    (* Flower Shield raises the Defense stat of all Grass-type Pokémon in the
      battle by one stage. *)
    | FlowerShield::t ->
        ((match ((List.mem Grass atk.current.curr_type),
              (List.mem Grass def.current.curr_type)) with
        | (true, true) -> secondary_effects ((StageBoost[(Defense,1)])::
            (StageAttack[(Defense,-1)])::t)
        | (true, false) -> secondary_effects ((StageBoost[(Defense,1)])::t)
        | (false, true) -> secondary_effects ((StageAttack[(Defense,-1)])::t)
        | _ -> ()); secondary_effects t)
    | Rototiller::t ->
        ((match ((List.mem Grass atk.current.curr_type),
              (List.mem Grass def.current.curr_type)) with
        | (true, true) -> secondary_effects ((StageBoost[(Attack,1);
            (SpecialAttack, 1)])::(StageAttack[(Attack,-1);
              (SpecialAttack, -1)])::t)
        | (true, false) -> secondary_effects ((StageBoost[(Attack ,1);
            (SpecialAttack, 1)])::t)
        | (false, true) -> secondary_effects ((StageAttack[(Attack,-1);
            (SpecialAttack, -1)])::t)
        | _ -> ()); secondary_effects t)
    (* For the move rain dance *)
    | RainDance::t -> ((match w with
                    | Rain _ | HeavyRain _ -> ()
                    | _ -> (wt.weather <- Rain 5; newmove
                      := RainDanceA !newmove));
                    secondary_effects t)
    (* For the move sand storm *)
    | SandStormMake::t -> ((match w with
                    | SandStorm _ -> ()
                    | _ -> (wt.weather <- SandStorm 5; newmove
                      := SandStormA !newmove));
                    secondary_effects t)
    (* For the move hail *)
    | HailMake::t -> ((match w with
                    | Hail _ -> ()
                    | _ -> (wt.weather <- Hail 5; newmove := HailA !newmove));
                    secondary_effects t)
    | (Encore n)::t -> let rec findForcedMove = function
                        | (ForcedMove _)::t -> true
                        | h::t -> findForcedMove t
                        | [] -> false in
                       let containsMove poke str  =
                        poke.pokeinfo.move1.name = str ||
                        poke.pokeinfo.move2.name = str ||
                        poke.pokeinfo.move3.name = str ||
                        poke.pokeinfo.move4.name = str in
                      let prevmove = if wt.terrain.side1 == t1
                                      then !prevmove2 else
                                     if wt.terrain.side1 == t2
                                      then !prevmove1 else
                                     failwith "Faulty Game Logic: Debug 1135" in
                      (if findForcedMove (snd def.current.curr_status)
                          then (newmove := EncoreFail)
                      else if containsMove def.current prevmove then
                        (def.current.curr_status <-(fst def.current.curr_status,
                            (ForcedMove (n, prevmove))::
                              (snd def.current.curr_status));
                        newmove := EncoreA !newmove; secondary_effects t)
                      else
                        (newmove := EncoreFail)
                      )
    | PainSplit::t -> let half_health = (atk.current.curr_hp +
                        def.current.curr_hp)/2 in
                      atk.current.curr_hp <- min atk.current.bhp half_health;
                      def.current.curr_hp <- min def.current.bhp half_health;
                      secondary_effects t
    (* dangerous secondary move with no other additional secondary effects *)
    | CopyPrevMove::[] -> let prevmove = if wt.terrain.side1 == t1 then
                          !prevmove2 else
                         if wt.terrain.side1 == t2 then !prevmove1 else
                         failwith "Faulty Game Logic: Debug 1135" in
                         let validmove = try (let move' = getMoveFromString
                            prevmove in not (List.mem
                              CopyPrevMove move'.secondary)) with _ -> false in
                         if validmove then
                          (let move' = getMoveFromString prevmove in
                              match move'.dmg_class with
                          | Status -> newmove := CopyPrevMoveA
                            (status_move_handler atk def (wt, t1, t2) move')
                          | _ ->  newmove := CopyPrevMoveA (move_handler atk def
                              (wt, t1, t2) move'))
                          else
                            (newmove := CopyFail)
    (* Literally just for the move taunt *)
    | TauntMove::[] ->  let rec findTaunt lst = match lst with
                    | (Taunt _)::_ -> true
                    | h::t -> findTaunt t
                    | [] -> false in
                    if findTaunt (snd def.current.curr_status) then
                      (newmove := TauntFail)
                    else
                      (newmove := TauntA !newmove;
                      def.current.curr_status <- (fst def.current.curr_status,
                          (Taunt 3)::(snd def.current.curr_status)))
    (* for the move stealth rocks *)
    | StealthRockMake::t -> if List.mem StealthRock !t2 then
                                (newmove := FailA "Stealth Rock")
                              else
                                (t2 := StealthRock::!t2;
                                newmove := StealthRockA !newmove;
                                secondary_effects t )
    | StickyWebMake::t -> if List.mem StickyWeb !t2 then
                              (newmove := FailA "StickyWeb")
                            else
                              (t2 := StickyWeb::!t2;
                              newmove := StickyWebA !newmove;
                              secondary_effects t)
    (* for the move sleep talk *)
    | SleepEffect::t ->
        let findSleep x = match x with
        | (Sleep _, _) -> true
        | _ -> false in
        if findSleep (atk.current.curr_status) then
        (let sleepmove = List.nth [atk.current.pokeinfo.move1;
            atk.current.pokeinfo.move2; atk.current.pokeinfo.move3;
            atk.current.pokeinfo.move4] (Random.int 4) in
            let prev_status = atk.current.curr_status in
            atk.current.curr_status <- (NoNon, snd atk.current.curr_status);
            match sleepmove.dmg_class with
            | Status -> (newmove := SleepTalkA
                (!newmove, status_move_handler atk def (wt, t1, t2) sleepmove))
            | _ -> (newmove := SleepTalkA
                (!newmove, move_handler atk def (wt, t1, t2) sleepmove);
                                  atk.current.curr_status <- prev_status))
        else
          (newmove := FailA "Sleep Talk")
    | VenomDrench::t ->
        if (fst (def.current.curr_status) = Poisoned) then
        (secondary_effects (StageAttack[(Attack,1)]
                          ::StageAttack[(SpecialAttack,1)]
                          ::StageAttack[(Speed,1)]::t))
        else
          (newmove := FailA "Venom Drench")
    | RandMove::t ->
        (let moves = getAllMoves atk.current.pokeinfo.name in
        let move = getMoveFromString (getRandomElement moves) in
        match move.dmg_class with
        | Status -> (newmove := RandMoveA
            (status_move_handler atk def (wt, t1, t2) move))
        | _ -> (newmove := RandMoveA (move_handler atk def (wt, t1, t2) move)))
    | ItemSwitch::t ->
            (let prev_item = atk.current.curr_item in
            atk.current.curr_item <- def.current.curr_item;
            def.current.curr_item <- prev_item;
            newmove := ItemSwapA !newmove;
            secondary_effects t)
    | WishMake::t ->
            (let rec findWish = function
              | (Wish _)::t -> true
              | h::t -> findWish t
              | [] -> false in
              if findWish !t1 then
                newmove := FailA "Wish"
              else
                (let healing = atk.current.bhp / 2 in
                t1 := (Wish (1, healing))::!t1;
                newmove := WishA !newmove)
            )
    | SelfSwitch::t ->
        if List.length atk.alive > 0 then
          ((match move.name with
          | "baton-pass" -> t1 := (BatonPass (atk.stat_enhance,
              snd atk.current.curr_status))::!t1
          | _ -> ());
            secondary_effects t)
        else
          newmove := FailA move.name
    | AbilityChange::t ->
            (match move.name with
             | "simple-beam" -> def.current.curr_abil <- "simple"
             | "worry-seed" -> def.current.curr_abil <- "insomnia"
             | "entrainment" -> def.current.curr_abil <- atk.current.curr_abil
             | "skill-swap" -> (let tmp = atk.current.curr_abil in
                               atk.current.curr_abil <- def.current.curr_abil;
                               def.current.curr_abil <- tmp)
             | _ -> ());
             newmove := AbilityChangeA !newmove;
             secondary_effects t
    | ReverseStats::t ->
        (let stats = atk.stat_enhance in
        stats.attack <- (-fst stats.attack, snd stats.attack);
        stats.defense <- (-fst stats.defense, snd stats.defense);
        stats.speed <- (-fst stats.speed, snd stats.speed);
        stats.special_attack <- (-fst stats.special_attack,
          snd stats.special_attack);
        stats.special_defense <- (-fst stats.special_defense,
          snd stats.special_defense);
        stats.evasion <- (-fst stats.evasion, snd stats.evasion);
        stats.accuracy <- (-fst stats.accuracy, snd stats.accuracy);
        secondary_effects t)
    | PowerSwap::t ->
        (let astats = atk.stat_enhance in
        let dstats = def.stat_enhance in
        let tmp = fst astats.attack in
        astats.attack <- (fst dstats.attack, snd astats.attack);
        dstats.attack <- (tmp, snd dstats.attack);
        let tmp2 = fst astats.special_attack in
        astats.special_attack <- (fst dstats.special_attack,
          snd astats.special_attack);
        dstats.special_attack <- (tmp2, snd dstats.special_attack);
          secondary_effects t)
    | GuardSwap::t ->
        (let astats = atk.stat_enhance in
        let dstats = def.stat_enhance in
        let tmp = fst astats.defense in
        astats.defense <- (fst dstats.defense, snd astats.defense);
        dstats.defense <- (tmp, snd dstats.defense);
        let tmp2 = fst astats.special_defense in
        astats.special_defense <- (fst dstats.special_defense,
          snd astats.special_defense);
        dstats.special_defense <- (tmp2, snd dstats.special_defense);
          secondary_effects t)
    | HeartSwap::t ->
        (let astats = atk.stat_enhance in
        let dstats = def.stat_enhance in
        let tmp = fst astats.attack in
        astats.attack <- (fst dstats.attack, snd astats.attack);
        dstats.attack <- (tmp, snd dstats.attack);
        let tmp2 = fst astats.special_attack in
        astats.special_attack <- (fst dstats.special_attack,
          snd astats.special_attack);
        dstats.special_attack <- (tmp2, snd dstats.special_attack);
        let tmp3 = fst astats.defense in
        astats.defense <- (fst dstats.defense, snd astats.defense);
        dstats.defense <- (tmp3, snd dstats.defense);
        let tmp4 = fst astats.special_defense in
        astats.special_defense <- (fst dstats.special_defense,
          snd astats.special_defense);
        dstats.special_defense <- (tmp4, snd dstats.special_defense);
        let tmp5 = fst astats.speed in
        astats.speed <- (fst dstats.speed, snd astats.speed);
        dstats.speed <- (tmp5, snd dstats.speed);
        let tmp6 = fst astats.evasion in
        astats.evasion <- (fst dstats.evasion, snd astats.evasion);
        dstats.evasion<- (tmp6, snd dstats.evasion);
        let tmp7 = fst astats.accuracy in
        astats.accuracy <- (fst dstats.accuracy, snd astats.accuracy);
        dstats.accuracy <- (tmp7, snd dstats.accuracy); secondary_effects t)
    | GastroAcid::t ->
        def.current.curr_abil <- "nothing";
        newmove := GastroAcidA !newmove;
        secondary_effects t
    | [] -> ()
    | _ -> failwith "Faulty Game Logic: Debug 1188"
  in
  let hit, reason' = hitMoveDueToStatus atk (!newmove) move in
  if hit then (
    let hit', newreason = hitStatus atk def move reason' in
    if hit' then (
      newmove := newreason;
      (* Returns a description of the status *)
    (match def.current.curr_abil with
    | "lightning-rod" when move.element = Electric ->
          (newmove := StatAttackA (SpecialAttack, 1,(FailA move.name));
            def.stat_enhance.special_attack <- (min 6
              (fst def.stat_enhance.special_attack + 1),
              snd def.stat_enhance.special_attack))
    | "volt-absorb" when move.element = Electric ->
          (newmove := HealOppA (FailA move.name);
          def.current.curr_hp <- min def.current.bhp (def.current.curr_hp +
          def.current.bhp/4))
    | _ ->
      secondary_effects move.secondary;
      (match atk.current.curr_item with
      | ChoiceBand | ChoiceSpecs | ChoiceScarf ->
           atk.current.curr_status <- (fst atk.current.curr_status,
           ForcedMove (1, move.name)::(snd atk.current.curr_status))
      | _ -> ()));

      !newmove)
      (* returns a move description *)
    else
      newreason)
  else
    (atk.current.curr_status <-
    (fst atk.current.curr_status,
    List.filter (fun s -> s <> Charge) (snd atk.current.curr_status)); reason')

let rec filterNonvola lst = match lst with
  (* Confusion decremented in hit move due to status *)
  | [] -> []
  | (Confusion n)::t -> (Confusion n)::(filterNonvola t)
  | Flinch::t -> filterNonvola t
  | Leeched::t -> Leeched::filterNonvola t
  (* Charge dealt with in hit attck *)
  | Charge::t -> Charge::(filterNonvola t)
  | (Substitute n)::t-> (Substitute n)::(filterNonvola t)
  | Protected::t -> UsedProtect::(filterNonvola t)
  | UsedProtect::t -> filterNonvola t
  | RechargingStatus::t -> RechargingStatus::(filterNonvola t)
  | (ForcedMove (n, s))::t -> if n = 0 then filterNonvola t else
      (ForcedMove ((n-1), s))::(filterNonvola t)
  | (Taunt n)::t -> if n = 0 then filterNonvola t else (Taunt (n-1))::
      (filterNonvola t)
  | (ForcedMoveNoSwitch (n, s))::t -> if n <= 0 then filterNonvola t else
      (ForcedMoveNoSwitch (n-1,s))::(filterNonvola t)
  | (PartialTrapping (s, n))::t -> if n <= 0 then filterNonvola t else
      (PartialTrapping (s, (n-1)))::(filterNonvola t)

let remove_some_status bp =
  let nonvola, vola = bp.curr_status in
  let newvola = filterNonvola vola in
  match nonvola with
  | Toxic n -> bp.curr_status <- (Toxic (n + 1), newvola)
  | Sleep n -> bp.curr_status <- (Sleep (n - 1), newvola)
  | _ -> bp.curr_status <- (nonvola, newvola)
(* Called after the turn ends; Decrements sleep counter; checks if Pokemon
   faints; etc... Note Pl1 always faints before Pl2*)
let handle_next_turn t w =
  (Printf.printf "Turn Ending\n%!";
  match t.current.curr_hp with
  | 0 -> if List.length t.alive > 0 then (Printf.printf "Hello\n%!"; FaintpConvert) else LoseGameConvert
  | _ -> remove_some_status t.current; Printf.printf "hello\n%!"; NoActionConvert)

(* Handles weather processing -- highest priority *)
let handle_process_weather t1 w =
  (match w.weather with
  | Sun n -> if n <= 0 then (w.weather <- ClearSkies; SunFade)
            else (w.weather <- Sun (n-1); Base)
  | Rain n -> if n <= 0 then
                (w.weather <- ClearSkies; RainFade)
              else
                (w.weather <- Rain (n-1); Base)
  | SandStorm n ->
      (if n <= 0 then (w.weather <- ClearSkies; SandStormFade)
      else
        (w.weather <- (SandStorm (n-1));
        (if (List.mem Rock t1.current.curr_type
              || List.mem Ground t1.current.curr_type
              || List.mem Steel t1.current.curr_type
              || t1.current.curr_abil = "sand-rush"
              || t1.current.curr_abil = "sand-force"
              || t1.current.curr_abil = "magic-guard")
        then
          (t1.current.curr_hp <- max 0 (t1.current.curr_hp - t1.current.bhp/16);
          SandBuffet)
        else
          Base)))
  | Hail n ->
      (if n <= 0 then
      (w.weather <- ClearSkies; HailFade)
      else
        (w.weather <- (Hail (n-1));
        if (List.mem Ice t1.current.curr_type
              || t1.current.curr_abil = "magic-guard ") then
          (t1.current.curr_hp <- max 0 (t1.current.curr_hp - t1.current.bhp/16);
          HailBuffet)
        else
          (Base)))
    | _ -> Base)

(* hanldes the processing of status conditions *)
let handle_process_status t =
  match fst t.current.curr_status with
  | Burn -> if List.mem Fire t.current.curr_type then
              (t.current.curr_status <- (NoNon, snd t.current.curr_status);
               BreakBurn)
            else if t.current.curr_abil = "magic-guard" then Base
            else
              (t.current.curr_hp <- max 0 (t.current.curr_hp - 1 * t.current.bhp / 8);
              BurnDmg)
  | Freeze -> if List.mem Ice t.current.curr_type then
              (t.current.curr_status <- (NoNon, snd t.current.curr_status);
              BreakFreeze)
            else
              Base
  | Paralysis -> if List.mem Electric t.current.curr_type then
                  (t.current.curr_status <- (NoNon, snd t.current.curr_status);
                  BreakPara)
                else
                  Base
  | Poisoned -> if List.mem Poison t.current.curr_type || List.mem Steel
                  t.current.curr_type then
                  (t.current.curr_status <- (NoNon, snd t.current.curr_status);
                  BreakPoison)
                else if t.current.curr_abil = "magic-guard" then Base
                else
                  (t.current.curr_hp <- max 0 (t.current.curr_hp - 1 * t.current.bhp/8);
                    PoisonDmg)
  | Toxic n -> if List.mem Poison t.current.curr_type || List.mem Steel
                    t.current.curr_type then
                  (t.current.curr_status <- (NoNon, snd t.current.curr_status);
                  BreakPoison)
                else if t.current.curr_abil = "magic-guard" then Base
                else
                  (let damage = t.current.bhp * (n+1) / 16 in
                  t.current.curr_hp <- max 0 (t.current.curr_hp - damage);
                  PoisonDmg)
  | _ -> Base

(* handles items -- third priority *)
let handle_process_items t =
  match t.current.curr_item with
  | Leftovers ->  (t.current.curr_hp <- min t.current.bhp (t.current.curr_hp + t.current.bhp/16);
                  LeftOversHeal)
  | _ -> Base

(* handles abilities -- fourth priority *)
let handle_process_ability t =
   match t.current.curr_abil with
  | "speed-boost" -> let stage, multiplier = t.stat_enhance.speed in
                    let boost = stage + 1 in
                    if boost >= 6 then
                      Base
                    else
                      (t.stat_enhance.speed <- (boost, multiplier);
                      (SpeedBoost))
  | _ -> Base

(* handles nonvolatile statuses -- fifth priority *)
let handle_process_nonvola t1 t2  =
  let rec helper lst descript = match lst with
  | [] -> descript
  | Leeched::t ->
        if t1.current.curr_abil <> "magic-guard" then
          (if t2.current.curr_hp > 0 then
            (let damage = min t1.current.curr_hp t1.current.bhp / 16 in
            t1.current.curr_hp <- t1.current.curr_hp - damage;
            t2.current.curr_hp <- min t2.current.bhp t2.current.curr_hp + damage;
            helper t (LeechDmg descript))
          else
            helper t descript)
        else
          helper t descript
  | (PartialTrapping (s, n))::t ->
        (t1.current.curr_hp <- max 0 (t1.current.curr_hp - t1.current.bhp / 8);
        helper t (TrapDamage (s,descript)))
  | (Taunt 0)::t -> helper t (TauntFade descript)
  | h::t -> helper t descript in
  helper (snd t1.current.curr_status) Base

(* handles terrain changes -- last priority *)
let handle_process_terrain t ter =
  let rec helper lst acc descript  = match lst with
  | (LightScreen n)::t' -> if n = 0 then
                           helper t' acc (LightScreenFade descript)
                          else
                            helper t' ((LightScreen (n-1))::acc) descript
  | (Reflect n)::t' -> if n = 0 then
                        helper t' acc (ReflectFade descript)
                       else
                        helper t' ((Reflect (n-1))::acc) descript
  | (Wish (n, heal)::t') -> if n = 0 then
                            (t.current.curr_hp <- min t.current.bhp
                                (t.current.curr_hp + heal);
                            helper t' acc (WishEnd descript) )
                          else
                         (helper t' ((Wish ((n-1), heal))::acc) descript)
  | h::t' -> helper t' (h::acc) descript
  | [] -> (acc, descript) in
  let new_ter, descript = helper !ter [] Base in
  ter := new_ter; descript

(* Gets entry hazard damage. *)
let getEntryHazardDmg t ter1 new_message=
  let rec helper acc lst = match lst with
  | [] -> acc
  | StickyWeb::t' -> if List.mem Flying t.current.curr_type then helper acc t'
                    else
                      (let (s, f) = t.stat_enhance.speed in
                      t.stat_enhance.speed <- ((max (-6) (s-1)), f);
                      new_message := StickyWebSlow !new_message;
                      helper acc t')
  | (Spikes n)::t' ->if List.mem Flying t.current.curr_type ||
      t.current.curr_abil = "levitate" then helper acc t'
                    else
                      (new_message := SpikeDamage !new_message;
                      (if n = 1 then (helper (0.125 +. acc) t')
                      else if n = 2 then (helper (1. /. 6. +. acc) t')
                      else (helper (0.25 +. acc) t')))
  | StealthRock::t' -> (new_message := StealthRocksDamage !new_message;
                        let typeeffect = List.fold_left (fun acc x ->
                        acc *. getElementEffect
                        Rock x) 1. t.current.curr_type in
                        match typeeffect with
                        | 0.25 -> helper (0.03125 +. acc) t'
                        | 0.50 -> helper (0.0625 +. acc) t'
                        | 2. -> helper (0.25 +. acc) t'
                        | 4. -> helper (0.5 +. acc) t'
                        | _ -> helper (0.125 +. acc) t')
  | (ToxicSpikes n)::t' -> if List.mem Flying t.current.curr_type ||
    t.current.curr_abil = "levitate" then helper acc t'
                      else
                        ((match t.current.curr_status with
                        | (NoNon, x) -> (new_message := ToxicSpikePoison
                            !new_message;
                            if n = 1 then (t.current.curr_status <-
                            (Poisoned, x)) else (t.current.curr_status <-
                                (Toxic 0, x)))
                        | _ -> ()); helper acc t')
  | h::t -> helper acc t in
  let damage = int_of_float (helper 0. !ter1 *. float_of_int t.current.bhp) in
  t.current.curr_hp <- max 0 (t.current.curr_hp - damage)

(* test for forced moves *)
let rec getForcedMove lst =
  match lst with
  | (ForcedMove (_, s))::t -> (true, s)
  | h::t -> getForcedMove t
  | [] -> (false, "")

(* gets the overall speed of the Pokemon *)
let getSpeed t w =
  (float_of_int t.current.bspeed *.
    getStageAD (fst t.stat_enhance.speed) *. (snd t.stat_enhance.speed) *.
    (if t.current.curr_item = ChoiceScarf then 1.5 else 1.0)) *.
  (match w.weather with
    | Rain _ | HeavyRain _  -> (if t.current.curr_abil = "swift-swim" then
                                  2.
                                else
                                  1.)
    | Sun _ | HarshSun _ -> (if t.current.curr_abil = "chlorophyll" then
                                2.
                            else
                                1.)
    | SandStorm _-> (if t.current.curr_abil = "sand-rush" then
                        2.
                    else
                      1.)
    | _ -> 1.)

(* gets which Pokemon goes first based on move *)
let getFaster t1 t2 move1 move2 w =
  if move1.priority > move2.priority then
    1
  else if move1.priority = move2.priority then
    if getSpeed t1 w > getSpeed t2 w then
      1
    else if getSpeed t1 w = getSpeed t2 w then
      0
    else
      -1
  else
    -1

(* Switches poke handlers *)
let switchPokeHandler faint nextpoke t ter1 t2 w new_message =
  let rec findBatonPass = function
  | BatonPass (enhance, vola)::t -> Some (enhance, vola)
  | h::t -> findBatonPass t
  | [] -> None in
  let rec filterBatonPass = function
  | (BatonPass _)::t -> filterBatonPass t
  | h::t -> h::(filterBatonPass t)
  | [] -> []  in
  let prevPoke = t.current in
  let switchPoke, restPoke = findBattlePoke t.alive nextpoke in
  let new_stat_enhance, new_vola_status = match findBatonPass !ter1 with
  | Some (e, v) -> e, v
  | None -> (switchOutStatEnhancements t, []) in
  t.stat_enhance <- new_stat_enhance;
  t.current.curr_status <- switchOutStatus t.current;
  t.current.curr_abil <- t.current.pokeinfo.ability;
  t.current.curr_type <- t.current.pokeinfo.element;
  t.current <- switchPoke;
  t.current.curr_status <- (fst t.current.curr_status, new_vola_status);
  (if faint then
    (t.dead <- prevPoke::t.dead; t.alive <- restPoke)
  else
    t.alive <- prevPoke::restPoke);
  getEntryHazardDmg t ter1 new_message;
  ter1 := filterBatonPass !ter1;
  if t.current.curr_hp > 0 then
    (match t.current.curr_abil with
    | "intimidate" -> t2.stat_enhance.attack <- (fst t2.stat_enhance.attack - 1,
      snd t2.stat_enhance.attack); new_message := Intimidate !new_message
    | "drizzle" -> w.weather <- Rain 5; new_message := MakeItRain !new_message
    | "drought" -> w.weather <- Sun 5; new_message := MakeSunny !new_message
    | "snow-warning" -> w.weather <- Hail 5; new_message := MakeBlizzard
                        !new_message
    | "sand-stream" -> w.weather <- SandStorm 5;
                        new_message := MakeSandStorm !new_message
    | _ -> ())

 let handle_action t1 t2 ter1 ter2 w x = match x with
  | UseAttack a ->  if t1.current.curr_hp > 0 && t2.current.curr_hp > 0 then
                      (let curr_move = findBattleMove t1.current.pokeinfo a in
                      let new_message =  (match curr_move.dmg_class with
                      | Status -> status_move_handler t1 t2 (w, ter1,
                              ter2) curr_move
                      | _ -> move_handler t1 t2 (w, ter1,
                              ter2) curr_move) in
                      if List.mem SelfSwitch curr_move.secondary &&
                          List.length t1.alive > 0 then
                          (wait_for_input command_queue;
                          ForceChoose new_message)
                      else if List.mem ForceSwitch curr_move.secondary &&
                        List.length t2.alive > 0 then
                        (empty_out command_queue;
                        (if ter1 = w.terrain.side1 then
                          enqueue (Player2 (Poke (getRandomPoke t2))) command_queue
                        else
                          enqueue (Player1 (Poke (getRandomPoke t2))) command_queue);
                        UsedMove new_message
                        )
                      else
                        (UsedMove new_message))
                    else
                      NoAction
  | FaintPoke p | Poke p -> let fnt = x = FaintPoke p in
                   let new_message = ref (SwitchedInto p) in
                   let () = switchPokeHandler fnt p t1 ter1 t2 w new_message in
                   if (t1.current.curr_hp = 0) then
                    (empty_out command_queue;
                    if List.length t1.alive = 0 then
                      (LoseGame)
                    else
                      (SPoke (SFaint !new_message))
                    )
                  else
                    SPoke !new_message
  | _ -> failwith "Faulty Game Logic"

let handle_calculation t1 t2 ter1 ter2 w x =
    match x with
    | PreprocessWeather when t1.current.curr_hp > 0 -> handle_process_weather t1 w
    | PreprocessStatus when t1.current.curr_hp > 0 -> handle_process_status t1
    | PreprocessItem when t1.current.curr_hp > 0 -> handle_process_items t1
    | PreprocessAbility when t1.current.curr_hp > 0 -> handle_process_ability t1
    | PreprocessNonVola when t1.current.curr_hp > 0 -> handle_process_nonvola t1 t2
    | PreprocessTerrain when t1.current.curr_hp > 0  -> handle_process_terrain t1 ter1
    | ProcessNextTurn -> handle_next_turn t1 w
    | _ -> Base

(* handles a single action *)
let rec handle_single_action t1 t2 w m =
  let command = dequeue command_queue in
  match command with
  | Player1 x  -> let new_move = handle_action t1 t2 w.terrain.side1
                    w.terrain.side2 w x in
                  m := Pl1 (new_move)
  | Player2 x -> let new_move = handle_action t2 t1 w.terrain.side2
                    w.terrain.side1 w x in
                  m := Pl2 (new_move)
  | Process1 ProcessNextTurn -> Printf.printf "NOTwtf\n%!";
          (match handle_calculation t1 t2 w.terrain.side1 w.terrain.side2 w ProcessNextTurn with
          | FaintpConvert -> m := Pl1 (Faintp)
          | LoseGameConvert -> m := Pl1 (LoseGame)
          | _ -> m := Pl1 (NoAction))
  | Process2 ProcessNextTurn -> Printf.printf "notwtf\n%!";
          (match handle_calculation t2 t1 w.terrain.side2 w.terrain.side1 w ProcessNextTurn with
          | FaintpConvert -> m := Pl2 (Faintp)
          | LoseGameConvert -> m := (Pl2 LoseGame)
          | _ -> m := Pl2 (NoAction))
  | Process1 x -> Printf.printf "WTF\n%!";
                  let new_move = handle_calculation t1 t2 w.terrain.side1
                    w.terrain.side2 w x in
                  if new_move = Base then handle_single_action t1 t2 w m else
                  m := Pl1 (EndMove new_move)
  | Process2 x -> Printf.printf "wtf\n%!";
                  let new_move = handle_calculation t2 t1 w.terrain.side2
                    w.terrain.side1 w x in
                  if new_move = Base then handle_single_action t1 t2 w m else
                  m := Pl2 (EndMove new_move)
  (* Queue should be empty at this point -- Preprocess is always the last
    thing in the queue *)
  | Preprocess -> (if getSpeed t1 w > getSpeed t2 w then
                          (command_queue := [
                            Process1 PreprocessWeather;
                            Process2 PreprocessWeather;
                            Process1 PreprocessStatus;
                            Process2 PreprocessStatus;
                            Process1 PreprocessItem;
                            Process2 PreprocessItem;
                            Process1 PreprocessAbility;
                            Process2 PreprocessAbility;
                            Process1 PreprocessNonVola;
                            Process2 PreprocessNonVola;
                            Process1 PreprocessTerrain;
                            Process2 PreprocessTerrain;
                            Process1 ProcessNextTurn;
                            Process2 ProcessNextTurn])
                  else
                          (command_queue := [
                            Process2 PreprocessWeather;
                            Process1 PreprocessWeather;
                            Process2 PreprocessStatus;
                            Process1 PreprocessStatus;
                            Process2 PreprocessItem;
                            Process1 PreprocessItem;
                            Process2 PreprocessAbility;
                            Process1 PreprocessAbility;
                            Process2 PreprocessNonVola;
                            Process1 PreprocessNonVola;
                            Process2 PreprocessTerrain;
                            Process1 PreprocessTerrain;
                            Process1 ProcessNextTurn;
                            Process2 ProcessNextTurn]);
                  handle_single_action t1 t2 w m)
  | Buffer -> take_in_input command_queue; handle_single_action t1 t2 w m


(* rewritten main action handler for the game;
  puts the two commands in queue in the order of execution and also update
  the previous poke values. *)
let handle_action t1 t2 w action1 action2 =
  let () = prevpoke1 := {t1.current with curr_hp = t1.current.curr_hp};
    prevpoke2 := {t2.current with curr_hp = t2.current.curr_hp} in
  (match action1 with
  | Poke p ->
    (match action2 with
    | Poke p2 -> if getSpeed t1 w > getSpeed t2 w then
                    (enqueue (Player1 (Poke p)) command_queue;
                    enqueue (Player2 (Poke p2)) command_queue)
                 else
                    (enqueue (Player2 (Poke p2)) command_queue;
                    enqueue (Player1 (Poke p)) command_queue)
    | UseAttack a' -> let forcedmove1, forcedmove1name =
                      getForcedMove (snd t2.current.curr_status) in
                      let a = if forcedmove1 then forcedmove1name else a' in
                      if a = "pursuit" then
                        (enqueue (Player2 (UseAttack a)) command_queue;
                        enqueue (Player1 (Poke p)) command_queue)
                     else
                        (enqueue (Player1 (Poke p)) command_queue;
                        enqueue (Player2 (UseAttack a)) command_queue)
    | NoMove | NoPreprocess -> (enqueue (Player1 (Poke p)) command_queue)
    | _ -> failwith "Faulty Game Logic: Debug 2482")
  | UseAttack a' ->
    let forcedmove1, forcedmove1name =
      getForcedMove (snd t1.current.curr_status) in
    let a = if forcedmove1 then forcedmove1name else a' in
    (match action2 with
     | Poke p -> if a = "pursuit" then
                    (enqueue (Player1 (UseAttack a)) command_queue;
                    enqueue (Player2 (Poke p)) command_queue)
                 else
                    (enqueue (Player2 (Poke p)) command_queue;
                    enqueue (Player1 (UseAttack a)) command_queue)
      | UseAttack a2'->let forcedmove2, forcedmove2name =
                           getForcedMove (snd t2.current.curr_status) in
                      let a2 = if forcedmove2 then forcedmove2name else a2' in
                      let curr_move = findBattleMove t1.current.pokeinfo a in
                      let curr_move' = findBattleMove t2.current.pokeinfo a2 in
                      let t2faster () =
                            (enqueue (Player2 (UseAttack a2)) command_queue;
                              enqueue (Player1 (UseAttack a)) command_queue) in
                      let t1faster () =
                            (enqueue (Player1 (UseAttack a)) command_queue;
                              enqueue (Player2 (UseAttack a2)) command_queue) in
                      (match getFaster t1 t2 curr_move curr_move' w with
                      | -1 -> t2faster ()
                      | 0 -> if Random.int 2 = 0 then
                                t1faster ()
                              else
                                t2faster ()
                      | 1 -> t1faster ()
                      | _ -> failwith "Faulty Game Logic: Debug 2512")
        | NoMove -> enqueue (Player1 (UseAttack a)) command_queue
        | _ -> failwith "Faulty Game Logic: Debug 2514")
  | NoMove -> (match action2 with
                  | FaintPoke p -> enqueue (Player2 (FaintPoke p)) command_queue
                  | Poke p -> enqueue (Player2 (Poke p)) command_queue
                  | UseAttack a' -> let forcedmove2, forcedmove2name =
                          getForcedMove (snd t2.current.curr_status) in
                          let a = if forcedmove2 then forcedmove2name else a' in
                          enqueue (Player2 (UseAttack a)) command_queue
                  | NoMove -> ()
                  | _ -> failwith "Faulty Game Logic: Debug 2523")
  | FaintPoke p -> (match action2 with
                    | FaintPoke p' ->
                        (if getSpeed t1 w > getSpeed t2 w then
                          (enqueue (Player1 (FaintPoke p)) command_queue;
                          enqueue (Player2 (FaintPoke p')) command_queue)
                        else
                          (enqueue (Player2 (FaintPoke p')) command_queue;
                          enqueue (Player1 (FaintPoke p)) command_queue))
                    | _ -> enqueue (Player1 (FaintPoke p)) command_queue)
  | NoPreprocess -> (match action2 with
                  | FaintPoke p -> enqueue (Player2 (FaintPoke p)) command_queue
                  | Poke p -> enqueue (Player2 (Poke p)) command_queue
                  | UseAttack a' -> let forcedmove2, forcedmove2name =
                          getForcedMove (snd t2.current.curr_status) in
                          let a = if forcedmove2 then forcedmove2name else a' in
                          enqueue (Player2 (UseAttack a)) command_queue
                  | NoMove -> ()
                  | NoPreprocess -> ()
                  | _ -> failwith "Faulty Game Logic: Debug 2523")
  | AIMove -> failwith "Faulty Game Logic: Debug 2339"
  )

(* Main loop for 1 player -- gets input from AI *)
let rec main_loop_1p engine gui_ready ready ready_gui () =
  let t1, t2, w, m = match get_game_status engine with
    | Battle InGame (t1, t2, w, m) -> t1, t2, w, m
    | _ -> failwith "Faulty Game Logic" in
  upon (Ivar.read !gui_ready) (* Replace NoMove with ai move later *)
    (fun (cmd1, cmd2) -> let c1 = unpack cmd1 in
                         let c2 = match (unpack cmd2) with
                          | AIMove -> UseAttack (Ai.get_move_better t1.current
                              t2.current)
                          | NoMove -> NoMove
                          | UseAttack s -> UseAttack s
                          | NoPreprocess -> NoPreprocess
                          | Poke s -> Poke s
                          | FaintPoke _ -> FaintPoke (Ai.replace_dead_better
                              t1.current t2.alive) in
                         let () = handle_action t1 t2 w c1 c2 in
                         (if c1 = NoPreprocess || c2 = NoPreprocess then
                            ()
                          else
                            (enqueue (Preprocess) command_queue));
                         let () = handle_single_action t1 t2 w m in
                         gui_ready := Ivar.create ();
                         let queue_empty = is_empty command_queue in
                         Ivar.fill !ready_gui (queue_empty || List.mem Buffer !command_queue);
                         (main_loop_1p engine gui_ready ready ready_gui ()));
   Printf.printf "Debug %d \n%!" (Scheduler.cycle_count ())

(* Main loop for 0 player -- gets input from AI *)
let rec main_loop_0p engine gui_ready ready ready_gui () =
  let t1, t2, w, m = match get_game_status engine with
    | Battle InGame (t1, t2, w, m) -> t1, t2, w, m
    | _ -> failwith "Faulty Game Logic" in
  upon (Ivar.read !gui_ready) (* Replace NoMove with ai move later *)
    (fun (cmd1, cmd2) -> let c1 = match (unpack cmd1) with
                          | AIMove -> UseAttack (Ai.get_move_better t2.current
                              t1.current)
                          | NoMove -> NoMove
                          | UseAttack s -> UseAttack s
                          | NoPreprocess -> NoPreprocess
                          | Poke s -> Poke s
                          | FaintPoke _ -> FaintPoke (Ai.replace_dead_better
                              t2.current t1.alive) in
                         let c2 = match (unpack cmd2) with
                          | AIMove -> UseAttack (Ai.get_move_better t1.current
                              t2.current)
                          | NoMove -> NoMove
                          | UseAttack s -> UseAttack s
                          | NoPreprocess -> NoPreprocess
                          | Poke s -> Poke s
                          | FaintPoke _ -> FaintPoke (Ai.replace_dead_better
                              t1.current t2.alive) in
                         let () = handle_action t1 t2 w c1 c2 in
                         (if c1 = NoPreprocess || c2 = NoPreprocess then
                            ()
                          else
                            (enqueue (Preprocess) command_queue));
                         let () = handle_single_action t1 t2 w m in
                         gui_ready := Ivar.create ();
                         let queue_empty = is_empty command_queue in
                         Ivar.fill !ready_gui (queue_empty || List.mem Buffer !command_queue);
                         (main_loop_0p engine gui_ready ready ready_gui ()));
   Printf.printf "Debug %d \n%!" (Scheduler.cycle_count ())


(* Main loop for 2 player -- gets input from two players *)
let rec main_loop_2p engine gui_ready ready ready_gui () =
  let t1, t2, w, m = match get_game_status engine with
    | Battle InGame (t1, t2, w, m) -> t1, t2, w, m
    | _ -> failwith "Faulty Game Logic" in
  upon (Ivar.read !gui_ready)
    (fun (cmd1, cmd2) -> let c1 = unpack cmd1 in let c2 = unpack cmd2 in
                          let () = handle_action t1 t2 w c1 c2 in
                         (if c1 = NoPreprocess || c2 = NoPreprocess then
                            ()
                          else
                            (enqueue (Preprocess) command_queue));
                         let () = handle_single_action t1 t2 w m in
                         gui_ready := Ivar.create ();
                         let queue_empty = is_empty command_queue in
                         Ivar.fill !ready_gui (queue_empty || List.mem Buffer !command_queue);
                         (main_loop_2p engine gui_ready ready ready_gui ()));
    Printf.printf "Debug %d \n%!" (Scheduler.cycle_count ())

(* Main controller for random zero player *)
let rec main_controller_random0p engine gui_ready ready ready_gui=
 Printf.printf "Initializing battle r1p\n%!";
  let team1 = getRandomTeam `Random in
  let team2 = getRandomTeam `Random in
  let battle = initialize_battle team1 team2 in
  let () = engine := Ivar.create (); Ivar.fill !engine battle;
  Ivar.fill !ready_gui true in
  main_loop_0p engine gui_ready ready ready_gui ()

(* Main controller for random one player *)
let rec main_controller_random1p engine gui_ready ready ready_gui=
 Printf.printf "Initializing battle r1p\n%!";
  let team1 = getRandomTeam `Random in
  let team2 = getRandomTeam `Random in
  let battle = initialize_battle team1 team2 in
  let () = engine := Ivar.create (); Ivar.fill !engine battle;
  Ivar.fill !ready_gui true in
  main_loop_1p engine gui_ready ready ready_gui ()

(* Main controller for random two player *)
let rec main_controller_random2p engine gui_ready ready ready_gui =
  Printf.printf "Initializing battle r2p\n%!";
  let team1 = getRandomTeam `Random in
  let team2 = getRandomTeam `Random in
  let battle = initialize_battle team1 team2 in
  let () = engine := Ivar.create (); Ivar.fill !engine battle;
  Ivar.fill !ready_gui true in
  main_loop_2p engine gui_ready ready ready_gui ()

(* Main controller for 1 player preset battle *)
let rec main_controller_preset1p engine gui_ready ready ready_gui t =
  Printf.printf "Initializing battle p1p\n%!";
  let stat_enhance = {attack=(0,1.); defense=(0,1.); speed=(0,1.);
      special_attack=(0,1.); special_defense=(0,1.); evasion=(0,1.);
      accuracy=(0,1.)} in
  let team1' = List.map getBattlePoke t in
  let team1 = {current = List.hd team1'; alive = List.tl team1'; dead = [];
                  stat_enhance} in
  let team2 = getRandomTeam `Preset in
  let battle = initialize_battle team1 team2 in
  let () = engine := Ivar.create (); Ivar.fill !engine battle;
  Ivar.fill !ready_gui true in
  main_loop_1p engine gui_ready ready ready_gui ()

(* Main controller for 2 player preset battle *)
let rec main_controller_preset2p engine gui_ready ready ready_gui t t'=
  Printf.printf "Initializing battle p1p\n%!";
  let stat_enhance = {attack=(0,1.); defense=(0,1.); speed=(0,1.);
      special_attack=(0,1.); special_defense=(0,1.); evasion=(0,1.);
      accuracy=(0,1.)} in
  let stat_enhance2 = {attack=(0,1.); defense=(0,1.); speed=(0,1.);
      special_attack=(0,1.); special_defense=(0,1.); evasion=(0,1.);
      accuracy=(0,1.)} in
  let team1' = List.map getBattlePoke t in
  let team1 = {current = List.hd team1'; alive = List.tl team1'; dead = [];
                  stat_enhance} in
  let team2' = List.map getBattlePoke t' in
  let team2 = {current = List.hd team2'; alive = List.tl team2'; dead = [];
                stat_enhance = stat_enhance2} in
  let battle = initialize_battle team1 team2 in
  let () = engine := Ivar.create (); Ivar.fill !engine battle;
  Ivar.fill !ready_gui true in
  main_loop_1p engine gui_ready ready ready_gui ()

(* Main controller for tournament mode *)
let rec main_controller_tourn engine gui_ready ready ready_gui t =
  Printf.printf "Initializing battle t\n%!";
  let stat_enhance = {attack=(0,1.); defense=(0,1.); speed=(0,1.);
      special_attack=(0,1.); special_defense=(0,1.); evasion=(0,1.);
      accuracy=(0,1.)} in
  let team1' = List.map getBattlePoke t in
  let team1 = {current = List.hd team1'; alive = List.tl team1'; dead = [];
                  stat_enhance} in
  let team2 = getRandomTeam `Tournament in
  let battle = initialize_battle team1 team2 in
  let () = engine := Ivar.create (); Ivar.fill !engine battle;
  Ivar.fill !ready_gui true in
  main_loop_1p engine gui_ready ready ready_gui ()

(* Initialize controller -- called by Game.ml *)
let initialize_controller (engine, battle_engine) =
  let battle_status, gui_ready, ready, ready_gui = battle_engine in
  upon (Ivar.read !battle_status) (Printf.printf "Initializing battle\n%!";
    fun s -> match s with
    | Random0p -> (main_controller_random0p engine gui_ready ready ready_gui)
    | Random1p -> (main_controller_random1p engine gui_ready ready ready_gui)
    | Random2p -> (main_controller_random2p engine gui_ready ready ready_gui)
    | TournBattle t ->
        (main_controller_tourn engine gui_ready ready ready_gui t)
    | Preset1p t ->
        (main_controller_preset1p engine gui_ready ready ready_gui t)
    | Preset2p (t, t') ->
        main_controller_preset2p engine gui_ready ready ready_gui t t');
  ()