open Info

(* [get_move_random poke] gets the name of a random move for the Pokemon [poke].
 *
 *  - [poke] is a battle Pokemon.
 *)
let get_move_random (poke : battle_poke) : string =
  match Random.int 4 with
  | 0 -> poke.pokeinfo.move1.name
  | 1 -> poke.pokeinfo.move2.name
  | 2 -> poke.pokeinfo.move3.name
  | 3 -> poke.pokeinfo.move4.name
  | _ -> failwith "Does not occur." (* Here to satisfy the compiler >:( *)

(* Calculate weight of a move by it's accuracy and power *)
let calculate_move_weights (pmove : move) : int =
  (pmove.accuracy * pmove.power) / 100

(* Assign status moves a weight, 15% of the old total. *)
let assign_status_weight (weight : int) (total : int) : int =
  if weight = 0 then (int_of_float (0.15 *. float_of_int (total)))
  else weight

(* Usually the picks the better move (one with a greater weight). If all moves
 * are status moves, then randomly pick a status move. *)
let get_move_better (poke : battle_poke) : string =
  let m1 = calculate_move_weights poke.pokeinfo.move1 in
  let m2 = calculate_move_weights poke.pokeinfo.move2 in
  let m3 = calculate_move_weights poke.pokeinfo.move3 in
  let m4 = calculate_move_weights poke.pokeinfo.move4 in
  let total = m1+m2+m3+m4 in
  if total = 0 then get_move_random poke
  else
    (let w1 = assign_status_weight m1 total in
    let w2 = assign_status_weight m2 total in
    let w3 = assign_status_weight m3 total in
    let w4 = assign_status_weight m4 total in
    let newtotal = w1+w2+w3+w4 in
    let randnum = Random.int newtotal in
    if (randnum < w1) then poke.pokeinfo.move1.name
    else if (randnum >= w1 && randnum < (w1+w2)) then poke.pokeinfo.move2.name
    else if (randnum >= (w1+w2) && randnum < (w1+w2+w3))
         then poke.pokeinfo.move3.name
    else poke.pokeinfo.move4.name)

(* [replace_dead_random lst] randomly chooses a Pokemon given a non-empty
 * list [lst] of alive ones.
 *
 *  - [lst] is a list of currently-alive battle Pokemon.
 *)
let replace_dead_random (lst : battle_poke list) : string =
  let n = Random.int (List.length lst) in (List.nth lst n).pokeinfo.name

(* [get_advantage_value elst1 elst2] returns the float value representing
 * the type advantage of the element of the Pokemon [elst1] against the element
 * of the opposing Pokemon [elst2].
 *
 *  - [elst1] is a list of up to two element types for Pokemon 1.
 *  - [elst2] is a list of up to two element types for Pokemon 2.
 *)
let rec get_advantage_value (elst1:element list) (elst2:element list) : float =
  match elst1 with
  | [] -> 1.
  | h::t -> (List.fold_left (fun acc x -> acc *. Pokemon.getElementEffect h x)
                1. elst2) *. (get_advantage_value t elst2)

(* [replace_dead_better poke alivelst] returns the string name of the first
 * Pokemon in the non-empty list of alive Pokemon [alivelst] that has the
 * maximum type advantage against the opposing Pokemon [poke]. If there are
 * no alive Pokemon with a type advantage, a Pokemon is chosen randomly.
 *
 *  - [poke] is a battle Pokemon to be compared against.
 *  - [alivelst] is a list of battle Pokemon.
 *)
let replace_dead_better (poke:battle_poke) (alivelst:battle_poke list) : string =
  let type2 = poke.pokeinfo.element in
  let rec get_poke_max_advantage p maxv = function
    | [] -> (p,maxv)
    | h::t ->
        let type1 = h.pokeinfo.element in
        let currv = get_advantage_value type1 type2 in
        if (currv > maxv) then get_poke_max_advantage h currv t
        else get_poke_max_advantage p maxv t
  in
  match alivelst with
  | [] -> failwith "Precondition Violated!"
  | h::t ->
      let type1 = h.pokeinfo.element in
      let currv = get_advantage_value type1 type2 in
      let bestpokemax = get_poke_max_advantage h currv t in
      if (snd bestpokemax > 1.) then (fst bestpokemax).pokeinfo.name
      else replace_dead_random alivelst
