open Info

(* [get_move_random poke] gets the string name of a random move for the
 * Pokemon [poke].
 *
 *  - [poke] is a battle Pokemon.
 *)
let get_move_random (poke:battle_poke) : string =
  match Random.int 4 with
  | 0 -> poke.pokeinfo.move1.name
  | 1 -> poke.pokeinfo.move2.name
  | 2 -> poke.pokeinfo.move3.name
  | 3 -> poke.pokeinfo.move4.name
  | _ -> failwith "Does not occur." (* Here to satisfy the compiler >:( *)

(* [calculate_move_weight pmove elst] returns the integer weight of a
 * move [pmove] based on its accuracy, power, and type advantage against
 * the opposing Pokemon's type [elst].
 *
 *  - [pmove] is the move whose weight to determine.
 *  - [elst] is the opposing Pokemon's type, a list of elements.
 *)
let calculate_move_weight (pmove:move) (elst:element list) : int =
  let tweight = List.fold_left (fun acc x ->
                    acc *. Pokemon.getElementEffect pmove.element x) 1. elst in
  int_of_float ((float_of_int (pmove.accuracy * pmove.power))
      *. tweight /. 100.)

(* [assign_status_weight pmove weight total] is a helper function for
 * [get_move_better] that returns a new weight that is 15% of the old total
 * weight of the four moves if the move is a status move. Otherwise, returns
 * the original weight.
 *
 *  - [pmove] is the move whose weight to determine.
 *  - [weight] is the integer weight of the move.
 *  - [total] is the total weight of the four moves of the Pokemon.
 *)
let assign_status_weight (pmove:move) (weight:int) (total:int) : int =
  match pmove.dmg_class with
  | Status -> int_of_float (0.15 *. float_of_int (total))
  | _ -> weight

(* [get_move_better poke1 poke2] returns the string name of one of four moves
 * of the Pokemon [poke2] to be used against the opposing Pokemon [poke1].
 * The probability of the selected move is based on the weight assigned to
 * each of the four moves. If all four moves are status moves, then a move
 * is selected randomly (equal weight).
 *
 *  - [poke1] is a battle Pokemon.
 *  - [poke2] is a battle Pokemon.
 *)
let get_move_better (poke1:battle_poke) (poke2:battle_poke) : string =
  let m1 = calculate_move_weight poke2.pokeinfo.move1 poke1.pokeinfo.element in
  let m2 = calculate_move_weight poke2.pokeinfo.move2 poke1.pokeinfo.element in
  let m3 = calculate_move_weight poke2.pokeinfo.move3 poke1.pokeinfo.element in
  let m4 = calculate_move_weight poke2.pokeinfo.move4 poke1.pokeinfo.element in
  let total = m1+m2+m3+m4 in
  if (total = 0) then get_move_random poke2
  else
    (let w1 = assign_status_weight poke2.pokeinfo.move1 m1 total in
    let w2 = assign_status_weight poke2.pokeinfo.move2 m2 total in
    let w3 = assign_status_weight poke2.pokeinfo.move3 m3 total in
    let w4 = assign_status_weight poke2.pokeinfo.move4 m4 total in
    let newtotal = w1+w2+w3+w4 in
    let randnum = Random.int newtotal in
    if (randnum < w1) then poke2.pokeinfo.move1.name
    else if (randnum >= w1 && randnum < (w1+w2)) then poke2.pokeinfo.move2.name
    else if (randnum >= (w1+w2) && randnum < (w1+w2+w3))
         then poke2.pokeinfo.move3.name
    else poke2.pokeinfo.move4.name)

(* [replace_dead_random lst] randomly chooses a Pokemon given a non-empty
 * list [lst] of alive ones.
 *
 *  - [lst] is a list of currently-alive battle Pokemon.
 *)
let replace_dead_random (lst:battle_poke list) : string =
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
 * maximum type advantage against the opposing Pokemon [poke].
 *
 *  - [poke] is a battle Pokemon to be compared against.
 *  - [alivelst] is a list of battle Pokemon.
 *)
let replace_dead_better (poke:battle_poke) (alivelst:battle_poke list) : string =
  let type2 = poke.pokeinfo.element in
  let rec get_poke_max_advantage p maxv = function
    | [] -> p
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
      let bestpoke = get_poke_max_advantage h currv t in
      bestpoke.pokeinfo.name