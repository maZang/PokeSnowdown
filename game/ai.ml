open Info

(* [getRandomMove poke] gets the name of a random move for the Pokemon [poke].
 *
 *  - [poke] is a battle Pokemon.
 *)
let getRandomMove (poke : battle_poke) : string =
  match Random.int 4 with
  | 0 -> poke.pokeinfo.move1.name
  | 1 -> poke.pokeinfo.move2.name
  | 2 -> poke.pokeinfo.move3.name
  | 3 -> poke.pokeinfo.move4.name
  | _ -> failwith "Does not occur." (* Here to satisfy the compiler >:( *)

(* Calculate weight of a move by it's accuracy and power *)
let calculateMoveWeights (pmove: move) : int =
  (pmove.accuracy * pmove.power) / 100

(* Usually the picks the better move (one with a greater weight). If all moves
 * are status moves, then randomly pick a status move. Doesn't pick a status
 * move if there is at least 1 non-status move. *)
let getBetterMove (poke : battle_poke) : string =
  let m1 = calculateMoveWeights poke.pokeinfo.move1 in
  let m2 = calculateMoveWeights poke.pokeinfo.move2 in
  let m3 = calculateMoveWeights poke.pokeinfo.move3 in
  let m4 = calculateMoveWeights poke.pokeinfo.move4 in
  let total = m1+m2+m3+m4 in
  if total=0 then getRandomMove poke
  else
    (let randum = Random.int total in
    if (randum < m1) then poke.pokeinfo.move1.name
    else if (randum >= m1 && randum < (m1+m2)) then poke.pokeinfo.move2.name
    else if (randum >= (m1+m2) && randum < (m1+m2+m3))
         then poke.pokeinfo.move3.name
    else poke.pokeinfo.move4.name)

(* [replaceDead lst] randomly chooses a Pokemon given a list [lst] of alive
 * ones.
 *
 *  - [lst] is a list of currently-alive battle Pokemon.
 *)
let replaceDead (lst : battle_poke list) : string =
  let n = Random.int (List.length lst) in (List.nth lst n).pokeinfo.name

(* [get_advantage_value elst1 elst2] returns the float value representing
 * the type advantage of the element of the Pokemon [elst1] against the element
 * of the opposing Pokemon [elst2].
 *
 *  - [elst1] is a list of up to two element types for Pokemon 1.
 *  - [elst2] is a list of up to two element types for Pokemon 2.
 *)
let get_advantage_value (elst1:element list) (elst2:element list) : float =
  let rec helper = function
    | [] -> 1.
    | h::t -> (List.fold_left (fun acc x -> acc *. Pokemon.getElementEffect h x)
                1. elst2) *. (helper t)
  in
  helper elst1

(* [replaceBetterDead poke alivelst] returns the string name of the first
 * Pokemon in the non-empty list of alive pokemon [alivelst] that has the
 * maximum type advantage against the opposing Pokemon [poke].
 *
 *  - [poke] is a battle Pokemon to be compared against.
 *  - [alivelst] is a list of battle Pokemon.
 *)
let replaceBetterDead (poke:battle_poke) (alivelst:battle_poke list) : string =
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
