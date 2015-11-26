open Info

(* Randomly gets the name of a move *)
let getRandomMove (poke:battle_poke) : string =
  match Random.int 4 with
  | 0 -> poke.pokeinfo.move1.name
  | 1 -> poke.pokeinfo.move2.name
  | 2 -> poke.pokeinfo.move3.name
  | 3 -> poke.pokeinfo.move4.name
  | _ -> failwith "Does not occur"

(* Randomly choose a pokemon given a list of alive pokemon.
 * Btw, doesn't work if if list is empty,
 * what should I do if no alive pokemon Matt? *)
let replaceDead (lst:battle_poke list) : string =
  let n = Random.int (List.length lst) in
    (List.nth lst n).pokeinfo.name