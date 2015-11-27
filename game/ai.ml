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

(* [has_advantage] checks whether the element of the pokemon [elst1] has
 * a type advantage against the element of the opposing pokemon [elst2].
 *)
let has_advantage (elst1:element list) (elst2:element list) : bool =
  let rec helper = function
    | [] -> 1.
    | h::t ->
        (List.fold_left (fun acc x -> acc *. Pokemon.getElementEffect h x)
        1. elst2) *. (helper t)
  in
  if (helper elst1 >= 2.) then true
  else false

(* [replaceDead2] returns the string name of the first pokemon in the list of
 * alive pokemon [alivelst] that has a type advantage against the opposing
 * pokemon [poke]. If there are no such pokemon, one is randomly chosen
 * from the list [alivelst].
 *)
let replaceDead2 (poke:battle_poke) (alivelst:battle_poke list) : string =
  let rec helper = function
    | [] -> let n = Random.int (List.length alivelst) in
            (List.nth alivelst n).pokeinfo.name
    | h::t ->
        if (has_advantage h.pokeinfo.element poke.pokeinfo.element) then
          h.pokeinfo.name
        else helper t
  in
  if (alivelst = []) then failwith "No Pokemon Left. IDK what to do here"
  else helper alivelst