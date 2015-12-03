open Pokemon

exception FaultyGameSave
exception BadFieldOption
exception BadEVInput

let updatePokemonFields pokename move1 move2 move3 move4 ability nature item hpevs atkevs defevs spatkevs spdefevs speedevs key=
  let move_lst = getAllMoves pokename in
  let abil_lst = getAllAbilities pokename in
  let nature_list = nature_list in
  let item_list = item_list in
  match (List.mem move1 move_lst), (List.mem move2 move_lst), (List.mem
  move3 move_lst), (List.mem move4 move_lst), (List.mem ability abil_lst),
  (List.mem nature nature_list), (List.mem item item_list) with
  | (true, true, true, true, true, true ,true) ->
      (if (hpevs + atkevs + defevs + spatkevs + spdefevs + speedevs > 510) then
        raise BadEVInput
      else
        match key with
        | "moves" -> ("moves", `List [`String move1; `String move2; `String move3; `String move4])
        | "ability" -> ("ability", `String ability)
        | "nature" -> ("nature", `String nature)
        | "item" -> ("item", `String item)
        | "evs" -> ("evs", `Assoc [("hp", `String (string_of_int hpevs));
                      ("attack", `String (string_of_int atkevs));
                      ("defense", `String (string_of_int defevs));
                      ("special-attack", `String (string_of_int spatkevs));
                      ("special-defense", `String (string_of_int spdefevs));
                      ("speed", `String (string_of_int speedevs))])
        | _ -> raise FaultyGameSave)
  | _ -> raise FaultyGameSave

let createSavePokeEdit pokename move1 move2 move3 move4 ability nature item hpevs atkevs defevs spatkevs spdefevs speedevs =
  let prevSave = unlocked_pokemon () in
  let make_new json =
  match json with
  | `Assoc l -> let l', _ = List.split l in `Assoc (List.map (updatePokemonFields pokename move1 move2 move3 move4 ability nature item hpevs atkevs defevs spatkevs spdefevs speedevs) l')
  | _ -> raise FaultyGameSave in
  let rec create_new_save = function
  | (s, json)::t -> if s = pokename then (s, make_new json)::t else (s, json)::create_new_save t
  | [] -> raise FaultyGameSave in
  let newSave = match prevSave with
  | `Assoc lst -> `Assoc (create_new_save lst)
  | _ -> raise FaultyGameSave in
  Yojson.Basic.to_file "../data/factorysets1.json" newSave