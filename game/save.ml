open Pokemon
open Info

(* Save handles all the save data and the user/opponent files.
*)

exception FaultyGameSave
exception BadFieldOption
exception BadEVInput
exception OwnPokemonAlready

(* Updates the pokemon fields for the save file and Pokemon editor
*)
let updatePokemonFields pokename move1 move2 move3 move4 ability nature
  item hpevs atkevs defevs spatkevs spdefevs speedevs key=
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
        | "moves" -> ("moves", `List [`String move1; `String move2;
                      `String move3; `String move4])
        | "ability" -> ("ability", `String ability)
        | "nature" -> ("nature", `String nature)
        | "item" -> ("item", `String item)
        | "name" -> ("name", `String pokename)
        | "evs" -> ("evs", `Assoc [("hp", `String (string_of_int hpevs));
                      ("attack", `String (string_of_int atkevs));
                      ("defense", `String (string_of_int defevs));
                      ("special-attack", `String (string_of_int spatkevs));
                      ("special-defense", `String (string_of_int spdefevs));
                      ("speed", `String (string_of_int speedevs))])
        | _ -> raise FaultyGameSave)
  | _ -> raise BadFieldOption

(* Creates the save file for pokedit
*)
let createSavePokeEdit pokename move1 move2 move3 move4 ability nature item
  hpevs atkevs defevs spatkevs spdefevs speedevs =
  let prevSave = unlocked_pokemon () in
  let make_new json =
  match json with
  | `Assoc l -> let l', _ = List.split l in `Assoc (List.map
      (updatePokemonFields pokename move1 move2 move3 move4 ability nature
      item hpevs atkevs defevs spatkevs spdefevs speedevs) l')
  | _ -> raise FaultyGameSave in
  let rec create_new_save = function
  | (s, json)::t -> if s = pokename then (s, make_new json)::t
                    else (s, json)::create_new_save t
  | [] -> raise FaultyGameSave in
  let newSave = match prevSave with
  | `Assoc lst -> `Assoc (create_new_save lst)
  | _ -> raise FaultyGameSave in
  Yojson.Basic.to_file "../data/factorysets.json" newSave

(* Converts a pokemon (from record to a json) to a json format
*)
let convertPokeToJson poke =
  `Assoc [("name", `String poke.name);
          ("moves", `List [`String poke.move1.name; `String poke.move2.name;
          `String poke.move3.name; `String poke.move4.name]);
          ("ability", `String poke.ability);
          ("nature", `String (string_of_nature poke.nature));
          ("item", `String (string_of_item poke.item));
          ("evs", (`Assoc [("hp", `String (string_of_int poke.evs.hp));
                          ("attack", `String (string_of_int poke.evs.attack));
                          ("defense", `String (string_of_int poke.evs.defense));
                          ("special-attack", `String (string_of_int
                                poke.evs.special_attack));
                          ("special-defense", `String (string_of_int
                                poke.evs.special_defense));
                          ("speed", `String (string_of_int
                                poke.evs.speed))]))]

(* Add a pokemon to your unlocked list
*)
let rec addToUnlocked pokename lst =
  match lst with
  | (s, `List x)::t -> if s = "pokemon" then (s, `List
      ((`String pokename)::x))::t else (s, `List x)::(addToUnlocked pokename t)
  | (s, x)::t -> ((s, x)::(addToUnlocked pokename t))
  | _ -> raise FaultyGameSave

(* Used for incrementing your saves (such as number of times beat Professor Oak,
etc...)
*)
let rec incPrevSave key lst =
  match lst with
  | (s, `Int n)::t -> if key = s then (s, `Int (n+1))::t
                          else (s, `Int n)::(incPrevSave key t)
  | (s, x)::t -> ((s,x)::(incPrevSave key t))
  | _ -> raise FaultyGameSave

(* Increments the number of times you play Oak *)
let rec incPlayOak () =
  let prevSave = unlocked_pokemon () in
  let newSave = match prevSave with
  |`Assoc lst -> `Assoc (incPrevSave "play oak" lst)
  | _ -> raise FaultyGameSave in
  Yojson.Basic.to_file "../data/factorysets.json" newSave

(*
*)
let addPoke beat str =
  if List.mem str (unlocked_poke_string_list ()) then
    raise OwnPokemonAlready
  else
    (let poke = generatePokemon str in
    let json_of_poke = convertPokeToJson poke in
    let prevSave = unlocked_pokemon () in
    let newSave = match prevSave with
    |`Assoc lst -> let lst' = addToUnlocked str (incPrevSave beat
                  (incPrevSave "unlocked" lst)) in
                  `Assoc ((str, json_of_poke)::lst')
    | _ -> raise FaultyGameSave in
    Yojson.Basic.to_file "../data/factorysets.json" newSave)

let getFileMessage () =
  let open Yojson.Basic.Util in
  let save = unlocked_pokemon () in
  match save with
  | `Assoc l -> "Unlocked Pokemon: " ^ (List.assoc "unlocked" l |> to_int
                                            |> string_of_int) ^ "\n" ^
                "You beat baldman " ^ (List.assoc "baldman" l |> to_int
                                          |> string_of_int) ^ " times\n" ^
                "You beat beauty " ^ (List.assoc "beauty" l |> to_int
                                          |> string_of_int) ^ " times\n" ^
                "You beat bugcatcher " ^ (List.assoc "bugcatcher" l
                                |> to_int |> string_of_int) ^ " times\n" ^
                "You beat campernerd " ^ (List.assoc "campernerd" l
                                |> to_int |> string_of_int) ^ " times\n" ^
                "You beat dragontamer " ^ (List.assoc "dragontamer" l
                                |> to_int |> string_of_int) ^ " times\n" ^
                "You beat falkner " ^ (List.assoc "falkner" l |> to_int
                                |> string_of_int) ^ " times\n" ^
                "You beat fatman " ^ (List.assoc "fatman" l |> to_int
                                |> string_of_int) ^ " times\n" ^
                "You beat psychic " ^ (List.assoc "psychic" l |> to_int
                                |> string_of_int) ^ " times\n" ^
                "You beat youngster " ^ (List.assoc "youngster" l |> to_int
                                |> string_of_int) ^ " times\n" ^
                (let times_beat_oak = List.assoc "professor oak" l
                                    |> to_int in if times_beat_oak > 0 then
                "You beat Professor Oak " ^ string_of_int times_beat_oak
                  ^ " times\n" else "")
  | _ -> raise FaultyGameSave

let beat_game () =
  let open Yojson.Basic.Util in
  match unlocked_pokemon () with
  | `Assoc l -> let beat_list = [(List.assoc "baldman" l |> to_int);
                  (List.assoc "beauty" l |> to_int);
                  (List.assoc "bugcatcher" l |> to_int);
                  (List.assoc "campernerd" l |> to_int);
                  (List.assoc "dragontamer" l |> to_int);
                  (List.assoc "falkner" l |> to_int);
                  (List.assoc "fatman" l |> to_int);
                  (List.assoc "psychic" l |> to_int);
                  (List.assoc "youngster" l |> to_int)] in
                let times_play_oak = List.assoc "play oak" l |> to_int in
                List.fold_left (fun acc x -> (x > times_play_oak) && acc)
                true beat_list
  | _ -> raise FaultyGameSave