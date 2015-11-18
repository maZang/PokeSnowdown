open Info
open Yojson.Basic.Util
(*factory sets http://www.objgen.com/json/models/VT0 *)
(*This module is responsible for getting random valid pokemon *)

(* number of unique pokemon and mega evolutions *)
let open_json s = try Yojson.Basic.from_file ("../data/" ^ s ^ ".json") with
                  _ -> Printf.printf "Json file for %s not found\n%!" s; exit 0

let nth i lst = List.nth lst i

(* All data *)
let poke_json = open_json "pokemon"
let num_pokemon_total = try poke_json |> member "total" |> to_int with
                      _ -> Printf.printf "Corrupted Json File \n%!"; exit 0
let all_pokemon = [poke_json] |> filter_member "pokemon" |> flatten

(* Preset data *)
let preset_json = open_json "factorysets"
let num_unlocked = try preset_json |> member "unlocked" |> to_int with
                  _ -> Printf.printf "Corrupted Save File\n%!"; exit 0
let preset_pokemon = [preset_json] |> filter_member "pokemon" |> flatten

(* The array with all your unlocked pokemon *)
let preset_array = let arr = Array.make num_unlocked "" in
  for i = 0 to (num_unlocked - 1) do
    arr.(i) <- preset_pokemon |> nth i |> member "name" |> to_string
  done; arr
(* The array with all the pokemon in the game *)
let poke_array = let arr = Array.make num_pokemon_total "" in
  for i = 0 to (num_pokemon_total - 1) do
    arr.(i) <- all_pokemon |> nth i |> member "name" |> to_string
  done; arr

let getRandomPokemon () = failwith "TODO"