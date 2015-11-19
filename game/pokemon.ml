open Info
open Yojson.Basic.Util
(*factory sets http://www.objgen.com/json/models/VT0 *)
(*This module is responsible for getting random valid pokemon *)

(* number of unique pokemon and mega evolutions *)
let open_json s = try Yojson.Basic.from_file ("../data/" ^ s ^ ".json") with
                  _ -> Printf.printf "Json file for %s not found\n%!" s; exit 0

let nth i lst = List.nth lst i

let () = Random.self_init ()

(* All data *)
let poke_json = open_json "pokemon"
let poke_arr = open_json "pokemonlist"
let move_json = open_json "moves"
let num_pokemon_total = 721

let getRandomNature () =
  match Random.int 4 with
  | 0 -> Adamant
  | 1 -> Modest
  | 2 -> Careful
  | 3 -> Timid
  | _ -> failwith "Does not occur"

let getRandomItem () =
  match Random.int 4 with
  | 0 -> Leftovers
  | 1 -> ChoiceBand
  | 2 -> LifeOrb
  | 3 -> ChoiceSpecs
  | _ -> failwith "Does not occur"

let getElement str =
  match str with
  | "fire" -> Fire
  | "water" -> Water
  | "grass" -> Grass
  | "rock" -> Rock
  | "ground" -> Ground
  | "fairy" -> Fairy
  | "dark" -> Dark
  | "electric" -> Electric
  | "ghost" -> Ghost
  | "steel" -> Steel
  | "normal" -> Normal
  | "bug" -> Bug
  | "flying" -> Flying
  | "psychic" -> Psychic
  | "ice" -> Ice
  | "dragon" -> Dragon
  | "fighting" -> Fighting
  | "poison" -> Poison
  | _ -> failwith "Not a valid type"

(* Gets a random element in a list *)
let getRandomElement lst =
  let l = List.length lst in
  lst |> nth (Random.int l)

let getTarget str =
  match str with
  | "specific-move" -> SpecificPoke
  | "selected-pokemon-me-first" -> SelectedPokeMeFirst
  | "ally" -> Ally
  | "users-field" -> UsersField
  | "user-or-ally" -> UserOrAlly
  | "opponents-field" -> OpponentsFields
  | "user" -> User
  | "random-opponent" -> RandomOpp
  | "all-other-pokemon" -> AllOthers
  | "selected-pokemon" -> SelectedPoke
  | "all-opponents" -> AllOpp
  | "entire-field" -> EntireField
  | "user-and-allies" -> UserAndAlly
  | "all-pokemon" -> All
  | _ -> failwith "Not a valid target"

let getDmgClass str =
  match str with
  | "physical" -> Physical
  | "special" -> Special
  | "status" -> Status
  | _ -> failwith "Not a valid damage class"

(* Returns something of form  {name:string; priority: int; target: target; dmg_class: dmg_class;
    power:int; effect_chance: int; accuracy: int; element: element;
    description: string} *)
let getMoveFromString str =
  let move = move_json |> member str in
  let priority = move |> member "priority" |> to_string |> int_of_string in
  let powerstr = move |> member "power" |> to_string in
  let power = try int_of_string powerstr with |_ -> 0 in
  let dmg_class = move |> member "dmg_class" |> to_string |> getDmgClass in
  let target = move |> member "target" |> to_string |> getTarget in
  let effect_chance_str = move |> member "effect_chance" |> to_string in
  let effect_chance = try int_of_string effect_chance_str with |_ -> 100 in
  let accuracy_str = move |> member "accuracy" |> to_string in
  let accuracy = try int_of_string accuracy_str with  |_ -> 100 in
  let element = move |> member "type" |> to_string |> getElement in
  let description = move |> member "effect" |> to_string in
  {name = str; priority; target; dmg_class; power; effect_chance; accuracy;
  element; description}

let getRandomPokemon () =
  let randomPokeName = poke_arr |>
    member (string_of_int (Random.int num_pokemon_total + 1)) |> to_string in
  let randomPoke = poke_json |> member randomPokeName in
  let element_string = randomPoke |> member "type" |> to_list|> filter_string in
  let element = List.map getElement element_string in
  let moves = randomPoke |> member "moves" |> to_list in
  let move1s = ref (getRandomElement moves |> to_string) in
  let move2s = ref (getRandomElement moves |> to_string) in
  let move3s = ref (getRandomElement moves |> to_string) in
  let move4s = ref (getRandomElement moves |> to_string) in
  let hp = randomPoke |> member "stats" |> member "hp" |> to_string
            |> int_of_string in
  let attack = randomPoke |> member "stats" |> member "attack" |> to_string
            |> int_of_string in
  let defense = randomPoke |> member "stats" |> member "defense" |> to_string
            |> int_of_string in
  let special_defense = randomPoke |> member "stats" |> member "special-defense"
            |> to_string |> int_of_string in
  let special_attack = randomPoke |> member "stats" |> member "special-attack"
            |> to_string |> int_of_string in
  let speed = randomPoke |> member "stats" |> member "speed" |> to_string
            |> int_of_string in
  let ability = getRandomElement (randomPoke |> member "ability" |> to_list)
            |> to_string in
  let evs = {attack = 84; defense =  84; special_attack= 84; special_defense= 84;
            hp=84; speed=84} in
  let nature = getRandomNature () in
  let item = getRandomItem () in
  while (move2s = move1s) do move2s := getRandomElement moves |> to_string done;
  while (move3s = move2s || move3s = move1s) do move3s :=
      getRandomElement moves |> to_string done;
  while (move4s = move3s || move4s = move2s || move4s = move1s) do
      move4s := getRandomElement moves |> to_string done;
  let move1 = !move1s |> getMoveFromString in
  let move2 = !move2s |> getMoveFromString in
  let move3 = !move3s |> getMoveFromString in
  let move4 = !move4s |> getMoveFromString in
  {name = randomPokeName; element; move1 ; move2; move3 ; move4 ; hp;
  attack; defense; special_defense; special_attack; speed; ability; evs;
  nature; item}


 (*  while !move2 = move1 do move2 := getRandomPokemon done *)