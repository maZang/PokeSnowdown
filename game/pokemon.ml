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

let unlocked_pokemon () = open_json "factorysets"

let unlocked_poke_string_list () =
  List.map (to_string) (unlocked_pokemon () |> member "pokemon" |> to_list)

let getNatureFromString str =
  match str with
  | "hardy" -> Hardy
  | "lonely" -> Lonely
  | "adamant" -> Adamant
  | "naughty" -> Naughty
  | "brave" -> Brave
  | "bold" -> Bold
  | "docile" -> Docile
  | "impish" -> Impish
  | "lax" -> Lax
  | "relaxed" -> Relaxed
  | "modest" -> Modest
  | "mild" -> Mild
  | "bashful" -> Bashful
  | "rash" -> Rash
  | "quiet" -> Quiet
  | "calm" -> Calm
  | "gentle" -> Gentle
  | "careful" -> Careful
  | "quirky" -> Quirky
  | "sassy" -> Sassy
  | "timid" -> Timid
  | "hasty" -> Hasty
  | "jolly" -> Jolly
  | "naive" -> Naive
  | "serious" -> Serious
  | _ -> failwith "Does not occur"

let getRandomNature () =
  match Random.int 25 with
  | 0 -> Hardy
  | 1 -> Lonely
  | 2 -> Adamant
  | 3 -> Naughty
  | 4 -> Brave
  | 5 -> Bold
  | 6 -> Docile
  | 7 -> Impish
  | 8 -> Lax
  | 9 -> Relaxed
  | 10 -> Modest
  | 11 -> Mild
  | 12 -> Bashful
  | 13 -> Rash
  | 14 -> Quiet
  | 15 -> Calm
  | 16 -> Gentle
  | 17 -> Careful
  | 18 -> Quirky
  | 19 -> Sassy
  | 20 -> Timid
  | 21 -> Hasty
  | 22 -> Jolly
  | 23 -> Naive
  | 24 -> Serious
  | _ -> failwith "Does not occur"

let string_of_item item =
  match item with
  | Leftovers -> "Leftovers"
  | ChoiceBand -> "ChoiceBand"
  | LifeOrb -> "LifeOrb"
  | ChoiceSpecs -> "ChoiceSpecs"
  | CharizarditeX -> "CharizarditeX"
  | Nothing -> "NO ITEM"

let getItemFromString str =
  match str with
  | "life orb" -> LifeOrb
  | "choice band" -> ChoiceBand
  | "leftovers" -> Leftovers
  | "choice specs" -> ChoiceSpecs
  | _ -> failwith "Does not occur"

let getRandomItem () =
  match Random.int 4 with
  | 0 -> Leftovers
  | 1 -> ChoiceBand
  | 2 -> LifeOrb
  | 3 -> ChoiceSpecs
  | _ -> failwith "Does not occur"

let string_of_element elm =
  match elm with
  | Fire -> "Fire"
  | Water -> "Water"
  | Grass -> "Grass"
  | Rock -> "Rock"
  | Ground -> "Ground"
  | Fairy -> "Fairy"
  | Dark -> "Dark"
  | Electric -> "Electric"
  | Ghost -> "Ghost"
  | Steel -> "Steel"
  | Normal -> "Normal"
  | Bug -> "Bug"
  | Flying -> "Flying"
  | Psychic -> "Psychic"
  | Ice -> "Ice"
  | Dragon -> "Dragon"
  | Fighting -> "Fighting"
  | Poison -> "Poison"

let string_of_status elm =
  match elm with
  | Burn -> "Burn"
  | Freeze -> "Freeze"
  | Paralysis -> "Paralysis"
  | Poisoned -> "Poison"
  | Toxic _ -> "Toxic"
  | Sleep _ -> "Sleep"
  | NoNon -> "None"

let string_of_vola_status elm =
  match elm with
  | Confusion _-> "Confusion"
  | Curse -> "Curse"
  | Embargo -> "Embargo"
  | Encore -> "Encore"
  | Flinch -> "Flinch"
  | HealBlock -> "HealBlock"
  | Identification -> "Identification"
  | Infatuation -> "Infatuation"
  | Nightmare -> "Nightmare"
  | Trapped -> "Trapped"
  | PerishSong -> "PerishSong"
  | Leeched -> "Leeched"
  | Taunt -> "Taunt"
  | Torment -> "Torment"
  | Levitate -> "Levitate"
  | Charge -> "Charging"
  | Substitute _ -> "Substitute"
  | Protected -> "Protect"
  | UsedProtect -> "Used Protect"

let string_of_poke_status (non, vola) =
  List.fold_left (fun acc s -> acc ^ ", " ^ string_of_vola_status s) (string_of_status non) vola

let getStageEvasion num =
  if abs(num) > 6 then failwith "Faulty Game Logic: Debug 43";
  if num <= 0 then
    3. /. float_of_int (-1 * num + 3)
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

let string_of_type elm =
  match elm with
  | [x] -> string_of_element x
  | [x1;x2] -> string_of_element x1 ^ "/" ^ string_of_element x2
  | _ -> failwith "Faulty Game Logic: Debug 100"

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

let getElementEffect elm1 elm2 =
  match elm1 with
  | Normal ->
    (match elm2 with
      | Rock | Steel -> 0.5
      | Ghost -> 0.
      | _ -> 1.
    )
  | Fighting ->
    (match elm2 with
      | Normal | Rock | Steel | Ice | Dark -> 2.
      | Flying | Poison | Bug |Psychic | Fairy -> 0.5
      | Ghost -> 0.
      | _ -> 1.
    )
  | Flying ->
    (match elm2 with
      | Fighting | Bug | Grass -> 2.
      | Rock | Electric -> 0.5
      | _ -> 1.
    )
  |  Poison ->
    (match elm2 with
      | Grass | Fairy -> 2.
      | Poison | Ground | Rock | Ghost -> 0.5
      | _ -> 1.
    )
  |  Ground ->
    (match elm2 with
      | Poison | Rock | Steel | Fire | Electric -> 2.
      | Bug | Grass -> 0.5
      | Flying -> 0.
      | _ -> 1.
    )
  |  Rock ->
    (match elm2 with
      | Flying | Bug | Fire | Ice -> 2.
      | Fighting | Ground | Steel -> 0.5
      | _ -> 1.
    )
  |  Bug ->
    (match elm2 with
      | Grass | Psychic | Dark -> 2.
      | Fire | Fighting | Poison | Flying | Ghost | Steel | Fairy -> 0.5
      | _ -> 1.
    )
  |  Ghost ->
    (match elm2 with
      | Psychic | Ghost -> 2.
      | Dark -> 0.5
      | Normal -> 0.
      | _ -> 1.
    )
  |  Steel ->
    (match elm2 with
      | Ice | Rock | Fairy -> 2.
      | Fire | Water | Electric | Steel -> 0.5
      | _ -> 1.
    )
  |  Fire ->
    (match elm2 with
      | Grass | Ice | Bug | Steel -> 2.
      | Fire | Water | Rock | Dragon -> 0.5
      | _ -> 1.
    )
  |  Water ->
    (match elm2 with
      | Fire | Ground | Rock -> 2.
      | Water | Grass | Dragon -> 0.5
      | _ -> 1.
    )
  |  Grass ->
    (match elm2 with
      | Water | Ground | Rock -> 2.
      | Fire | Grass | Poison | Flying | Bug | Dragon | Steel -> 0.5
      | _ -> 1.
    )
  |  Electric ->
    (match elm2 with
      | Water | Flying -> 2.
      | Electric | Grass | Dragon -> 0.5
      | Ground -> 0.
      | _ -> 1.
    )
  |  Psychic ->
    (match elm2 with
      | Fighting | Poison -> 2.
      | Psychic | Steel -> 0.5
      | Dark -> 0.
      | _ -> 1.
    )
  | Ice->
    (match elm2 with
      | Grass | Ground | Flying | Dragon -> 2.
      | Fire | Water | Ice | Steel -> 0.5
      | _ -> 1.
    )
  | Dragon ->
    (match elm2 with
      | Dragon -> 2.
      | Steel -> 0.5
      | Fairy -> 0.
      | _ -> 1.
    )
  |  Dark ->
    (match elm2 with
      | Psychic | Ghost -> 2.
      | Fighting | Dark | Fairy -> 0.5
      | _ -> 1.
    )
  |  Fairy ->
    (match elm2 with
      | Fighting | Dragon | Dark -> 2.
      | Fire | Poison | Steel -> 0.5
      | _ -> 1.
    )
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

let string_of_class cls =
  match cls with
  | Physical -> "Physical"
  | Special -> "Special"
  | Status -> "Status"

let string_of_weather w =
  match w with
  | HarshSun _-> "There is a very harsh sunlight."
  | Hail _-> "Hail is pouring down."
  | Rain _-> "Rain is pouring down."
  | SandStorm _-> "A sandstorm is kicking up."
  | HeavyRain _-> "Rain is pouring down heavily"
  | Sun _-> "It is currently very sunny."
  | AirCurrent _-> "An air current is present."
  | ClearSkies -> "There are no aberrant weather conditions."

let getDmgClass str =
  match str with
  | "physical" -> Physical
  | "special" -> Special
  | "status" -> Status
  | _ -> failwith "Not a valid damage class"

let getSecondaryEffect str = match str with
  | "karate-chop" | "razor-leaf" | "crabhammer" | "slash" | "aeroblast" |
      "cross-chop" -> [IncCrit 1]
  | "double-slap" | "comet-punch" | "fury-attack" | "pin-missile" |
      "spike-cannon" | "barrage" | "fury-swipes" | "bone-rush" -> [RandMultHit]
  | "fire-punch" | "ember" | "flamethrower" | "fire-blast" |
        "flame-wheel" | "will-o-wisp" -> [BurnChance]
  | "ice-punch" | "ice-beam" | "blizzard" -> [FreezeChance]
  | "thunder-punch" | "body-slam" | "stun-spore" | "thunder-shock"
    | "thunderbolt" | "thunder-wave" | "thunder" | "lick" | "glare"
    | "zap-cannon" | "spark" | "dragon-breath" | "nuzzle"-> [ParaChance]
  | "guillotine" | "horn-drill" | "fissure"-> [OHKO]
  | "razor-wind" -> [ChargeMove "It made a whirlwind!"]
  | "fly" -> [ChargeMove "It flew up into the air."]
  | "skull-bash" -> [ChargeMove "It tucked its head in."; StageBoost [(Defense, 1)]]
  | "dig" -> [ChargeMove "It went slightly underground"]
  | "dive" -> [ChargeMove "It went underwater somehow"]
  | "bounce" -> [ChargeMove "It bounced up high"; ParaChance]
  | "freeze-shock" -> [ChargeMove "Charging"; ParaChance]
  | "swords-dance" -> [StageBoost [(Attack, 2)]]
  | "charm" -> [StageAttack [(Attack, 2)]]
  | "meditate" | "sharpen" | "metal-claw" | "howl" -> [StageBoost [(Attack, 1)]]
  | "whirlwind" | "roar" | "dragon-tail" -> [ForceSwitch]
  | "stomp" | "rolling-kick" | "headbutt" | "bite" | "bone-club" | "waterfall"
    | "rock-slide" | "hyper-fang" | "twister" -> [FlinchMove]
  | "double-kick" | "gear-grind" | "bonemerang" | "double-hit" -> [MultHit 2]
  | "sand-attack" | "smokescreen" | "kinesis" | "flash" |
      "mud-slap" | "octazooka"-> [StageAttack [(Accuracy, 1)]]
  | "take-down" | "double-edge" | "submission" -> [RecoilMove]
  | "tail-whip" | "leer" | "iron-tail" | "crunch" -> [StageAttack [(Defense, 1)]]
  | "poison-sting" | "poison-powder" | "smog" | "sludge" |
      "poison-gas" | "sludge-bomb" -> [PoisonChance]
  | "twineedle" -> [MultHit 2; PoisonChance]
  | "growl" | "aurora-beam" -> [StageAttack [(Attack, 1)]]
  | "sing" | "sleep-powder" | "hypnosis" | "lovely-kiss" | "spore" -> [PutToSleep]
  | "supersonic" | "psybeam" | "confusion" | "confuse-ray" | "dizzy-punch"
    | "sweet-kiss" | "dynamic-punch"-> [ConfuseOpp]
  | "sonic-boom" -> [ConstantDmg 20]
  | "acid" | "psychic" | "shadow-ball" -> [StageAttack [(SpecialDefense, 1)]]
  | "bubble-beam" | "bubble" | "powder-snow" | "icy-wind" -> [StageAttack [(Speed, 1)]]
  | "hyper-beam"| "blast-burn" | "frenzy-plant" | "hydro-cannon"
      | "roar-of-time" | "giga-impact" | "rock-wrecker"  -> [RechargeMove]
  | "low-kick" | "grass-knot" -> [WeightDamage]
  | "seismic-toss" | "night-shade" -> [ConstantDmg 100]
  | "absorb" | "mega-drain" | "leech-life" | "giga-drain" -> [DrainMove]
  | "leech-seed" -> [LeechSeed]
  | "growth" -> [StageBoostSunlight [(Attack, 1); (SpecialAttack, 1)]]
  | "solar-beam" -> [ChargeInSunlight "Need sunlight to charge faster!"]
  | "string-shot" | "cotton-spore" | "scary-face" -> [StageAttack [(Speed, 2)]]
  | "dragon-rage" -> [ConstantDmg 40]
  | "toxic" -> [ToxicChance]
  | "agility" -> [StageBoost [(Speed, 2)]]
  | "screech" -> [StageAttack [(Defense, 2)]]
  | "double-team" -> [StageBoost [(Evasion, 1)]]
  | "minimize" -> [StageBoost [(Evasion, 2)]]
  | "harden" | "withdraw" | "defense-curl" | "steel-wing" -> [StageBoost [(Defense, 1)]]
  | "barrier" | "acid-armor" | "iron-defense" -> [StageBoost [(Defense, 2)]]
  | "calm-mind" -> [StageBoost [(SpecialDefense, 1); (SpecialAttack, 1)]]
  | "leaf-storm" -> [StageBoost [(SpecialAttack, -2)]]
  | "recover" | "soft-boiled" | "milk-drink" | "roost" -> [Recovery]
  | "light-screen" -> [LightScreenMake]
  | "haze" -> [Haze]
  | "reflect" -> [ReflectMake]
  | "self-destruct" | "explosion" -> [UserFaint]
  | "swift" | "feint-attack" | "vital-throw" | "aerial-ace" | "magnet-bomb"
      | "shock-wave" | "magical-leaf" | "shadow-punch" -> [NeverMiss]
  | "amnesia" -> [StageBoost [(SpecialDefense, 2)]]
  | "nasty-plot" -> [StageBoost [(SpecialAttack, 2)]]
  | "dream-eater" -> [DrainMoveSleep]
  | "sky-attack" -> [ChargeMove "The Pokemon is glowing."; IncCrit 1; FlinchMove]
  | "psywave" -> [VariableDamage]
  | "rest" -> [Rest]
  | "tri-attack" -> [ParaChance; BurnChance; FreezeChance]
  | "super-fang" -> [SuperFang]
  | "substitute" -> [SubstituteMake]
  | "triple-kick" -> [MultHit 3]
  | "flail" | "reversal" -> [Flail]
  | "protect" | "detect"-> [Protect]
  | "belly-drum" -> [BellyDrum]
  | "spikes" -> [Spikes]
  | "swagger" -> [StageAttack [(Attack, -2)]; ConfuseOpp]
  | "heal-bell" | "aromatherapy" -> [HealBell]
  | "sweet-scent" -> [StageAttack [(Evasion, 2)]]
  | "morning-sun" | "synthesis" | "moonlight" -> [SunHeal]
  | "water-spout" | "eruption" -> [MaxHealthDmg]
  | "sunny-day" -> [SunnyDay]
  | "refresh" -> [Refresh]
  | "false-swipe" | "hold-back" -> [FalseSwipe]
  | "psych-up" -> [PsychUp]
  | "acupressure" -> [RandStageBoost]
  | "flower-shield" -> [FlowerShield]
  | "rain-dance" -> [RainDance]
  | "sandstorm" -> [SandStormMake]
  | "hail" -> [HailMake]
  | "dragon-dance" -> [StageBoost [(Attack,1);(Speed,1)]]
  | "bulk-up" -> [StageBoost [(Attack,1);(Defense,1)]]
  | _ -> []

(* Returns something of form  {name:string; priority: int; target: target; dmg_class: dmg_class;
    power:int; effect_chance: int; accuracy: int; element: element;
    description: string} *)
let getMoveFromString str =
  let move = move_json |> member str in
  let priority = move |> member "priority" |> to_string |> int_of_string in
  let powerstr = move |> member "power" |> to_string in
  let power = match str with
            | "triple-kick" -> 20
            | "return" | "frustration" -> 102
            | "hyper-beam" | "blast-burn" | "frenzy-plant" | "hydro-cannon"
              | "roar-of-time" | "giga-impact" | "rock-wrecker" -> 120
            | _ ->  (try int_of_string powerstr with |_ -> 0) in
  let dmg_class = move |> member "dmg_class" |> to_string |> getDmgClass in
  let target = move |> member "target" |> to_string |> getTarget in
  let effect_chance_str = move |> member "effect_chance" |> to_string in
  let effect_chance = if str = "tri-attack" then 7 else
                      try int_of_string effect_chance_str with |_ -> 100 in
  let accuracy_str = move |> member "accuracy" |> to_string in
  let accuracy = try int_of_string accuracy_str with  |_ -> 100 in
  let element = move |> member "type" |> to_string |> getElement in
  let description = move |> member "effect" |> to_string in
  let secondary = getSecondaryEffect str in
  {name = str; priority; target; dmg_class; power; effect_chance; accuracy;
  element; description; secondary}

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

let getPresetPokemon str =
  let presetjson = unlocked_pokemon () in
  let poke = poke_json |> member str in
  let presetpoke = presetjson |> member str in
  let ev_helper str =
    presetpoke |> member "evs" |> member str |> to_string |> int_of_string in
  let element_string = poke |> member "type" |> to_list|> filter_string in
  let element = List.map getElement element_string in
  let moveslist = List.map (to_string) (presetpoke |> member "moves" |> to_list) in
  let hp = poke |> member "stats" |> member "hp" |> to_string |> int_of_string in
  let attack = poke |> member "stats" |> member "attack" |> to_string |> int_of_string in
  let defense = poke |> member "stats" |> member "defense" |> to_string
            |> int_of_string in
  let special_defense = poke |> member "stats" |> member "special-defense"
            |> to_string |> int_of_string in
  let special_attack = poke |> member "stats" |> member "special-attack"
            |> to_string |> int_of_string in
  let speed = poke |> member "stats" |> member "speed" |> to_string
            |> int_of_string in
  let ability = presetpoke |> member "ability" |> to_string in
  let evs = {attack = ev_helper "attack"; defense = ev_helper "defense";
            special_attack = ev_helper "special-attack"; special_defense =
            ev_helper "special-defense"; speed = ev_helper "speed";
            hp = ev_helper "hp"} in
  let nature = getNatureFromString (presetpoke |> member "nature" |> to_string) in
  let item = getItemFromString (presetpoke |> member "item" |> to_string) in
  let move1 = getMoveFromString (List.hd moveslist) in
  let move2 = getMoveFromString (List.nth moveslist 1) in
  let move3 = getMoveFromString (List.nth moveslist 2) in
  let move4 = getMoveFromString (List.nth moveslist 3) in
  {name = str; element; move1 ; move2; move3 ; move4 ; hp;
  attack; defense; special_defense; special_attack; speed; ability; evs;
  nature; item}

let getTestPoke () =
  let evs = {attack = 0; defense =  255; special_attack= 0; special_defense= 255;
            hp=255; speed=255} in
  let nature = Bold in
  let item = Leftovers in
  {name="gardevoir-mega"; element=[Grass]; move1= getMoveFromString "double-hit"; move2 =
  getMoveFromString "dragon-dance"; move3 = getMoveFromString "psych-up";
  move4 = getMoveFromString "false-swipe"; hp = 68; attack = 85; special_attack = 165; defense = 65;
  speed = 0; special_defense = 135; ability="pixilate"; evs; nature; item}

let getTestOpp () =
  let evs = {attack = 0; defense =  255; special_attack= 0; special_defense= 255;
            hp=255; speed=255} in
  let nature = Bold in
  let item = Leftovers in
  {name="gallade-mega"; element=[Grass]; move1= getMoveFromString "swords-dance"; move2 =
  getMoveFromString "swords-dance"; move3 = getMoveFromString "swords-dance";
  move4 = getMoveFromString "swords-dance"; hp = 68; attack = 85; special_attack = 165; defense = 65;
  speed = 100; special_defense = 135; ability="pixilate"; evs; nature; item}

let getPokeToolTip t =
  let battlePoke = t.current in
  "Name: " ^ battlePoke.pokeinfo.name ^
  "\nType: " ^ string_of_type battlePoke.pokeinfo.element ^
  "\nAttack: " ^ string_of_int battlePoke.battack ^ "                  Modified: " ^ string_of_float (float_of_int battlePoke.battack *. getStageAD (fst t.stat_enhance.attack) *. (snd t.stat_enhance.attack)) ^
  "\nDefense: " ^ string_of_int battlePoke.bdefense ^ "               Modified: " ^ string_of_float (float_of_int battlePoke.bdefense *. getStageAD (fst t.stat_enhance.defense) *. (snd t.stat_enhance.defense)) ^
  "\nSpecial Attack: " ^ string_of_int battlePoke.bspecial_attack ^ "     Modified: " ^ string_of_float (float_of_int battlePoke.bspecial_attack *. getStageAD (fst t.stat_enhance.special_attack) *. (snd t.stat_enhance.special_attack)) ^
  "\nSpecial Defense: " ^ string_of_int battlePoke.bspecial_defense ^ "  Modified: " ^ string_of_float (float_of_int battlePoke.bspecial_defense *. getStageAD (fst t.stat_enhance.special_defense) *. (snd t.stat_enhance.special_defense)) ^
  "\nSpeed: " ^ string_of_int battlePoke.bspeed ^ "                  Modified: " ^ string_of_float (float_of_int battlePoke.bspeed *. getStageAD (fst t.stat_enhance.speed) *. (snd t.stat_enhance.speed)) ^
  "\nItem: " ^ string_of_item battlePoke.curr_item ^
  "\nStatus: " ^ string_of_poke_status battlePoke.curr_status

let getMoveToolTip move =
  "Class: " ^ string_of_class move.dmg_class ^
  (if move.dmg_class = Physical || move.dmg_class = Special then
    "\nPower: " ^ string_of_int move.power else "") ^
  "\nAccuracy: " ^ string_of_int move.accuracy ^
  "\nType: " ^ string_of_element move.element ^
  "\nDescription: " ^ move.description