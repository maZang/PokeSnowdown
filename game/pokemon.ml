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

let nature_list =  ["hardy"; "lonely";"adamant";"naughty";"brave";"bold";"docile";
  "impish";"lax";"relaxed";"modest";"mild";"bashful";"rash";"quiet";"calm";"gentle";
  "careful";"quirky";"sassy";"timid";"hasty";"jolly";"naive";"serious"]

let item_list = ["leftovers";"choice band";"life orb";"choice specs"
  ;"choice scarf"; "MegaStone"; "MegaStoneX"; "MegaStoneY"; "nothing"]

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

let string_of_nature nature =
  match nature with
  | Hardy -> "hardy"
  | Lonely -> "lonely"
  | Adamant -> "adamant"
  | Naughty -> "naughty"
  | Brave -> "brave"
  | Bold -> "bold"
  | Docile -> "docile"
  | Impish -> "impish"
  | Lax -> "lax"
  | Relaxed -> "relaxed"
  | Modest -> "modest"
  | Mild -> "mild"
  | Bashful -> "bashful"
  | Rash -> "rash"
  | Quiet -> "quiet"
  | Calm -> "calm"
  | Gentle -> "gentle"
  | Careful -> "careful"
  | Quirky -> "quirky"
  | Sassy -> "sassy"
  | Timid -> "timid"
  | Hasty -> "hasty"
  | Jolly -> "jolly"
  | Naive -> "naive"
  | Serious -> "serious"

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
  | Leftovers -> "leftovers"
  | ChoiceBand -> "choice band"
  | LifeOrb -> "life orb"
  | ChoiceSpecs -> "choice specs"
  | ChoiceScarf -> "choice scarf"
  | MegaStone -> "MegaStone"
  | MegaStoneX -> "MegaStoneX"
  | MegaStoneY -> "MegaStoneY"
  | Nothing -> "nothing"

let getItemFromString str =
  match str with
  | "life orb" -> LifeOrb
  | "choice band" -> ChoiceBand
  | "leftovers" -> Leftovers
  | "choice specs" -> ChoiceSpecs
  | "choice scarf" -> ChoiceScarf
  | "nothing" -> Nothing
  | "MegaStone" -> MegaStone
  | "MegaStoneX" -> MegaStoneX
  | "MegaStoneY" -> MegaStoneY
  | _ -> failwith "Does not occur"

let getRandomItem () =
  match Random.int 3 with
  | 0 -> Leftovers
  | 1 -> Nothing
  | 2 -> LifeOrb
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
  | Flinch -> "Flinch"
  | Leeched -> "Leeched"
  | Charge -> "Charging"
  | Substitute _ -> "Substitute"
  | Protected -> "Protect"
  | UsedProtect -> "Used Protect"
  | RechargingStatus -> "Recharging"
  | ForcedMove (n, s) -> "Forced to use: " ^ s
  | ForcedMoveNoSwitch (n, s) -> "Locked into: " ^  s
  | Taunt n -> "Taunted"
  | PartialTrapping (s,n) -> "Trapped by "^  s ^ " for " ^ string_of_int n  ^ " turns."

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
  | "karate-chop" | "razor-leaf" | "crabhammer" | "slash" | "aeroblast"
    | "cross-chop" | "air-cutter" | "stone-edge" | "attack-order" | "drill-run"
    | "leaf-blade" | "night-slash" | "psycho-cut" | "shadow-claw" | "spacial-rend" -> [IncCrit 1]
  | "double-slap" | "comet-punch" | "fury-attack" | "pin-missile"
    | "spike-cannon" | "barrage" | "fury-swipes" | "bone-rush"
    | "bullet-seed" | "tail-slap" | "icicle-spear" | "rock-blast" -> [RandMultHit]
  | "fire-punch" | "ember" | "flamethrower" | "fire-blast" | "flame-wheel"
    | "will-o-wisp" | "blue-flare" | "lava-plume" | "heat-wave"
    | "scald" | "searing-shot" | "sacred-fire" -> [BurnChance]
  | "ice-punch" | "ice-beam" | "blizzard" | "powder-snow" -> [FreezeChance]
  | "thunder-punch" | "body-slam" | "stun-spore" | "thunder-shock"
    | "thunderbolt" | "thunder-wave" | "thunder" | "lick" | "glare"
    | "zap-cannon" | "spark" | "dragon-breath" | "nuzzle" | "secret-power"
    | "discharge" | "force-palm" -> [ParaChance]
  | "guillotine" | "horn-drill" | "fissure" | "sheer-cold" -> [OHKO]
  | "razor-wind" -> [ChargeMove "It made a whirlwind!"]
  | "fly" -> [ChargeMove "It flew up into the air."]
  | "skull-bash" -> [ChargeMove "It tucked its head in."; StageBoost [(Defense, 1)]]
  | "dig" -> [ChargeMove "It went slightly underground"]
  | "dive" -> [ChargeMove "It went underwater somehow"]
  | "bounce" -> [ChargeMove "It bounced up high"; ParaChance]
  | "freeze-shock" -> [ChargeMove "Charging"; ParaChance]
  | "swords-dance" -> [StageBoost [(Attack, 2)]]
  | "charm" | "feather-dance" -> [StageAttack [(Attack, 2)]]
  | "meditate" | "sharpen" | "metal-claw" | "howl" | "meteor-mash" | "power-up-punch" -> [StageBoost [(Attack, 1)]]
  | "whirlwind" | "roar" | "dragon-tail"| "circle-throw" -> [ForceSwitch]
  | "double-kick" | "gear-grind" | "bonemerang" | "double-hit" | "dual-chop" -> [MultHit 2]
  | "sand-attack" | "smokescreen" | "kinesis" | "flash"
    | "mud-slap" | "octazooka" | "leaf-tornado" | "mud-bomb"
    | "muddy-water" | "mirror-shot"-> [StageAttack [(Accuracy, 1)]]
  | "take-down" | "double-edge" | "submission" | "brave-bird" | "wild-charge"
      | "wood-hammer" | "flare-blitz" | "head-smash" | "head-charge" -> [RecoilMove]
  | "tail-whip" | "leer" | "iron-tail" | "crunch"
    | "rock-smash" | "razor-shell" | "crush-claw" -> [StageAttack [(Defense, 1)]]
  | "poison-sting" | "poison-powder" | "smog" | "sludge" | "poison-gas"
    | "sludge-bomb" | "poison-jab" | "gunk-shot" | "sludge-wave" -> [PoisonChance]
  | "cross-poison" | "poison-tail" -> [PoisonChance; IncCrit 1]
  | "twineedle" -> [MultHit 2; PoisonChance]
  | "growl" | "aurora-beam" | "baby-doll-eyes" | "play-rough" | "play-nice"
    | "struggle-bug" -> [StageAttack [(Attack, 1)]]
  | "sing" | "sleep-powder" | "hypnosis" | "lovely-kiss" | "spore" | "dark-void"
    | "grass-whistle" | "relic-song" -> [PutToSleep]
  | "supersonic" | "psybeam" | "confusion" | "confuse-ray" | "dizzy-punch"
    | "sweet-kiss" | "dynamic-punch" | "water-pulse" | "hurricane" | "chatter"
    | "rock-climb" | "signal-beam" | "teeter-dance" -> [ConfuseOpp]
  | "waterfall" | "extrasensory" | "dark-pulse" | "iron-head"
    | "astonish" | "air-slash" | "bite" | "bone-club" | "dragon-rush"
    | "headbutt" | "heart-stamp" | "hyper-fang" | "icicle-crash"
    | "needle-arm" | "rock-slide" | "rolling-kick" | "steamroller"
    | "stomp" | "zen-headbutt" -> [FlinchMove]
  | "acid" | "psychic" | "shadow-ball" | "flash-cannon" | "bug-buzz"
    | "energy-ball" | "focus-blast" | "earth-power" -> [StageAttack [(SpecialDefense, 1)]]
  | "mist-ball" | "moonblast" | "snarl" -> [StageAttack [(SpecialAttack, 1)]]
  | "bubble-beam" | "bubble" | "icy-wind" | "mud-shot" | "electroweb" | "constrict"
    | "rock-tomb" | "low-sweep" | "bulldoze"-> [StageAttack [(Speed, 1)]]
  | "hyper-beam"| "blast-burn" | "frenzy-plant" | "hydro-cannon"
      | "roar-of-time" | "giga-impact" | "rock-wrecker"  -> [RechargeMove]
  | "swift" | "feint-attack" | "vital-throw" | "aerial-ace" | "magnet-bomb"
    | "shock-wave" | "magical-leaf" | "shadow-punch" -> [NeverMiss]
  | "recover" | "soft-boiled" | "milk-drink" | "roost" | "heal-order"
    | "slack-off" -> [Recovery]
  | "hyperspace-fury" -> [StageBoost [(Defense,-1)]; NeverMiss]
  | "low-kick" | "grass-knot" | "crush-grip"-> [WeightDamage]
  | "sonic-boom" -> [ConstantDmg 20]
  | "dragon-rage" -> [ConstantDmg 40]
  | "seismic-toss" | "night-shade" -> [ConstantDmg 100]
  | "absorb" | "mega-drain" | "leech-life" | "giga-drain" | "drain-punch"
    | "horn-leech" | "draining-kiss" -> [DrainMove]
  | "leech-seed" -> [LeechSeed]
  | "growth" -> [StageBoostSunlight [(Attack, 1); (SpecialAttack, 1)]]
  | "solar-beam" -> [ChargeInSunlight "Need sunlight to charge faster!"]
  | "string-shot" | "cotton-spore" | "scary-face" -> [StageAttack [(Speed, 2)]]
  | "agility" | "rock-polish" -> [StageBoost [(Speed, 2)]]
  | "hammer-arm" -> [StageBoost [(Speed, -1)]]
  | "screech" | "metal-sound" -> [StageAttack [(Defense, 2)]]
  | "double-team" -> [StageBoost [(Evasion, 1)]]
  | "minimize" -> [StageBoost [(Evasion, 2)]]
  | "harden" | "withdraw" | "defense-curl" | "steel-wing" -> [StageBoost [(Defense, 1)]]
  | "barrier" | "acid-armor" | "iron-defense" -> [StageBoost [(Defense, 2)]]
  | "calm-mind" -> [StageBoost [(SpecialDefense, 1); (SpecialAttack, 1)]]
  | "overheat" | "draco-meteor" | "leaf-storm" | "psycho-boost" -> [StageBoost [(SpecialAttack, -2)]]
  | "work-up" -> [StageBoost [(Attack, 1); (SpecialAttack, 1)]]
  | "light-screen" -> [LightScreenMake]
  | "haze" -> [Haze]
  | "reflect" -> [ReflectMake]
  | "self-destruct" | "explosion" -> [UserFaint]
  | "memento" -> [UserFaint; StageAttack [(Attack, 2); (SpecialAttack, 2)]]
  | "amnesia" -> [StageBoost [(SpecialDefense, 2)]]
  | "nasty-plot" -> [StageBoost [(SpecialAttack, 2)]]
  | "charge-beam" -> [StageBoost [(SpecialAttack, 1)]]
  | "dream-eater" -> [DrainMoveSleep]
  | "sky-attack" -> [ChargeMove "The Pokemon is glowing."; IncCrit 1; FlinchMove]
  | "psywave" -> [VariableDamage]
  | "rest" -> [Rest]
  | "tri-attack" -> [ParaChance; BurnChance; FreezeChance]
  | "super-fang" -> [SuperFang]
  | "substitute" -> [SubstituteMake]
  | "triple-kick" -> [MultHit 3]
  | "flail" | "reversal" -> [Flail]
  | "protect" | "detect"| "quick-guard" | "wide-guard" | "crafty-shield"-> [Protect]
  | "belly-drum" -> [BellyDrum]
  | "spikes" -> [Spikes]
  | "swagger" -> [StageAttack [(Attack, -2)]; ConfuseOpp]
  | "flatter" -> [StageAttack [(SpecialAttack, -1)]; ConfuseOpp]
  | "heal-bell" | "aromatherapy" -> [HealBell]
  | "sweet-scent" -> [StageAttack [(Evasion, 2)]]
  | "morning-sun" | "synthesis" | "moonlight" -> [SunHeal]
  | "water-spout" | "eruption" -> [MaxHealthDmg]
  | "sunny-day" -> [SunnyDay]
  | "refresh" -> [Refresh]
  | "flame-charge" -> [StageBoost [(Speed, 1)]]
  | "false-swipe" | "hold-back" -> [FalseSwipe]
  | "psych-up" -> [PsychUp]
  | "acupressure" -> [RandStageBoost]
  | "flower-shield" -> [FlowerShield]
  | "rain-dance" -> [RainDance]
  | "sandstorm" -> [SandStormMake]
  | "hail" -> [HailMake]
  | "dragon-dance" -> [StageBoost [(Attack,1);(Speed,1)]]
  | "bulk-up" -> [StageBoost [(Attack,1);(Defense,1)]]
  | "blaze-kick" -> [IncCrit 1; BurnChance]
  | "encore" -> [Encore 3]
  | "pain-split" -> [PainSplit]
  | "noble-roar" -> [StageAttack [(Attack, 1);(SpecialAttack, 1)]]
  | "superpower" -> [StageBoost [(Attack,-1);(Defense,-1)]]
  | "thrash" | "outrage" | "petal-dance" -> [SelfEncore]
  | "mirror-move" | "copycat" -> [CopyPrevMove]
  | "fire-fang" -> [BurnChance; FlinchMove]
  | "thunder-fang" -> [ParaChance; FlinchMove]
  | "ice-fang" -> [FreezeChance; FlinchMove]
  | "poison-fang" | "toxic" -> [ToxicChance]
  | "tail-glow" -> [StageBoost [(SpecialAttack, 3)]]
  | "defend-order" -> [StageBoost [(SpecialDefense, 1); (Defense, 1)]]
  | "cotton-guard" -> [StageBoost [(Defense, 3)]]
  | "tickle" -> [StageAttack [(Attack,1);(Defense,1)]]
  | "captivate" | "eerie-impulse" -> [StageAttack [(SpecialAttack, 2)]]
  | "fake-tears" | "acid-spray" | "seed-flare" -> [StageAttack [(SpecialDefense, 2)]]
  | "knock-off" -> [KnockOff]
  | "frost-breath" | "storm-throw" -> [IncCrit 3]
  | "hone-claws" -> [StageBoost [(Attack,1);(Accuracy,1)]]
  | "ominous-wind" | "silver-wind" | "ancient-power" -> [ChanceStageBoost]
  | "volt-switch" | "u-turn" -> [SelfSwitch]
  | "foul-play" -> [FoulPlay]
  | "quiver-dance" -> [StageBoost [(SpecialAttack, 1); (SpecialDefense,1); (Speed, 1)]]
  | "shell-smash" -> [StageBoost [(SpecialAttack, 2); (Attack,2); (Speed, 2); (Defense, -1); (SpecialDefense, -1)]]
  | "volt-tackle" -> [RecoilMove; ParaChance]
  | "coil" -> [StageBoost [(Attack, 1); (Defense, 1); (Accuracy, 1)]]
  | "taunt" -> [TauntMove]
  | "ice-burn" -> [ChargeMove "The Pokemon is accumlating ice"; BurnChance]
  | "stealth-rock" -> [StealthRockMake]
  | "toxic-spikes" -> [TSpikes]
  | "sticky-web" -> [StickyWebMake]
  | "rototiller" -> [Rototiller]
  | "sleep-talk" | "snore" -> [SleepEffect; FlinchMove]
  | "beat-up" -> [BeatUp]
  | "fire-spin" | "whirlpool" | "clamp" | "sand-tomb" | "wrap" | "bind"
    | "infestation" | "magma-storm" -> [CausePartialTrapping]
  | "hex" | "brine" | "venoshock"-> [DoublePower]
  | "stored-power" -> [StoredPower]
  | "shift-gear" -> [StageBoost [(Speed, 2); (Attack, 1)]]
  | "cosmic-power" -> [StageBoost [(Defense, 1); (SpecialDefense, 1)]]
  | "venom-drench" -> [VenomDrench]
  | "v-create" -> [StageBoost [(Defense,-1); (SpecialDefense,-1); (Speed,-1)]]
  | "metronome" -> [RandMove]
  | "electro-ball" -> [ElectroBall]
  | "rapid-spin" -> [RapidSpin]
  | "switcheroo" | "trick" -> [ItemSwitch]
  | "close-combat" -> [StageBoost [(Defense, -1); (SpecialDefense, -1)]]
  | "wish" -> [WishMake]
  | "magnitude" -> [ChancePower]
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
            | "heavy-slam" | "heat-crash" -> 120
            | "trump-card" -> 80
            | "natural-gift" -> 100
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

let getAllMoves poke =
  List.fast_sort compare (List.map (to_string) (poke_json |> member poke |> member "moves" |> to_list))

let getAllAbilities poke =
  List.map (to_string) (poke_json |> member poke |> member "ability" |> to_list)

let convertToMega pokeinfo str =
  let newpokename = pokeinfo.name ^ str in
  let newpoke = poke_json |> member newpokename in
  let hp = newpoke |> member "stats" |> member "hp" |> to_string
            |> int_of_string in
  let attack = newpoke |> member "stats" |> member "attack" |> to_string
            |> int_of_string in
  let defense = newpoke |> member "stats" |> member "defense" |> to_string
            |> int_of_string in
  let special_defense = newpoke |> member "stats" |> member "special-defense"
            |> to_string |> int_of_string in
  let special_attack = newpoke |> member "stats" |> member "special-attack"
            |> to_string |> int_of_string in
  let speed = newpoke |> member "stats" |> member "speed" |> to_string
            |> int_of_string in
  let ability = getRandomElement (newpoke |> member "ability" |> to_list)
            |> to_string in
  {name = newpokename; element = pokeinfo.element; move1 = pokeinfo.move1;
  move2 = pokeinfo.move2; move3 = pokeinfo.move3; move4 = pokeinfo.move4;
  hp; attack; defense; special_defense; special_attack; speed; evs = pokeinfo.evs;
  nature = pokeinfo.nature; item = pokeinfo.item; ability}

let findMegaY pokename =
  (poke_json |> member (pokename ^ "-mega-y") <> `Null)

let findMegaX pokename =
  (poke_json |> member (pokename ^ "-mega-x") <> `Null)

let findMega pokename =
  (poke_json |> member (pokename ^ "-mega") <> `Null)

let generatePokemon str =
  let randomPoke = poke_json |> member str in
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
  {name = str; element; move1 ; move2; move3 ; move4 ; hp;
  attack; defense; special_defense; special_attack; speed; ability; evs;
  nature; item}

let getRandomPokemon () =
  let randomPokeName = poke_arr |>
    member (string_of_int (Random.int num_pokemon_total + 1)) |> to_string in
  generatePokemon randomPokeName

let getPresetPokemon ?pjson:(pjson=unlocked_pokemon ()) str =
  Printf.printf "%s\n%!" str;
  let poke = poke_json |> member str in
  let presetpoke = pjson |> member str in
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

let getRandomPreset ?pjson:(pjson=unlocked_pokemon ()) () =
 let full_list = List.map (to_string) (pjson |> member "pokemon" |> to_list) in
 let rand = Random.int (List.length full_list) in
 getPresetPokemon ~pjson:pjson (List.nth full_list rand)

let getTestPoke () =
  let evs = {attack = 0; defense =  255; special_attack= 255; special_defense= 255;
            hp=255; speed=255} in
  let nature = Bold in
  let item = Leftovers in
  {name="gardevoir-mega"; element=[Grass]; move1= getMoveFromString "flail"; move2 =
  getMoveFromString "magnitude"; move3 = getMoveFromString "hyperspace-fury";
  move4 = getMoveFromString "poison-powder"; hp = 98; attack = 100; special_attack = 165; defense = 65;
  speed = 120; special_defense = 135; ability="drizzle"; evs; nature; item}

let getTestOpp () =
  let evs = {attack = 255; defense =  0; special_attack= 0; special_defense= 255;
            hp=255; speed=255} in
  let nature = Bold in
  let item = Leftovers in
  {name="gallade-mega"; element=[Grass]; move1= getMoveFromString "roost"; move2 =
  getMoveFromString "sand-tomb"; move3 = getMoveFromString "rapid-spin";
  move4 = getMoveFromString "toxic"; hp = 68; attack = 255; special_attack = 165; defense = 65;
  speed = 100; special_defense = 135; ability="gale-wings"; evs; nature; item}

let getPokeToolTip t =
  let battlePoke = t.current in
  "Name: " ^ battlePoke.pokeinfo.name ^
  "\nType: " ^ string_of_type battlePoke.pokeinfo.element ^
  "\nAbility: " ^ battlePoke.pokeinfo.ability ^
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