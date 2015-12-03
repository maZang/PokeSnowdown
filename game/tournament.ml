open Info

type enemy = RoughNeck | Beauty | BugCatcher | CampNerd | DragonTamer | Falkner | FatMan | Psychic | Youngster

let enemy1 = ref Beauty
let enemy2 = ref Falkner
let selectedEnemy = ref Falkner

let profOakQuotes = ["Welcome to Pokemon Snowdown.";
                    "You may pick one of the two trainers to face.";
                    "Look at the enemy trainers closely to discern what type of Pokemon they use.";
                    "You will have the option of choosing your Pokemon after you make your selection."]

let roughNeckQuotes =
  ["I am Brutal Bill.";
  "I am a dark and twisted man.";
  "I've been reported in League of Legends for toxicity.";
  "But most of all, I am your father.";
  "Sike."]

let beautyQuotes =
  ["I am Chirag's Mother.";
  "I will rekt you harder than Chirag rekts Piazza.";
  "My heart is ice cold.";
  "And I am fairy mean.";
  "Prepare yourself mate."]

let bugCatcherQuotes =
  ["I'm gonna bug you so hard.";
  "But please don't flame me if I lose.";
  "I'll probably lose if you flame me though."]

let campNerdQuotes =
  ["What is this game?";
  "What is life?";
  "Am I shocking?";
  "Am I hot?";
  "What am I?";
  "What is life?"]

let dragonTamerQuotes =
  ["I am a Targaryen.";
  "Prepare to be bathed in my Houses' Blood."]

let falknerQuotes =
  ["Please don't interrupt me.";
    "In bird culture that is considered a duck move."]

let fatManQuotes =
  ["I'm not fat, I'm big boned.";
  "I will fight you to the death";
  "With some kung fu fighting";
  "Kicks fast as lightning";
  "A little bit frightenning"]

let psychicQuotes =
  ["I knew you would choose me";
  "But what can I say, I'm psychic";
  "Or crazy. Your pick."]

let youngsterQuotes =
  ["You do know I'm underage right.";
  "You can't pick on someone your own size?";
  "But I guess I'll stand my ground."]

let getRandomEnemy () =
  match Random.int 9 with
  | 0 -> RoughNeck
  | 1 -> Beauty
  | 2 -> BugCatcher
  | 3 -> CampNerd
  | 4 -> DragonTamer
  | 5 -> Falkner
  | 6 -> FatMan
  | 7 -> Psychic
  | 8 -> Youngster
  | _ -> failwith "Does Not Happen"

let getStringFromEnemy enm =
  match enm with
  | RoughNeck -> "baldman"
  | Beauty -> "beauty"
  | BugCatcher -> "bugcatcher"
  | CampNerd -> "campernerd"
  | DragonTamer -> "dragontamer"
  | Falkner -> "falker"
  | FatMan -> "fatman"
  | Psychic -> "psychic"
  | Youngster -> "youngster"

let getRandomOpp1 () =
  let rand_enemy = getRandomEnemy () in
  let sprite = getStringFromEnemy rand_enemy in
  enemy1 := rand_enemy; sprite

let getRandomOpp2 () =
  let rand_enemy = getRandomEnemy () in
  let sprite = getStringFromEnemy rand_enemy in
  enemy2:= rand_enemy; sprite

let profOakQuotes = ["Welcome to Pokemon Snowdown.";
                    "You may pick one of the two trainers to face.";
                    "Look at the enemy trainers closely to discern what type of Pokemon they use.";
                    "You will have the option of choosing your Pokemon after you make your selection."]


let getQuotes enm =
  match enm with
  | RoughNeck -> roughNeckQuotes
  | Beauty -> beautyQuotes
  | BugCatcher -> bugCatcherQuotes
  | CampNerd -> campNerdQuotes
  | DragonTamer -> dragonTamerQuotes
  | Falkner -> falknerQuotes
  | FatMan -> fatManQuotes
  | Psychic -> psychicQuotes
  | Youngster -> youngsterQuotes

let opp1Quotes () = selectedEnemy := !enemy1; getQuotes !enemy1

let opp2Quotes () = selectedEnemy := !enemy2; getQuotes !enemy2

let getJson () = Yojson.Basic.from_file ("../data/tournament/NPCjson/baldman.json")

let unlockPokemon () =
  let open Yojson.Basic.Util in
  let unlock_list = List.map (to_string) (getJson () |> member "unlockable" |> to_list) in
  let rand = Random.int (List.length unlock_list) in
  Save.addPoke (List.nth unlock_list rand)