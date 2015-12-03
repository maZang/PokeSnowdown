open Info

type enemy = RoughNeck | Beauty | BugCatcher | CampNerd | DragonTamer | Falkner | FatMan | Psychic | Youngster

let enemy1 = ref Beauty
let enemy2 = ref Falkner

let profOakQuotes = ["Welcome to Pokemon Snowdown.";
                    "You may pick one of the two trainers to face.";
                    "Look at the enemy trainers closely to discern what type of Pokemon they use.";
                    "You will have the option of choosing your Pokemon after you make your selection."]

let opp1Quotes = ["I am Chirag's mom.";
                    "You will regret choosing me."]


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
  | CampNerd -> "campnerd"
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


let opp1Quotes () = ["I am Chirag's mom.";
                    "You will regret choosing me."]


let opp2Quotes = ["Hello there.";
                    "I am."]