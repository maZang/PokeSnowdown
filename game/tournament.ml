open Info

type enemy = RoughNeck | Beauty | BugCatcher | CampNerd | DragonTamer | Falkner | FatMan | Psychic | Youngster | ProfOak | Chancellor

let enemy1 = ref Beauty
let enemy2 = ref Falkner
let selectedEnemy = ref Falkner

let profOakBattleQuotes = ["I see. You have beat everyone I've thrown at you.";
                          "Team Rocket cannot have someone as strong as you";
                          "interrupting our Hunger Games"; "...err Pokemon Snowdown";
                          "Time to show you why they call me Professor."]

let profOakQuotes = ["Welcome to Pokemon Snowdown. Due to the stark winter";
                    "we have held a recent Pokemon competition to maintain the population.";
                    "You have the option to choose any of the two trainers to battle.";
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

let chancellorQuotes =
  ["Save the Concord? There is no situation with the Concord.";
    "It would be unwise to pursue this line of questioning.";
    "Peaceful Protest? There is no such thing. Bring out the poice and arrest these agitators."]

let getRandomEnemy () =
  match Random.int 10 with
  (*match 9 with*)
  | 0 -> RoughNeck
  | 1 -> Beauty
  | 2 -> BugCatcher
  | 3 -> CampNerd
  | 4 -> DragonTamer
  | 5 -> Falkner
  | 6 -> FatMan
  | 7 -> Psychic
  | 8 -> Youngster
  | 9 -> Chancellor
  | _ -> failwith "Does Not Happen"

let getStringFromEnemy enm =
  match enm with
  | RoughNeck -> "baldman"
  | Beauty -> "beauty"
  | BugCatcher -> "bugcatcher"
  | CampNerd -> "campernerd"
  | DragonTamer -> "dragontamer"
  | Falkner -> "falkner"
  | FatMan -> "fatman"
  | Psychic -> "psychic"
  | Youngster -> "youngster"
  | ProfOak -> "professoroak"
  | Chancellor -> "honorable"

let getStringOfEnemy () =
  getStringFromEnemy !selectedEnemy

let getRandomOpp1 () =
  let rand_enemy = getRandomEnemy () in
  let sprite = getStringFromEnemy rand_enemy in
  enemy1 := rand_enemy; sprite

let getRandomOpp2 () =
  let rand_enemy = getRandomEnemy () in
  let sprite = getStringFromEnemy rand_enemy in
  enemy2:= rand_enemy; sprite

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
  | ProfOak -> profOakBattleQuotes
  | Chancellor -> chancellorQuotes

let getProfOakQuotes () = selectedEnemy := ProfOak; profOakBattleQuotes

let opp1Quotes () = selectedEnemy := !enemy1; getQuotes !enemy1

let opp2Quotes () = selectedEnemy := !enemy2; getQuotes !enemy2

let getJson () =
   Yojson.Basic.from_file ("../data/tournament/NPCjson/" ^ (getStringFromEnemy !selectedEnemy) ^ ".json")

let unlockPokemon () =
  let open Yojson.Basic.Util in
  let unlock_list = List.map (to_string) (getJson () |> member "unlockable" |> to_list) in
  let rand = Random.int (List.length unlock_list) in
  let poke_to_unlock = List.nth unlock_list rand in
  Save.addPoke (getStringFromEnemy !selectedEnemy) poke_to_unlock; poke_to_unlock

type region = IceCave | TourneyNoOak | TourneyOak

let ice_obstacles = [(0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7);
                       (14,0);(14,1);(14,2);(14,3);(14,4);(14,5);(14,6);(14,7);
                       (0,0);(1,0);(2,0);(3,0);(4,0);(5,0);(6,-1);(7,0);(8,-1);
                       (9,0);(10,0);(11,0);(12,0);(13,0);(14,0);(0,7);(1,7);
                       (2,7);(3,7);(4,7);(5,7);(6,7);(8,7);(9,7);(10,7);
                       (11,7);(12,7);(13,7);(14,7); (7,8);
                       (* ice rock *)
                       (6,4); (8,4); (6,3); (8, 3); (6, 2);(8, 2); (5,1); (9,1);
                       (* random rock *)
                       (1,1);(2,1);(13,1);(12,1)]

let tilemap_profoak = [(0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7);
                       (14,0);(14,1);(14,2);(14,3);(14,4);(14,5);(14,6);(14,7);
                       (0,0);(1,0);(2,0);(3,0);(4,0);(5,0);(6,0);(7,0);(8,0);
                       (9,0);(10,0);(11,0);(12,0);(13,0);(14,0);(0,7);(1,7);
                       (2,7);(3,7);(4,7);(5,7);(6,7);(7,7);(8,7);(9,7);(10,7);
                       (11,7);(12,7);(13,7);(14,7);
                       (* cave and Oak *)
                       (7, 2); (8,1); (6,1);
                       (* tree stump and rock *)
                       (1,1);(13,1);(12,1)]

let tilemap_nooak = [(0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7);
                       (14,0);(14,1);(14,2);(14,3);(14,4);(14,5);(14,6);(14,7);
                       (0,0);(1,0);(2,0);(3,0);(4,0);(5,0);(6,0);(7,0);(8,0);
                       (9,0);(10,0);(11,0);(12,0);(13,0);(14,0);(0,7);(1,7);
                       (2,7);(3,7);(4,7);(5,7);(6,7);(7,7);(8,7);(9,7);(10,7);
                       (11,7);(12,7);(13,7);(14,7);
                       (* cave*)
                        (7,1);(8,1); (6,1);
                       (* tree stump and rock *)
                       (1,1);(13,1);(12,1)]

let getObstacleCoordinates () =
  if Save.beat_game () then
    tilemap_nooak
  else
    tilemap_profoak
