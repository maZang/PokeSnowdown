open Async.Std
open Info

(*
  Communication Details:
  TODO
*)

(* Wait function to allow GUI to wait for some animations/text to be
  displayed.
*)
let busywait = let ctr = ref 0 in fun () -> ctr := 0;
  for i = 1 to 250_000_000 do
    incr ctr
  done

(* Screen width and height parameters *)
let screen_width = 600
let screen_height= 480

(* Initializes GtkMain*)
let locale = GtkMain.Main.init ()

(* Holds similar information to the engine, but acts differently in battle
  In battle, engine holds the current battle state, but current_screen holds
  the information on which player is selecting move/pokemon/etc...
*)
let current_screen = ref MainMenu

(* Holds information on the moves/commands of players*)
let current_command = ref (None, None)

(* Returns the current status of the game. Called when sure it will be used*)
let get_game_status engine =
  match Deferred.peek (Ivar.read !engine) with
  | Some v -> v
  | None -> failwith "Faultly game logic"

(*
  make_battle_screen makes all the components needed in the battle screen
  and sends it to the main function:
  -- battle is a 4x4 table containing the battle
  -- text is a box containing the text buffer
  -- bg_img holds the background image
  -- text_buffer holds the text in game
  -- poke1_img holds the image of the current pokemon
  -- poke2_img holds the image of the opponent pokemon
  -- move1 to move4 are buttons for the move of the current pokemon
  -- switch gives you options to switch pokemon
  -- poke1 to poke5 gives you the available pokemon to switch to
  -- health_bar_holder and health_bar are the containers and the actual health
      bar respectively. health_bar1 is for player one and health_bar2 is for
      player2.
*)
let make_battle_screen ?packing () =
  (* Creates a 4x4 table for battle *)
  let battle = GPack.table ~rows:4 ~columns: 4 ?packing ~width:screen_width
    ~height: (2 * screen_height / 3) ~show:false
     () in
  (* Container used to hold health bars to force the health bars to be a
  certain size *)
  let health_bar_holder1 = GPack.box `VERTICAL ~width:200 ~height:12 () in
  let health_bar_holder2 = GPack.box `VERTICAL ~width:200 ~height:12 () in
  (* Actual health bars implemented with progress bars *)
  let health_bar1 = GRange.progress_bar
                          ~packing:(health_bar_holder1#pack ~expand:false) () in
  let health_bar2 = GRange.progress_bar
                          ~packing:(health_bar_holder2#pack ~expand:false) () in
  (* Holder to hold the text buffer *)
  let text = GPack.hbox ?packing ~height: (1 * screen_height / 6) () in
  (* Background image of the battle: TODO RANDOMIZE BACKGROUND *)
  let bg_img = GMisc.image ~file:"../data/backgrounds/bg-volcanocave.jpg"
     () in
  (* The text buffer to hold the narration during the battle *)
  let text_buffer = GEdit.entry ~width:600 ~height:80
              ~text:"Player One's Turn To Move"
    ~packing:(text#pack ~expand:true) ~editable:false () in
  (* The images to hold each pokemon initialized to default images*)
  let poke1_img = GMisc.image ~file:"../data/back-sprites/charizard.gif" () in
  let poke2_img = GMisc.image ~file:"../data/sprites/blaziken-mega.gif" () in
  (* The buttons that hold the moves of the pokemon with labels set to default
  values. The buttons are placed in the main_menu hbox*)
  let move1 = GButton.button ~label:"Move1" ~show:false () in
  let move2 = GButton.button ~label:"Move2" ~show:false () in
  let move3 = GButton.button ~label:"Move3" ~show:false () in
  let move4 = GButton.button ~label:"Move4" ~show:false () in
  (* These buttons are for the switch pokemon commands in battle *)
  let switch = GButton.button ~label:"Switch" ~show: false() in
  let poke1 = GButton.button ~label:"Poke1" ~show:false () in
  let poke2 = GButton.button ~label:"Poke2" ~show:false () in
  let poke3 = GButton.button ~label:"Poke3" ~show:false () in
  let poke4 = GButton.button ~label:"Poke4" ~show:false () in
  let poke5 = GButton.button ~label:"Poke5" ~show:false () in
  (* Initialize size and text of the health bars*)
  health_bar1#set_fraction 1.;
  health_bar1#set_text "Health";
  health_bar2#set_fraction 1.;
  health_bar2#set_text "Health";
  (* Set font of the text box *)
  text_buffer#misc#modify_font
      (Pango.Font.from_string "arial,monospace condensed 10");
  (* Place the images within the 4x4 table. Holds the pokemon as well as the
  health bars: TODO, figure out how to do animations *)
  battle#attach ~left:0 ~top:1 ~right:2 ~bottom:4 poke1_img#coerce;
  battle#attach ~left:1 ~top:1 ~right:3 ~bottom:3 poke2_img#coerce;
  battle#attach ~left:0 ~top:3 ~right:2 ~bottom:4
                  ~fill:`NONE health_bar_holder1#coerce;
  battle#attach ~left:1 ~top:0 ~right:3 ~bottom:1
                  ~fill:`NONE health_bar_holder2#coerce;
  battle#attach ~left:0 ~top:0 ~right:4 ~bottom:4 ~fill:`BOTH bg_img#coerce;
  (* Return all the objects created *)
  battle, text, bg_img, move1, move2, move3, move4, switch, poke1_img,
  poke2_img, text_buffer, poke1, poke2, poke3, poke4, poke5,
  (health_bar_holder1, health_bar_holder2, health_bar1, health_bar2)

(* Make all the menu items for the game loading screen
  -- vbox holds all the game components
  -- hbox1 contains all the buttons
  -- hbox2 contains the game screen
  -- button1 is the 1player button
  -- button2 is the 2player button
  -- button3 is the no player button
  -- button4 is the random battle button
  -- button5 is the preset battle button
  -- button6 is the tournament button
  -- button7 is the back button packed to the back
  -- img holds the main screen image
  -- img2 holds the 1player background
  -- load_screen contains the load screen image
*)
let make_menu ?packing () =
  (* vbox is known as menu_holder outside of this function *)
	let vbox = GPack.vbox ?packing () in
  (* hbox1 is known as main_menu outside of this function *)
	let hbox1 = GPack.hbox ~homogeneous:true ~packing:(vbox#pack) ~height:
    (screen_height/6) ()in
  (* hbox2 is known as battle_screen outside of this function *)
	let hbox2 = GPack.vbox ~packing:(vbox#pack) () in
  (* button1 is known as one_player outside of this function*)
	let button1 = GButton.button ~label:"1-Player"
		~packing:(hbox1#pack ~expand:true ~fill:true) () in
  (* button2 is known as two_player outside of this function*)
	let button2 = GButton.button ~label:"2-Player"
		~packing:(hbox1#pack ~expand:true ~fill:true) () in
  (* button3 is known as no_player outside of this function *)
	let button3 = GButton.button ~label:"No Player"
		~packing:(hbox1#pack ~expand:true ~fill:true) ()  in
  (* button4 is known as random_1p outside of this function *)
	let button4 = GButton.button ~label:"Random Battle"
		~packing:(hbox1#pack ~expand:true ~fill:true) ~show:false () in
  (* button5 is known as preset_1p outside of this function *)
	let button5 = GButton.button ~label:"Preset Battle"
		~packing:(hbox1#pack ~expand:true ~fill:true) ~show:false () in
  (* button6 is known as tournament outside of this function *)
	let button6 = GButton.button ~label:"Tournament"
		~packing:(hbox1#pack ~expand:true ~fill:true) ~show:false () in
  (* button7 is known as back_button outside of this function *)
	let button7 = GButton.button ~label:"Back"
		~packing:(hbox1#pack ~from:`END) () ~show:false in
  (* img is known as main_menu_bg outside of this code *)
  let img = GMisc.image ~file:"./gui_pics/main.gif" ~packing:(hbox2#pack)
    ~width:screen_width ~height:(5*screen_height /6) () in
  (* img2 is known as one_bg outside of this code *)
  let img2 = GMisc.image ~file:"./gui_pics/1p.jpg" ~packing:(hbox2#pack)
    ~width:screen_width ~height:(5*screen_height /6) ~show:false () in
  (* load_screen is a gif that plays before battle (during initialization)*)
  let load_screen = GMisc.image ~file:"../data/backgrounds/background.gif"
    ~show:false ~packing:(vbox#pack) () in
  (* Return all objects created *)
  (vbox, hbox1, hbox2, button1, button2, button3, button4,
		button5, button6, button7, img, img2, load_screen)

(* This is called once to load the battle screen . The game states will be
  engine -- Battle InGame _
  current_screen -- Battle P1 _ || Battle P2 _
  current_command -- Some _, Some _
  @PARAMS:
    --engine is the game state
    --img is the background image to hide
    --battle is the 4x4 table
    --text is the text box
    --button_hide consists of [random_1p;preset_1p;touranment]
    --button_show consists of [move1;move2;move3;move4;switch]
    --battle_status is the Ivar containing the current game mode
    --gui_ready contains the gui info
    --poke1_img contains the image of Player 1's poke
    --poke2_img contains the image of Player 2's poke
    --text_buffer contains the text itself
    --health_bar 1 and 2 are health bars for player 1 and 2 respectively

    NOTE SOME PARAMS MIGHT BE NOT USED AS OF NOW
*)
let load_battle_screen engine img battle text buttonhide buttonshow
  (battle_status, gui_ready) poke1_img poke2_img text_buffer
  (_, _, health_bar1, health_bar2) () =
  (* get the components of button show to manipulate *)
  let [move1;move2;move3;move4;_] = buttonshow in
  (* get current team information *)
  let team1, team2, weather = match get_game_status engine with
    | Battle InGame (t1, t2, w, _, _) -> (t1, t2, w)
    | _ -> failwith "Impossible" in
  let poke1 = (team1.current).pokeinfo.name in
  let poke2 = (team2.current).pokeinfo.name in
  img#misc#hide (); battle#misc#show (); text#misc#show ();
  (* hide and show necessary buttons *)
  List.iter (fun s -> s#misc#hide ()) buttonhide;
  List.iter (fun s -> s#misc#show ()) buttonshow;
  current_screen := Battle (P1 ChooseMove);
  move1#set_label (team1.current).pokeinfo.move1.name;
  move2#set_label (team1.current).pokeinfo.move2.name;
  move3#set_label (team1.current).pokeinfo.move3.name;
  move4#set_label (team1.current).pokeinfo.move4.name;
  (* set tooltips *)
  move1#misc#set_has_tooltip true;
  move2#misc#set_has_tooltip true;
  move3#misc#set_has_tooltip true;
  move4#misc#set_has_tooltip true;
  (* get tooltip information *)
  move1#misc#set_tooltip_text
          (Pokemon.getMoveToolTip team1.current.pokeinfo.move1);
  move2#misc#set_tooltip_text
          (Pokemon.getMoveToolTip team1.current.pokeinfo.move2);
  move3#misc#set_tooltip_text
          (Pokemon.getMoveToolTip team1.current.pokeinfo.move3);
  move4#misc#set_tooltip_text
          (Pokemon.getMoveToolTip team1.current.pokeinfo.move4);
  (* set Pokemon pictures *)
  poke1_img#set_file ("../data/back-sprites/" ^ poke1 ^ ".gif");
  poke2_img#set_file ("../data/sprites/" ^ poke2 ^ ".gif");
  (* set health bar information; give tooltips to health bars *)
  health_bar1#misc#set_has_tooltip true;
  health_bar1#set_fraction 1.;
  health_bar1#set_text (string_of_int team1.current.curr_hp ^ "/" ^ string_of_int team1.current.bhp);
  health_bar1#misc#set_tooltip_text (Pokemon.getPokeToolTip team1);
  health_bar2#misc#set_has_tooltip true;
  health_bar2#set_fraction 1.;
  health_bar2#set_text (string_of_int team2.current.curr_hp ^ "/" ^ string_of_int team2.current.bhp);
  health_bar2#misc#set_tooltip_text (Pokemon.getPokeToolTip team2)

(* In contrast to other cases, after engine is filled up with a battle status
the game controller hands control to the battle controller. Some screens are hid
the current screen is set to Battle Loading. The overall outlook is
engine -- Battle Loading
current_screen -- Battle Loading
After the callback is initiated, the battle information is read and the battle
is loaded.Then
engine -- Battle InGame
*)
let load_battle_load engine img load_screen battle text buttonhide buttonshow
  (battle_status, gui_ready, ready, ready_gui) mode main_menu battle_screen
  poke1_img poke2_img text_buffer health_holders () =
  (* wait for engine to be filled by the battle controller *)
  if (Ivar.is_empty (!engine)) then
    ((main_menu#misc#hide (); battle_screen#misc#hide ();
    load_screen#misc#show (); current_screen :=
    (Battle Loading); Ivar.fill !engine (Battle Loading);
    Printf.printf "Initializing gui\n%!";
    Ivar.fill !battle_status mode); let rec load_helper () =
    upon (Ivar.read !engine) (fun s -> match s with
      | Battle InGame _ ->
          (main_menu#misc#show ();
          battle_screen#misc#show ();
          load_screen#misc#hide ();
          load_battle_screen engine img battle text buttonhide buttonshow
          (battle_status, gui_ready) poke1_img poke2_img
          text_buffer health_holders ())
      | _ -> load_helper ()) in load_helper())
  else
    ()

let load_menu engine button_show button_hide img1 img2 screen () =
	if Ivar.is_empty (!engine) then
		(List.iter (fun s -> s#misc#hide ()) button_hide;
    List.iter (fun s -> s#misc#show ()) button_show;
    img1#misc#hide (); img2#misc#show ();
    current_screen := screen; Ivar.fill !engine screen)
	else
		()

let load_main_menu_from_battle engine one_player two_player no_player button_hide
 main_menu_bg battle text (battle_status, gui_ready, ready, ready_gui) () =
 if (match Deferred.peek (Ivar.read (!engine)) with
      | Some Battle InGame _ -> true
      | _ -> false) then
  (engine := Ivar.create (); Thread.delay 0.1;
  List.iter (fun s -> s#misc#hide ()) button_hide;
  List.iter (fun s -> s#misc#show ())[one_player; two_player; no_player];
  battle#misc#hide (); text#misc#hide (); main_menu_bg#misc#show ();
  current_screen := MainMenu; Ivar.fill !engine MainMenu; Ivar.fill !ready true;
  battle_status := Ivar.create (); current_command := (None, None);
  gui_ready := Ivar.create ())
else
  ()

let go_back engine (menu_holder, main_menu, battle_scren, one_player,
    two_player, no_player, random_1p, preset_1p, touranment,
    back_button, main_menu_bg, one_bg, load_screen) (battle, text, bg_img, move1, move2,
    move3, move4, switch, poke1_img, poke2_img, text_buffer, poke1, poke2,
    poke3, poke4, poke5, health_holders) battle_engine () =
	(if !current_screen = Menu1P then
		load_menu engine [one_player;two_player;no_player]
    [random_1p; preset_1p ;touranment; back_button] one_bg main_menu_bg
    MainMenu ());
  if (match !current_screen with
    | Battle (P1 ChooseMove)-> true
    | _ -> false ) then
    (load_main_menu_from_battle engine one_player two_player no_player [move1;
    move2; move3; move4; switch; back_button] main_menu_bg battle text
    battle_engine ());
  if (!current_screen = Battle (P1 SwitchPoke) || !current_screen = Battle (P2 SwitchPoke))  then
    (List.iter (fun s -> s#misc#show ()) [move1;move2;move3;move4;switch];
    List.iter (fun s -> s#misc#hide ()) [poke1;poke2;poke3;poke4;poke5];
    if (!current_screen = Battle (P1 SwitchPoke)) then
      current_screen := Battle (P1 ChooseMove)
    else
      current_screen := Battle (P2 ChooseMove));
  if (!current_screen = Battle Processing) then
    let battle_status, gui_ready, ready, ready_gui = battle_engine in
    Printf.printf "DID I FILL IT %B\n%!" (Ivar.is_full !gui_ready); ()

let switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
  move3;move4;switch] back_button () =
  let switch_poke_helper i a =
    let big_help poke = poke#set_label a.pokeinfo.name; poke#misc#show () in
    match i with
    | 0 -> big_help poke1
    | 1 -> big_help poke2
    | 2 -> big_help poke3
    | 3 -> big_help poke4
    | 4 -> big_help poke5
    | _ -> failwith "Invariant is not having more than five pokemon" in
  List.iter (fun s -> s#misc#hide ()) [move1;move2;move3;move4;switch];
  let team = (match get_game_status engine with
  | Battle (InGame (t1, t2, _, _, _)) ->
      match !current_screen with
      | Battle (P1 BothFaint) -> back_button#misc#hide (); t1
      | Battle (P1 Faint) -> back_button#misc#hide (); t1
      | Battle (P2 Faint) -> back_button#misc#hide (); t2
      | Battle (P1 _) -> current_screen := Battle (P1 SwitchPoke); t1
      | Battle (P2 _) -> current_screen := Battle (P2 SwitchPoke); t2
      | _ -> failwith "Faulty Game logic"
  | _ -> failwith "Fauly game Logic") in
  List.iteri switch_poke_helper team.alive

let rec getNumCritSuperNoAndFinal c s n v=
  match v with
  | NormMove str -> (c, s, n, str)
  | Crit v-> getNumCritSuperNoAndFinal (c+1) s n v
  | SEff v -> getNumCritSuperNoAndFinal c (s+1) n v
  | NoEff v -> getNumCritSuperNoAndFinal c s (n+1) v
  | _ -> failwith "Bad move"

let secondaryEffect = ref `P1
let endTurnEarly = ref false

let rec string_from_stat s =
  match s with
  | Attack -> "Attack"
  | Defense -> "Defense"
  | SpecialAttack -> "Special Attack"
  | SpecialDefense -> "Sepcial Defense"
  | Speed -> "Speed"
  | Accuracy -> "Accuracy"
  | Evasion -> "Evasion"

let rec getAttackString starter a =
  match a with
  | NormMove s -> starter ^ " used " ^ s ^"."
  | Crit s -> getAttackString starter s ^ "It was a critical hit."
  | SEff s -> getAttackString starter s ^ "It was super effective."
  | NoEff s -> getAttackString starter s ^ "It was not very effective."
  | NoEffAll s -> starter ^ " used " ^ s ^ " but it had no effect."
  | StatAttackA (stat, i, s) -> getAttackString starter s ^ "Opponent's " ^ string_from_stat stat ^ " was lowered " ^ string_of_int i ^ " stage."
  | HitMult (n, s) ->
      let c, s', n', str = getNumCritSuperNoAndFinal 0 0 0 s in
      starter ^ " used " ^ str ^ ". The move hit " ^ string_of_int n ^ " times with " ^ string_of_int
      c ^ " crits." ^ (if s' > 0 then "It was supereffective." else if n' > 0 then
      "It was not very effective." else "")
  | BurnMove s -> getAttackString starter s ^ "The opponent has been burned"
  | FreezeMove s -> getAttackString starter s ^ "The opponent is frozen solid"
  | ParaMove s -> getAttackString starter s ^ "The opponent has been paralyzed"
  | MissMove s ->  starter ^ " used " ^ s ^ " but it missed!"
  | Asleep -> starter ^ " was fast asleep!"
  | Wake s -> starter ^ " woke up." ^ getAttackString starter s
  | FrozenSolid -> starter ^ " was frozen solid!"
  | Thaw s-> starter ^ " unfroze." ^ getAttackString starter s
  | NoFreeze s -> starter ^ " cannot be frozen." ^ getAttackString starter s
  | NoBurn s -> starter ^ " cannot be burned." ^ getAttackString starter s
  | NoPara s -> starter ^ " cannot be paralyzed." ^ getAttackString starter s
  | Para -> starter ^ " couldn't move due to paralysis!"
  | OHKill s -> getAttackString starter s ^"This move is a one hit KO!"
  | FlinchA -> starter ^ " flinched!"
  | PoisonMove s -> getAttackString starter s ^ "The opponent has been poisoned"
  | Recoil s -> getAttackString starter s ^"The user suffered some recoil damage!"
  | BreakConfuse s -> starter ^ " has broken out of its confusion." ^ getAttackString starter s
  | Confused -> starter ^ " hit itself in its confusion."
  | DrainA s-> getAttackString starter s ^ starter ^ " drained some of it's opponents health."
  | ConfuseMoveA s -> getAttackString starter s ^ "The opponent is confused."
  | UserFaintA s -> getAttackString starter s ^ starter ^ " takes damage from the move and is about to faint!"
  | DrainSleepFail s -> starter ^ " used " ^ s ^ " but the opponent is not asleep!"
  | ChargingMove (s, n) -> (match !secondaryEffect with
                             | `P1 -> current_command := (Some (UseAttack n), snd !current_command)
                             |`P2 -> current_command := (fst !current_command, Some (UseAttack n))); starter ^ " is charging up." ^ s
  | Recharging s -> (match !secondaryEffect with
                        | `P1 -> current_command := (Some (NoMove), snd !current_command)
                        | `P2 -> current_command := (fst !current_command, Some NoMove)); getAttackString starter s ^ starter ^ " will need a turn to recharge."

(* starter is either 'Player One' or 'Player Two'*)
let rec getStatusString starter s =
  match s with
  | NormStatus s -> starter ^ " used " ^ s ^ "."
  | StatBoost (stat, i, s) -> getStatusString starter s ^ string_from_stat stat ^ " was boosted " ^ string_of_int i ^ " stage."
  | StatAttack (stat, i, s) -> getStatusString starter s ^ "Opponent's " ^ string_from_stat stat ^ " was lowered " ^ string_of_int i ^ " stage"
  | MissStatus s -> starter ^ " used " ^ s ^ " but it missed."
  | FrozenSolidS -> starter ^  " was frozen solid."
  | PoisonStatus s-> getStatusString starter s ^ "The opponent has been poisoned."
  | BadPoisonStatus s -> getStatusString starter s ^ "The opponent has been badly poisoned."
  | ParaStatus s -> getStatusString starter s ^ "The opponent has been paralyzed."
  | ThawS s -> " unfroze." ^ getStatusString starter s
  | NoFreezeS s -> starter ^ " cannot freeze. " ^ getStatusString starter s
  | NoBurnS s -> starter ^ " cannot burn. " ^ getStatusString starter s
  | NoParaS s -> " cannot be paralyzed." ^ getStatusString starter s
  | ParaS -> starter ^ " was paralyzed."
  | AsleepS -> starter ^ " is still asleep."
  | WakeS s -> starter ^ " woke up." ^ getStatusString starter s
  | MakeSleep s -> getStatusString starter s ^ "The opponent has fallen asleep."
  | FlinchS -> starter ^ " flinched."
  | BreakConfuseS s-> starter ^ " has broken out of its confusion." ^ getStatusString starter s
  | ConfusedS -> starter ^ " hit itself in its confusion."
  | ConfuseMove s -> getStatusString starter s ^ "The opponent is confused."
  | LeechS s -> getStatusString starter s ^ "Seeds were spread around the opponent."
  | HealHealth s -> getStatusString starter s ^ starter ^ " healed itself for some of its health."
  | LightScreenS s -> getStatusString starter s ^ starter ^ " has put up a field protecting it from special attacks."
  | HazeS s -> getStatusString starter s ^ "Both Pokemon's stat changes were removed."
  | ReflectS s -> getStatusString starter s ^ starter ^ " has put up a field protecting it from physical attacks."
  | RestS s -> getStatusString starter s ^ starter ^ " has fallen asleep and been completely restored."
  | SwitchOut s -> (match !secondaryEffect with
                    | `P1 -> current_command := (Some NoMove, Some (Poke "random"))
                    | `P2 -> current_command := (Some (Poke "random"), Some NoMove));
                    endTurnEarly := true; getStatusString starter s ^ "The opponent was forced out!"

let rec getEndString starter s =
  match s with
  | Base -> ""
  | BreakBurn -> starter ^ " cannot be burned."
  | BreakFreeze -> starter ^ " cannot be frozen."
  | BreakPara -> starter ^ " cannot be paralyzed."
  | BreakPoison -> starter ^ " cannot be posioned."
  | BurnDmg -> starter ^ " has taken burn damage."
  | PoisonDmg -> starter ^ " has taken poison damage."
  | LeechDmg s -> starter ^ " has taken leech seed damage." ^ getEndString starter s
  | LeechHeal s -> starter ^ " has healed from leech seeds." ^ getEndString starter s
  | LightScreenFade s -> getEndString starter s ^ starter ^ "'s Light Screen has faded."
  | ReflectFade s -> getEndString starter s ^ starter ^ "'s Reflect has faded."

let rec game_animation engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text
  (battle_status, gui_ready, ready, ready_gui) poke1_img poke2_img text_buffer (health_bar_holder1, health_bar_holder2, health_bar1, health_bar2)  back_button () =
  Printf.printf "DEBUG %B %B\n%!" (Ivar.is_empty !gui_ready) !endTurnEarly;
  let battle_buttons = [move1; move2; move3; move4; switch; back_button] in
  List.iter (fun s -> s#misc#hide ()) battle_buttons;
  let t1, t2, w, m1, m2 = match get_game_status engine with
    | Battle InGame (t1, t2, w, m1, m2) -> t1, t2, w, m1, m2
    | _ -> failwith "Fauly Game Logic: Debug 05" in
  let game_step () =
    upon (Ivar.read !ready_gui) (fun _ -> ready_gui := Ivar.create ();
    game_animation engine [move1; move2; move3; move4; poke1; poke2; poke3;
    poke4; poke5; switch] battle text (battle_status, gui_ready, ready,
    ready_gui) poke1_img poke2_img text_buffer (health_bar_holder1,
    health_bar_holder2, health_bar1, health_bar2) back_button ()) in
  let updatehealth1 () =
    health_bar1#set_fraction (float_of_int (t1.current).curr_hp /. float_of_int (t1.current).bhp);
    health_bar1#set_text (string_of_int t1.current.curr_hp ^ "/" ^ string_of_int t1.current.bhp) in
  let updatehealth2 () =
    health_bar2#set_fraction (float_of_int (t2.current).curr_hp /. float_of_int (t2.current).bhp);
    health_bar2#set_text (string_of_int t2.current.curr_hp ^ "/" ^ string_of_int t2.current.bhp) in
  let updatetools () =
    move1#set_label (t1.current).pokeinfo.move1.name;
    move2#set_label (t1.current).pokeinfo.move2.name;
    move3#set_label (t1.current).pokeinfo.move3.name;
    move4#set_label (t1.current).pokeinfo.move4.name;
    move1#misc#set_tooltip_text (Pokemon.getMoveToolTip t1.current.pokeinfo.move1);
    move2#misc#set_tooltip_text (Pokemon.getMoveToolTip t1.current.pokeinfo.move2);
    move3#misc#set_tooltip_text (Pokemon.getMoveToolTip t1.current.pokeinfo.move3);
    move4#misc#set_tooltip_text (Pokemon.getMoveToolTip t1.current.pokeinfo.move4);
    health_bar1#misc#set_tooltip_text (Pokemon.getPokeToolTip t1);
    health_bar2#misc#set_tooltip_text (Pokemon.getPokeToolTip t2);
    updatehealth1 (); updatehealth2 () in
  let skipturn () =
    match get_game_status battle_status with
    | Random1p -> (match !current_command with
                  | (None, _) -> text_buffer#set_text "Player One's Turn to move"; List.iter (fun s -> s#misc#show ()) battle_buttons; current_screen := Battle (P1 ChooseMove);
                                    current_screen := Battle (P1 ChooseMove)
                  | _ -> Ivar.fill !gui_ready !current_command; current_command := (None, None); game_step ())
    | _ -> failwith "Faulty Game Logic: Debug 100" in
  let simple_move () =
    updatetools ();
    skipturn () in
  let turn_end () =
    (Ivar.fill !gui_ready (Some TurnEnd, Some TurnEnd); game_step ()) in
  let pre_process () =
    if !endTurnEarly then
      (endTurnEarly := false; skipturn ())
    else
      (Ivar.fill !gui_ready (Some Preprocess, Some Preprocess);
      game_step ()) in
  (match !m1 with
  | Pl1 SPoke p -> text_buffer#set_text ("Player One has switched to " ^ p); poke1_img#set_file ("../data/back-sprites/" ^ p ^ ".gif");updatetools ();busywait ()
  | Pl2 SPoke p -> text_buffer#set_text ("Player Two has switched to " ^ p); poke2_img#set_file ("../data/sprites/" ^ p ^ ".gif"); updatetools (); busywait ()
  | Pl1 AttackMove a -> secondaryEffect := `P1;
                   let atk_string = getAttackString t1.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\.") atk_string in
                   updatehealth2 ();
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list
  | Pl2 AttackMove a -> secondaryEffect := `P2;
                   let atk_string = getAttackString t2.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\.") atk_string in
                   updatehealth1 ();
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list
  | Pl1 Status s ->secondaryEffect := `P1;
                   let status_string = getStatusString t1.current.pokeinfo.name s in
                   let str_list = Str.split (Str.regexp "\.") status_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list
  | Pl2 Status s ->secondaryEffect := `P2;
                   let status_string = getStatusString t2.current.pokeinfo.name s in
                   let str_list = Str.split (Str.regexp "\.") status_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list
  | Pl1 NoAction -> text_buffer#set_text ("Both Pokemon Not Doing Anything"); busywait ()
  | Pl2 NoAction -> text_buffer#set_text ("Both Pokemon Not Doing Anything"); busywait ()
  | Pl1 Continue | Pl2 Continue | Pl1 Next | Pl2 Next -> ()
  | Pl1 Faint -> text_buffer#set_text "Player One Pokemon has fainted. Choosing a new Pokemon.";
                  (match !m2 with
                  | Pl2 Faint -> current_screen := Battle (P1 BothFaint)
                  | _ -> current_screen := Battle (P1 Faint));
                  busywait (); updatetools ();
                  switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                  move3;move4;switch] back_button ()
  | Pl2 Faint -> text_buffer#set_text "Player Two Pokemon has fainted. Choosing a new Pokemon.";
                 (match get_game_status battle_status with
                 | Random1p -> busywait (); current_command := (Some (NoMove), Some (FaintPoke ""));
                               simple_move()
                 | _ -> failwith "Faulty Game Logic: Debug 007")
  | Pl1 EndMove x -> let txt = getEndString t1.current.pokeinfo.name x in
                    let str_list = Str.split (Str.regexp "\.") txt in
                    List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                    updatehealth1 ()
  | _ -> failwith "unimplements");
  if !endTurnEarly then
    (endTurnEarly := false; skipturn ())
  else
  (match !m2 with
  | Pl1 AttackMove a -> secondaryEffect := `P1;
                   let atk_string = getAttackString t1.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\.") atk_string in
                   updatetools ();
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   pre_process ()
  | Pl2 AttackMove a -> secondaryEffect := `P2;
                   let atk_string = getAttackString t2.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\.") atk_string in
                   updatetools ();
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   pre_process ()
  | Pl1 Status s ->secondaryEffect := `P1;
                   let status_string = getStatusString t1.current.pokeinfo.name s in
                   let str_list = Str.split (Str.regexp "\.") status_string in
                   updatetools ();
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   pre_process ()
  | Pl2 Status s ->secondaryEffect := `P2;
                   let status_string = getStatusString t2.current.pokeinfo.name s in
                   let str_list = Str.split (Str.regexp "\.") status_string in
                   updatetools ();
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   pre_process ()
  | Pl1 Faint -> text_buffer#set_text "Player One Pokemon has fainted. Choosing a new Pokemon.";
                  (match get_game_status battle_status with
                  | Random1p -> busywait (); updatetools (); current_screen := Battle (P1 Faint);
                                switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                                move3;move4;switch] back_button ()
                  | _ -> failwith "Faulty Game Logic: Debug 008"
                  )
  | Pl1 Continue -> turn_end ()
  | Pl2 Continue -> turn_end ()
  | Pl1 Next | Pl2 Next -> simple_move ()
  | Pl1 FaintNext | Pl2 FaintNext -> ()
  | Pl1 NoAction -> pre_process ()
  | Pl2 NoAction -> pre_process ()
  | Pl2 EndMove x -> let txt = getEndString t2.current.pokeinfo.name x in
                    let str_list = Str.split (Str.regexp "\.") txt in
                    List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                    updatehealth2 (); turn_end ()
  | _ -> failwith "unimplement")


 let process_command engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text
  (battle_status, gui_ready, ready, ready_gui) poke1_img poke2_img text_buffer health_holders back_button =
 let battle_buttons = [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch]  in
 match !current_command with
 | (None, None) -> failwith "Faulty Game Logic: Debug 03"
 | (Some _, None) -> current_screen := Battle (P2 ChooseMove);
                      (match get_game_status battle_status with
                      | Random1p -> List.iter (fun s -> s#misc#hide ()) battle_buttons; current_screen := Battle Processing; text_buffer#set_text "Both moves collected. Processing...";
                                          Ivar.fill !gui_ready !current_command; current_command := (None, None);
                                          upon (Ivar.read !ready_gui) (fun _ -> ready_gui := Ivar.create (); game_animation engine battle_buttons battle text
                                                                        (battle_status, gui_ready, ready, ready_gui) poke1_img poke2_img text_buffer health_holders back_button ())
                      | _ -> failwith "Faulty Game Logic: Debug 01")
 | (Some _, Some _) -> current_screen := Battle Processing; List.iter (fun s -> s#misc#hide ()) battle_buttons; current_screen := Battle Processing; text_buffer#set_text "Both moves collected. Processing...";
                                          Ivar.fill !gui_ready !current_command; current_command := (None, None);
                                          upon (Ivar.read !ready_gui) (fun _ -> ready_gui := Ivar.create (); game_animation engine battle_buttons battle text
                                                                        (battle_status, gui_ready, ready, ready_gui) poke1_img poke2_img text_buffer health_holders back_button ())

let poke_move_cmd button engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text
  (battle_status, gui_ready, ready, ready_gui) poke1_img poke2_img text_buffer health_holders back_button () =
  Printf.printf "Using a move!\n%!";
  let _ = match !current_command with
 | None, None -> current_command := (Some (UseAttack (button#label)), None)
 | None, Some x -> current_command := (Some (UseAttack (button#label)), Some x)
 | Some x, None -> current_command := Some x, Some (UseAttack button#label)
 | Some _, Some _ -> failwith "Faulty Game Logic: Debug 06" in
 process_command engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text
  (battle_status, gui_ready, ready, ready_gui) poke1_img poke2_img text_buffer health_holders back_button

 let switch_poke_cmd button engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text
  (battle_status, gui_ready, ready, ready_gui) poke1_img poke2_img text_buffer health_holders back_button () =
 Printf.printf "Switching Pokemon\n%!";
 let next () =
  process_command engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text
  (battle_status, gui_ready, ready, ready_gui) poke1_img poke2_img text_buffer health_holders back_button in
 match !current_screen with
  | Battle (P1 BothFaint) ->
        (match get_game_status battle_status with
        | Random1p -> current_command := (Some (FaintPoke (button#label)), Some (FaintPoke "")); next ()
        (* In two player, you would call switch_poke command again *)
        | _ -> failwith "Faulty Game Logic: Debug 616")
  | Battle (P1 Faint) -> current_command := (Some (FaintPoke (button#label)), snd !current_command); next ()
  | Battle (P2 Faint) -> current_command := (fst !current_command, Some (FaintPoke (button#label))); next ()
  | Battle (P1 SwitchPoke) -> current_command := (Some (Poke (button#label)), snd !current_command); next ()
  | Battle (P2 SwitchPoke) -> current_command := (fst !current_command, Some (Poke (button#label))); next ()
  | _ -> failwith "Faulty Game Logic: Debug 449"


let quit engine ready () =
  match !current_screen with
  | Battle _ -> engine := Ivar.create (); Thread.delay 0.1; Ivar.fill !engine Quit; Ivar.fill !ready true
  | _ -> Ivar.fill !engine Quit

(* The main gui *)
let main_gui engine battle_engine () =
	let window = GWindow.window ~width: screen_width ~height: screen_height
		~title: "Pokemon Snowdown" ~resizable:false () in
	(* menu = menu_holder, main_menu, one_player, two_player, no_player,
		one_player_menu, random_1p, preset_1p, touranment, buffer_area,
		back_button *)
  let battle_status, gui_ready, ready, ready_gui = battle_engine in
	let menu = make_menu ~packing:(window#add) () in
	let menu_holder, main_menu, battle_screen, one_player,
		two_player, no_player, random_1p, preset_1p, touranment,
		back_button, main_menu_bg, one_bg, load_screen = menu in
  let battler = make_battle_screen ~packing:(battle_screen#add) ()
  in let battle, text, bg_img, move1, move2, move3, move4, switch,
    poke1_img, poke2_img, text_buffer, poke1, poke2, poke3, poke4,
    poke5, health_holders = battler in
  main_menu#pack move1#coerce; main_menu#pack move2#coerce;
  main_menu#pack move3#coerce; main_menu#pack move4#coerce;
  main_menu#pack switch#coerce; main_menu#pack poke1#coerce;
  main_menu#pack poke2#coerce; main_menu#pack poke3#coerce;
  main_menu#pack poke4#coerce; main_menu#pack poke5#coerce;
  (* One player Button *)
	one_player#connect#clicked ~callback:(load_menu engine [random_1p;preset_1p;
  touranment;back_button] [one_player; two_player;no_player] main_menu_bg one_bg
  Menu1P);
 (* Back button *)
   back_button#connect#clicked
  ~callback:(go_back engine menu battler battle_engine);
  (* Random 1p battle button *)
  random_1p#connect#clicked ~callback:(load_battle_load engine one_bg load_screen
  battle text [random_1p;preset_1p;touranment] [move1; move2; move3; move4;
  switch] battle_engine Random1p main_menu battle_screen poke1_img poke2_img
  text_buffer health_holders);
  (* Switch button *)
  switch#connect#clicked ~callback:(switch_poke engine [poke1;poke2;poke3;
  poke4;poke5] [move1;move2;move3;move4;switch] back_button);
  (* Pokemon buttons *)
  poke1#connect#clicked ~callback:(switch_poke_cmd poke1 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine poke1_img poke2_img text_buffer health_holders back_button);
  poke2#connect#clicked ~callback:(switch_poke_cmd poke2 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine poke1_img poke2_img text_buffer health_holders back_button);
  poke3#connect#clicked ~callback:(switch_poke_cmd poke3 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine poke1_img poke2_img text_buffer health_holders back_button);
  poke4#connect#clicked ~callback:(switch_poke_cmd poke4 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine poke1_img poke2_img text_buffer health_holders back_button);
  poke5#connect#clicked ~callback:(switch_poke_cmd poke5 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine poke1_img poke2_img text_buffer health_holders back_button);
  (* Move buttons *)
  move1#connect#clicked ~callback:(poke_move_cmd move1 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine poke1_img poke2_img text_buffer health_holders back_button);
	window#show ();
  move2#connect#clicked ~callback:(poke_move_cmd move2 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine poke1_img poke2_img text_buffer health_holders back_button);
  window#show ();
  move3#connect#clicked ~callback:(poke_move_cmd move3 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine poke1_img poke2_img text_buffer health_holders back_button);
  window#show ();
  move4#connect#clicked ~callback:(poke_move_cmd move4 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine poke1_img poke2_img text_buffer health_holders back_button);
  window#connect#destroy ~callback:(quit engine ready);
  window#show ();
	let thread = GtkThread.start () in ()