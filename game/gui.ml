open Async.Std
open Info

(*
  Communication Details:
  TODO
*)

(* Wait function to allow GUI to wait for some animations/text to be
  displayed.
*)
(* let busywait = let ctr = ref 0 in fun () -> ctr := 0;
  for i = 1 to 100_000_000 do
    incr ctr
  done *)

let busywait_small = let ctr = ref 0 in fun () -> ctr := 0;
  for i = 1 to 800_000 do
    incr ctr
  done

(* Screen width and height parameters *)
let screen_width = 600
let screen_height= 480

(* base x and y position of the two Pokemon *)
let poke1x = 120
let poke1y = 165
let poke2x = 450
let poke2y = 80

(* used for user input *)
let continue = ref false

let busywait () =
  while not !continue do
    ()
  done;
  continue := false

(* Initializes GtkMain*)
let locale = GtkMain.Main.init ()

(* randomize battle background *)
let bg_string = ref ("../data/backgrounds/bg-volcanocave.png")

let rec test_string n () =
  match n with
  | 0 -> []
  | n -> (string_of_int n)::test_string (n-1) ()

(* Global references to an object for easy destruction *)
let selecttext = ref (GMisc.label ())
let select1 = ref (GEdit.combo ~popdown_strings:(test_string 750 ()) ())
let select2 = ref (GEdit.combo ~popdown_strings:(test_string 750 ()) ())
let select3 = ref (GEdit.combo ~popdown_strings:(test_string 750 ()) ())
let select4 = ref (GEdit.combo ~popdown_strings:(test_string 750 ()) ())
let select5 = ref (GEdit.combo ~popdown_strings:(test_string 750 ()) ())
let select6 = ref (GEdit.combo ~popdown_strings:(test_string 750 ()) ())
let selectimg = GMisc.image ~file:"../data/backgrounds/PokemonLogo.png" ~show:false ()
let () = !select1#destroy (); !select2#destroy (); !select3#destroy ();
          !select4#destroy (); !select5#destroy (); !select6#destroy ()
(* Holds similar information to the engine, but acts differently in battle
  In battle, engine holds the current battle state, but current_screen holds
  the information on which player is selecting move/pokemon/etc...
*)
let current_screen = ref MainMenu


let space_press s =
  if GdkEvent.Key.keyval s = 32 then
    match !current_screen with
    | Battle _ -> continue := true; true
    | _ -> false
  else
    false

(* Holds information on the moves/commands of players*)
let current_command = ref (None, None)

(* Changes the bg of the game match *)
let getRandomBg () =
  let base_string = "../data/backgrounds/" in
  match Random.int 15 with
  | 0 -> bg_string := base_string ^ "bg-beach.png"
  | 1 -> bg_string := base_string ^ "bg-beachshore.png"
  | 2 -> bg_string := base_string ^ "bg-city.png"
  | 3 -> bg_string := base_string ^ "bg-dampcave.png"
  | 4 -> bg_string := base_string ^ "bg-deepsea.png"
  | 5 -> bg_string := base_string ^ "bg-desert.png"
  | 6 -> bg_string := base_string ^ "bg-earthycave.png"
  | 7 -> bg_string := base_string ^ "bg-forest.png"
  | 8 -> bg_string := base_string ^ "bg-icecave.png"
  | 9 -> bg_string := base_string ^ "bg-meadow.png"
  | 10 -> bg_string := base_string ^ "bg-mountain.png"
  | 11 -> bg_string := base_string ^ "bg-river.png"
  | 12 -> bg_string := base_string ^ "bg-route.png"
  | 13 -> bg_string := base_string ^ "bg-thunderplains.png"
  | 14 -> bg_string := base_string ^ "bg-volcanocave.png"

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
  let text = GPack.hbox ?packing ~height: (1 * screen_height / 6) ~show:false () in
  (* Background image of the battle: TODO RANDOMIZE BACKGROUND *)
  let bg_img = GMisc.image ~file:!bg_string
     () in
  (* The text buffer to hold the narration during the battle *)
  let text_buffer = GEdit.entry ~width:600 ~height:80
              ~text:"Player One's Turn To Move. Press Space Bar to continue text commands."
    ~packing:(text#pack ~expand:true) ~editable:false () in
  (* The images to hold each pokemon initialized to default images*)
  let poke1_img = GMisc.image ~file:"../data/back-sprites/charizard.gif" () in
  let poke2_img = GMisc.image ~file:"../data/sprites/blaziken-mega.gif" () in
  let move_img = GMisc.image ~file:"../data/fx/fairywisp.png" ~show:false () in
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
  (* animation boxes for the pokemon and moves *)
  let pokeanim1 = GPack.fixed ~width:screen_width ~height:(2 * screen_height/3) () in
  let pokeanim2 = GPack.fixed ~width:screen_width ~height:(2 * screen_height/3) () in
  let moveanim = GPack.fixed ~width:screen_width ~height:(2 * screen_height/3) () in
  (* Initialize size and text of the health bars*)
  health_bar1#set_fraction 1.;
  health_bar1#set_text "Health";
  health_bar2#set_fraction 1.;
  health_bar2#set_text "Health";
  (* Set font of the text box *)
  text_buffer#misc#modify_font
      (Pango.Font.from_string "arial,monospace condensed 10");
  (* Place pokemon in animation box *)
  pokeanim1#put poke1_img#coerce poke1x poke1y;
  pokeanim2#put poke2_img#coerce poke2x poke2y;
  moveanim#put move_img#coerce 200 200;
  (* Place the images within the 4x4 table. Holds the pokemon as well as the
  health bars: TODO, figure out how to do animations *)
  battle#attach ~left:0 ~top:0 ~right:4 ~bottom:4 ~fill:`BOTH moveanim#coerce;
  battle#attach ~left:0 ~top:0 ~right:4 ~bottom:4 ~fill:`BOTH pokeanim1#coerce;
  battle#attach ~left:0 ~top:0 ~right:4 ~bottom:4 ~fill:`BOTH pokeanim2#coerce;
  battle#attach ~left:0 ~top:3 ~right:2 ~bottom:4
                  ~fill:`NONE health_bar_holder1#coerce;
  battle#attach ~left:1 ~top:0 ~right:4
                  ~fill:`NONE health_bar_holder2#coerce;
  battle#attach ~left:0 ~top:0 ~right:4 ~bottom:4 ~fill:`BOTH bg_img#coerce;
  (* Return all the objects created *)
  battle, text, bg_img, move1, move2, move3, move4, switch, poke1_img,
  poke2_img, move_img, text_buffer, poke1, poke2, poke3, poke4, poke5,
  (health_bar_holder1, health_bar_holder2, health_bar1, health_bar2), pokeanim1,
  pokeanim2, moveanim

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
  (* button4 is known as random outside of this function *)
	let button4 = GButton.button ~label:"Random Battle"
		~packing:(hbox1#pack ~expand:true ~fill:true) ~show:false () in
  (* button5 is known as preset outside of this function *)
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
  (* load_screen is a gif that plays before battle (during initialization)*)
  let load_screen = GMisc.image ~file:"../data/backgrounds/background.gif"
    ~show:false ~packing:(vbox#pack) () in
  (* put in logo but hide it *)
  hbox2#pack selectimg#coerce;
  (* Return all objects created *)
  (vbox, hbox1, hbox2, button1, button2, button3, button4,
		button5, button6, button7, img, load_screen)

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
let load_battle_screen engine img bg_img battle text buttonhide buttonshow
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
  getRandomBg (); bg_img#set_file !bg_string;
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
let load_battle_load engine img bg_img load_screen battle text buttonhide buttonshow
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
          load_battle_screen engine img bg_img battle text buttonhide buttonshow
          (battle_status, gui_ready) poke1_img poke2_img
          text_buffer health_holders ())
      | _ -> load_helper ()) in load_helper())
  else
    ()

let load_random  engine img bg_img load_screen battle text buttonhide buttonshow
  (battle_status, gui_ready, ready, ready_gui) main_menu battle_screen
  poke1_img poke2_img text_buffer health_holders () =
  match !current_screen with
  | Menu1P ->  load_battle_load engine img bg_img load_screen battle text buttonhide buttonshow
              (battle_status, gui_ready, ready, ready_gui) Random1p main_menu battle_screen
              poke1_img poke2_img text_buffer health_holders ()
  | Menu2P -> load_battle_load engine img bg_img load_screen battle text buttonhide buttonshow
              (battle_status, gui_ready, ready, ready_gui) Random2p main_menu battle_screen
              poke1_img poke2_img text_buffer health_holders ()
  | _ -> failwith "Faulty Game Logic: Debug 298"

let load_preset engine img bg_img load_screen battle text buttonhide preset buttonshow
  (battle_status, gui_ready, ready, ready_gui) main_menu (battle_screen : GPack.box)
  poke1_img poke2_img text_buffer health_holders () =
  (match !current_screen with
    | Menu1P -> current_screen := Preset1PChoose;
                List.iter (fun s -> s#misc#hide ()) buttonhide;
                preset#set_label "Continue";
                img#misc#hide ();
                selecttext := GMisc.label ~text:"Choose your 6 Pokemon from the drop down menus." ~packing:(battle_screen#pack) ();
                select1 := GEdit.combo ~popdown_strings:(Pokemon.unlocked_poke_string_list ()) ~case_sensitive:false ~allow_empty:false ~packing:(battle_screen#pack) ();
                select2 := GEdit.combo ~popdown_strings:(Pokemon.unlocked_poke_string_list ()) ~case_sensitive:false ~allow_empty:false ~packing:(battle_screen#pack) ();
                select3 := GEdit.combo ~popdown_strings:(Pokemon.unlocked_poke_string_list ()) ~case_sensitive:false ~allow_empty:false ~packing:(battle_screen#pack) ();
                select4 := GEdit.combo ~popdown_strings:(Pokemon.unlocked_poke_string_list ()) ~case_sensitive:false ~allow_empty:false ~packing:(battle_screen#pack) ();
                select5 := GEdit.combo ~popdown_strings:(Pokemon.unlocked_poke_string_list ()) ~case_sensitive:false ~allow_empty:false ~packing:(battle_screen#pack) ();
                select6 := GEdit.combo ~popdown_strings:(Pokemon.unlocked_poke_string_list ()) ~case_sensitive:false ~allow_empty:false ~packing:(battle_screen#pack) ();
                selectimg#misc#show ();
                ()
    | Menu2P -> ()
    | Preset1PChoose -> try (
                          let poke1 = Pokemon.getPresetPokemon (!select1#entry#text) in
                          let poke2 = Pokemon.getPresetPokemon (!select2#entry#text) in
                          let poke3 = Pokemon.getPresetPokemon (!select3#entry#text) in
                          let poke4 = Pokemon.getPresetPokemon (!select4#entry#text) in
                          let poke5 = Pokemon.getPresetPokemon (!select5#entry#text) in
                          let poke6 = Pokemon.getPresetPokemon (!select6#entry#text) in
                          selectimg#misc#hide (); !selecttext#destroy (); preset#misc#hide ();
                          !select1#destroy (); !select2#destroy (); !select3#destroy ();
                          !select4#destroy (); !select5#destroy (); !select6#destroy ();
                          preset#set_label "Preset Battle";
                          load_battle_load engine img bg_img load_screen battle text buttonhide buttonshow
                          (battle_status, gui_ready, ready, ready_gui) (Preset1p [poke1;poke2;poke3;poke4;poke5;poke6]) main_menu battle_screen
                          poke1_img poke2_img text_buffer health_holders ()
                      ) with _ -> let error_win = GWindow.message_dialog ~message:"Error in your Pokemon selection. Try making sure everything is spelled correctly."
                                  ~buttons:GWindow.Buttons.close  ~message_type:`ERROR () in ignore(error_win#connect#close ~callback:(error_win#destroy));
                                  ignore (error_win#connect#response ~callback:(fun s -> error_win#destroy ())); error_win#show ()
    | _ -> failwith "Faulty Game Logic: Debug 314"
  )

let load_menu engine button_show button_hide img screen () =
	if Ivar.is_empty (!engine) then
		(List.iter (fun s -> s#misc#hide ()) button_hide;
    List.iter (fun s -> s#misc#show ()) button_show;
    current_screen := screen; (match screen with
    | Menu1P -> img#set_file "./gui_pics/1p.jpg"
    | Menu2P -> img#set_file "./gui_pics/2p.jpg"
    | Menu0P -> img#set_file "./gui_pics/nop.jpg"
    | MainMenu -> img#set_file "./gui_pics/main.gif"
    | _ -> failwith "Faulty Game Logic: Debug 307"); Ivar.fill !engine screen)
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
  battle#misc#hide (); text#misc#hide ();
  main_menu_bg#set_file "./gui_pics/main.gif";
  main_menu_bg#misc#show ();
  current_screen := MainMenu; Ivar.fill !engine MainMenu; Ivar.fill !ready true;
  battle_status := Ivar.create (); current_command := (None, None);
  gui_ready := Ivar.create ())
else
  ()

let go_back engine (menu_holder, main_menu, battle_scren, one_player,
    two_player, no_player, random, preset, touranment,
    back_button, main_menu_bg, load_screen) (battle, text, bg_img, move1, move2,
    move3, move4, switch, poke1_img, poke2_img, move_img, text_buffer, poke1, poke2,
    poke3, poke4, poke5, health_holders, pokeanim1, pokeanim2, moveanim) battle_engine () =
	(if !current_screen = Menu1P || !current_screen = Menu2P || !current_screen = Menu0P then
		load_menu engine [one_player;two_player;no_player]
    [random; preset ;touranment; back_button] main_menu_bg
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
    (let battle_status, gui_ready, ready, ready_gui = battle_engine in
    Printf.printf "DID I FILL IT %B\n%!" (Ivar.is_full !gui_ready));
  if (!current_screen = Preset1PChoose) then
    (preset#set_label "Preset Battle";
    selectimg#misc#hide (); main_menu_bg#misc#show (); !selecttext#destroy ();
    !select1#destroy (); !select2#destroy (); !select3#destroy ();
    !select4#destroy (); !select5#destroy (); !select6#destroy ();
    load_menu engine [random; back_button] [] main_menu_bg Menu1P ());
  ()

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

let rec getMoveString a =
  match a with
  | NormMove s -> DontMiss s
  | Crit s -> getMoveString s
  | SEff s -> getMoveString s
  | NoEff s -> getMoveString s
  | NoEffAll s -> DontMiss s
  | StatBoostA (stat, i, s) -> getMoveString s
  | StatAttackA (stat, i, s) -> getMoveString s
  | HitMult (n, s) -> getMoveString s
  | BurnMove s -> getMoveString s
  | FreezeMove s -> getMoveString s
  | ParaMove s -> getMoveString s
  | MissMove s ->  Miss s
  | Asleep -> SleepMiss
  | Wake s -> getMoveString s
  | FrozenSolid -> FrozenMiss
  | Thaw s-> getMoveString s
  | NoFreeze s -> getMoveString s
  | NoBurn s -> getMoveString s
  | NoPara s -> getMoveString s
  | Para -> ParaMiss
  | OHKill s -> getMoveString s
  | FlinchA -> FlinchMiss
  | PoisonMove s -> getMoveString s
  | Recoil s -> getMoveString s
  | BreakConfuse s -> getMoveString s
  | Confused -> ConfuseMiss
  | DrainA s-> getMoveString s
  | ConfuseMoveA s -> getMoveString s
  | UserFaintA s -> getMoveString s
  | DrainSleepFail s -> DontMove
  | BreakSub s -> getMoveString s
  | SubDmg s -> getMoveString s
  | ProtectedA s -> DontMiss s
  | ChargingMove (s, n) -> DontMove
  | SwitchOutA s -> getMoveString s
  | Recharging s -> getMoveString s
  | FalseSwipeA s -> getMoveString s

let rec getAttackString starter a =
  match a with
  | NormMove s -> starter ^ " used " ^ s ^"."
  | Crit s -> getAttackString starter s ^ "It was a critical hit."
  | SEff s -> getAttackString starter s ^ "It was super effective."
  | NoEff s -> getAttackString starter s ^ "It was not very effective."
  | NoEffAll s -> starter ^ " used " ^ s ^ " but it had no effect."
  | StatBoostA (stat, i, s) -> getAttackString starter s ^ string_from_stat stat ^ " was boosted " ^ string_of_int i ^ " stage."
  | StatAttackA (stat, i, s) -> getAttackString starter s ^ "Opponent's " ^ string_from_stat stat ^ " was lowered " ^ string_of_int (-1 * i) ^ " stage."
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
  | BreakSub s -> getAttackString starter s ^ starter ^ " has broken the opponent's substitute."
  | SubDmg s -> getAttackString starter s ^ "The opponent's substitute took the damage instead."
  | ProtectedA s -> starter ^ " used " ^ s ^ " but opponent protected itself."
  | FalseSwipeA s -> getAttackString starter s ^ "The opponent cannot go below 1 HP."
  | ChargingMove (s, n) -> starter ^ " is charging up." ^ s
  | SwitchOutA s -> (match !secondaryEffect with
                    | `P1 -> current_command := (Some NoMove, Some (Poke "random"))
                    | `P2 -> current_command := (Some (Poke "random"), Some NoMove));
                    endTurnEarly := true; getAttackString starter s ^ "The opponent was forced out!"
  | Recharging s -> (match !secondaryEffect with
                        | `P1 -> current_command := (Some (NoMove), snd !current_command)
                        | `P2 -> current_command := (fst !current_command, Some NoMove)); getAttackString starter s ^ starter ^ " will need a turn to recharge."

(* starter is either 'Player One' or 'Player Two'*)
let rec getStatusString starter s =
  match s with
  | NormStatus s -> starter ^ " used " ^ s ^ "."
  | StatBoost (stat, i, s) -> getStatusString starter s ^ string_from_stat stat ^ " was boosted " ^ string_of_int i ^ " stage."
  | StatAttack (stat, i, s) -> getStatusString starter s ^ "Opponent's " ^ string_from_stat stat ^ " was lowered " ^ string_of_int (-1 * i) ^ " stage."
  | MissStatus s -> starter ^ " used " ^ s ^ " but it missed."
  | FrozenSolidS -> starter ^  " was frozen solid."
  | PoisonStatus s-> getStatusString starter s ^ "The opponent has been poisoned."
  | BurnStatus s -> getStatusString starter s ^ "The opponent has been burned."
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
  | SubBlock s -> getStatusString starter s ^ "The move was blocked by the opponent's substitute."
  | SubFail s -> getStatusString starter s ^ "The substitute failed."
  | SubMake s -> getStatusString starter s ^ starter ^ " has created a substitute."
  | ProtectedS s -> starter ^ " used " ^ s ^ " but opponent protected itself."
  | ProtectS s-> getStatusString starter s ^ starter ^ " has protected itself."
  | ProtectFail s-> getStatusString starter s ^ starter ^ " has failed to protect itself."
  | SpikesS s -> getStatusString starter s ^ starter ^ " has depositied a layer of spikes."
  | HealBellS s -> getStatusString starter s ^ starter ^ " has healed itself and its allies of status ailments."
  | RefreshS s -> getStatusString starter s ^ starter ^ " has healed itself of any burns, poisons, paralysis."
  | Fail s -> starter ^ " used " ^ s ^ " but it failed."
  | PsychUpS s -> getStatusString starter s ^ starter ^ " has copied opponent's status changes."
  | SunnyDayS s -> getStatusString starter s ^ starter ^ " has called out the sun."
  | RainDanceS s -> getStatusString starter s ^ starter ^ " has called out the rain."
  | SandStormS s -> getStatusString starter s ^ starter ^ " has created a large sandstorm."
  | HailS s -> getStatusString starter s ^ starter ^ " has made large pellets of hail fall down."
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
  | SunFade s-> getEndString starter s ^ "The sunlight has faded."
  | RainFade s -> getEndString starter s ^ "The rain has faded."
  | SandStormFade s -> getEndString starter s ^ "The sandstorm has faded."
  | SandBuffetB s -> getEndString starter s ^ "Both Pokemon got buffeted by the sandstorm."
  | SandBuffet1 s -> getEndString starter s ^ "Player one got buffeted by the sandstorm."
  | SandBuffet2 s -> getEndString starter s ^ "Player two got buffeted by the sandstorm"
  | HailFade s -> getEndString starter s ^ "The hail has faded."
  | HailBuffetB s -> getEndString starter s ^ "Both Pokemon get hit by the hail."
  | HailBuffet1 s -> getEndString starter s ^ "Player one gets hit by the hail."
  | HailBuffet2 s -> getEndString starter s ^ "Player two gets hit by the hail."

let animate_attack (animbox : GPack.fixed) img startx starty nextx' nexty (moveanim : GPack.fixed) move_img attack=
  let movestring = getMoveString attack in
  match movestring with
  | SleepMiss -> ()
  | FrozenMiss -> ()
  | ParaMiss -> ()
  | FlinchMiss -> ()
  | ConfuseMiss -> ()
  | DontMove -> ()
  | DontMiss s | Miss s ->
    (let themove = Pokemon.getMoveFromString s in
     let nextx = (if movestring = DontMiss s then nextx' else screen_width / 2) in
      match themove.dmg_class with
      | Physical ->
        (let incx = (nextx - startx) / 80 in
        let incy = (nexty - starty) / 80 in
        for i = 0 to 80 do
          animbox#move img#coerce (startx + i * incx) (starty + i * incy);
          busywait_small ()
        done;
        move_img#set_file "../data/fx/leftclaw.png";
        moveanim#move move_img#coerce nextx nexty;
        move_img#misc#show ();
        for l = 1 to 10 do
          busywait_small ()
        done;
        move_img#misc#hide ();
        for i = 80 downto 1 do
          animbox#move img#coerce (startx + i * incx) (starty + i * incy);
          busywait_small ()
        done)
    | Special ->
      (move_img#set_file ("../data/fx/" ^ (String.lowercase (Pokemon.string_of_element (Pokemon.getMoveFromString s).element)) ^ "wisp.png");
      moveanim#move move_img#coerce startx starty;
      move_img#misc#show ();
      let incx = (nextx - startx) / 80 in
      let incy = (nexty - starty) / 80 in
      for i = 0 to 80 do
        moveanim#move move_img#coerce (startx + i * incx) (starty + i * incy);
        busywait_small ()
      done;
      move_img#misc#hide ())
    | Status -> failwith "Faulty Game Logic: Debug 512")

let update_buttons engine move1 move2 move3 move4 =
  let team = (match get_game_status engine with
  | Battle (InGame (t1, t2, _, _, _)) ->
      (match !current_screen with
      | Battle (P1 ChooseMove) -> t1
      | Battle (P2 ChooseMove) -> t2
      | _ -> failwith "Faulty Game logic")
  | _ -> failwith "Fauly game Logic") in
  move1#set_label team.current.pokeinfo.move1.name;
  move2#set_label team.current.pokeinfo.move2.name;
  move3#set_label team.current.pokeinfo.move3.name;
  move4#set_label team.current.pokeinfo.move4.name;
  move1#misc#set_tooltip_text (Pokemon.getMoveToolTip team.current.pokeinfo.move1);
  move2#misc#set_tooltip_text (Pokemon.getMoveToolTip team.current.pokeinfo.move2);
  move3#misc#set_tooltip_text (Pokemon.getMoveToolTip team.current.pokeinfo.move3);
  move4#misc#set_tooltip_text (Pokemon.getMoveToolTip team.current.pokeinfo.move4)

let getWeatherString w =
  match w with
  | Sun _ -> "../data/fx/weather-sun.png"
  | Rain _ -> "../data/fx/weather-rain.png"
  | Hail _ -> "../data/fx/weather-hail.png"
  | SandStorm _ -> "../data/fx/weather-sandstorm.png"
  | _ -> !bg_string

let rec findForcedMove lst =
  match lst with
  | (ForcedMoveNoSwitch (_,s))::_ -> (true, s)
  | h::t -> findForcedMove t
  | [] -> (false, "")

let rec game_animation engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] (battle: GPack.table) text
  (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer (health_bar_holder1, health_bar_holder2, health_bar1, health_bar2)  pokeanim1 pokeanim2 moveanim back_button () =
  Printf.printf "DEBUG %B %B\n%!" (Ivar.is_empty !gui_ready) !endTurnEarly;
  let battle_buttons = [move1; move2; move3; move4; switch; back_button] in
  List.iter (fun s -> s#misc#hide ()) battle_buttons;
  let t1, t2, w, m1, m2 = match get_game_status engine with
    | Battle InGame (t1, t2, w, m1, m2) -> t1, t2, w, m1, m2
    | _ -> failwith "Fauly Game Logic: Debug 05" in
  let game_step () =
    bg_img#set_file (getWeatherString w.weather); continue := false;
    upon (Ivar.read !ready_gui) (fun _ -> ready_gui := Ivar.create ();
    game_animation engine [move1; move2; move3; move4; poke1; poke2; poke3;
    poke4; poke5; switch] battle text (battle_status, gui_ready, ready,
    ready_gui) bg_img poke1_img poke2_img move_img text_buffer (health_bar_holder1,
    health_bar_holder2, health_bar1, health_bar2) pokeanim1 pokeanim2 moveanim back_button ()) in
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
  let update_current_command () =
    let charging1, s1 = findForcedMove (snd t1.current.curr_status) in
    let charging2, s2 = findForcedMove (snd t2.current.curr_status) in
    let recharging1 = List.mem RechargingStatus (snd t1.current.curr_status) in
    let recharging2 = List.mem RechargingStatus (snd t2.current.curr_status) in
    (if (charging1) then
      (current_command := (Some (UseAttack s1), snd !current_command))
    else if (charging2) then
      (current_command := (fst !current_command, Some (UseAttack s2)))
    else if recharging1 then
      (current_command := (Some NoMove, snd !current_command))
    else if recharging2 then
      (current_command := (fst !current_command, Some NoMove))
    else
      ());
    (t1.current.curr_status <- (fst t1.current.curr_status, List.filter (fun s -> s <> RechargingStatus) (snd t1.current.curr_status)));
    (t2.current.curr_status <- (fst t2.current.curr_status, List.filter (fun s -> s <> RechargingStatus) (snd t2.current.curr_status))) in
  let skipturn () =
    update_current_command ();
    match get_game_status battle_status with
    | Random1p | Preset1p _ -> (match !current_command with
                  | (None, _) -> text_buffer#set_text (Pokemon.string_of_weather w.weather); List.iter (fun s -> s#misc#show ()) battle_buttons; current_screen := Battle (P1 ChooseMove); update_buttons engine move1 move2 move3 move4
                  | _ -> Ivar.fill !gui_ready !current_command; current_command := (None, None); game_step ())
    | Random2p -> (match !current_command with
                  | (None, _) -> text_buffer#set_text (Pokemon.string_of_weather w.weather); List.iter (fun s -> s#misc#show ()) battle_buttons; current_screen := Battle (P1 ChooseMove); update_buttons engine move1 move2 move3 move4
                  | (_, None) -> text_buffer#set_text (Pokemon.string_of_weather w.weather); List.iter (fun s -> s#misc#show ()) battle_buttons; current_screen := Battle (P2 ChooseMove); update_buttons engine move1 move2 move3 move4
                  | _ -> Ivar.fill !gui_ready !current_command; current_command := (None, None); game_step ())
    | _ -> failwith "Faulty Game Logic: Debug 100" in
  let simple_move () =
    updatetools ();
    skipturn () in
  let turn_end () =
    (Ivar.fill !gui_ready (Some TurnEnd, Some TurnEnd); game_step ()) in
  let pre_process () =
    if !endTurnEarly then
      (endTurnEarly := false; turn_end ())
    else
      (Ivar.fill !gui_ready (Some Preprocess, Some Preprocess);
      game_step ()) in
  (match !m1 with
  | Pl1 SPoke p -> text_buffer#set_text ("Player One has switched to " ^ p); poke1_img#set_file ("../data/back-sprites/" ^ p ^ ".gif");updatetools ();busywait ()
  | Pl2 SPoke p -> text_buffer#set_text ("Player Two has switched to " ^ p); poke2_img#set_file ("../data/sprites/" ^ p ^ ".gif"); updatetools (); busywait ()
  | Pl1 AttackMove a -> secondaryEffect := `P1;
                   let atk_string = getAttackString t1.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\.") atk_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim1 poke1_img poke1x poke1y poke2x poke2y moveanim move_img a;
                   updatehealth2 ()
  | Pl2 AttackMove a -> secondaryEffect := `P2;
                   let atk_string = getAttackString t2.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\.") atk_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim2 poke2_img poke2x poke2y poke1x poke1y moveanim move_img a;
                   updatehealth1 ()
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
  | Pl1 SFaint -> if List.length t1.dead = 5 then ((if !m2 = (Pl2 Faint) && List.length t2.dead = 5 then (text_buffer#set_text "Tie game!") else (text_buffer#set_text "Player Two wins!")); current_screen := Battle (P1 ChooseMove); back_button#misc#show ()) else
                  (poke1_img#set_file ("../data/back-sprites/" ^ t1.current.pokeinfo.name ^ ".gif");
                  text_buffer#set_text (t1.current.pokeinfo.name ^ " has fainted. Choosing a new Pokemon.");
                  (match !m2 with
                  | Pl2 Faint -> current_screen := Battle (P1 BothFaint)
                  | _ -> current_screen := Battle (P1 Faint));
                  busywait (); updatetools ();
                  switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                  move3;move4;switch] back_button ())
  | Pl1 Faint ->  if List.length t1.dead = 5 then (text_buffer#set_text "Player Two wins!"; current_screen := Battle (P1 ChooseMove); back_button#misc#show ()) else
                  (text_buffer#set_text "Player One Pokemon has fainted. Choosing a new Pokemon.";
                  (match !m2 with
                  | Pl2 Faint -> current_screen := Battle (P1 BothFaint)
                  | _ -> current_screen := Battle (P1 Faint));
                  busywait (); updatetools ();
                  switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                  move3;move4;switch] back_button ())
  | Pl2 Faint -> if List.length t2.dead = 5 then (text_buffer#set_text "Player One wins!"; current_screen := Battle (P1 ChooseMove); back_button#misc#show ()) else
                  (text_buffer#set_text "Player Two Pokemon has fainted. Choosing a new Pokemon.";
                 (match get_game_status battle_status with
                 | Random1p | Preset1p _ -> busywait (); current_command := ((if fst !current_command = None then Some (NoMove) else fst !current_command), Some (FaintPoke ""));
                               simple_move()
                 | Random2p -> current_screen := Battle (P2 Faint); busywait (); updatetools ();
                              current_command := (Some NoMove, snd !current_command);
                              switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                              move3;move4;switch] back_button ()
                 | _ -> failwith "Faulty Game Logic: Debug 007"))
  | Pl2 SFaint ->if List.length t2.dead = 5 then (text_buffer#set_text "Player One wins!"; current_screen := Battle (P1 ChooseMove); back_button#misc#show ()) else
                 (poke2_img#set_file ("../data/sprites/" ^ t2.current.pokeinfo.name ^ ".gif");
                 text_buffer#set_text (t2.current.pokeinfo.name ^ " has fainted. Choosing a new Pokemon.");
                 (match get_game_status battle_status with
                 | Random1p | Preset1p _ -> busywait (); current_command := (Some (NoMove), Some (FaintPoke ""));
                               simple_move()
                 | Random2p -> current_screen := Battle (P2 Faint); busywait (); updatetools ();
                                current_command := (Some (NoMove), snd !current_command);
                                switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                                move3;move4;switch] back_button ()
                 | _ -> failwith "Faulty Game Logic: Debug 007"))
  | Pl1 EndMove x -> let txt = getEndString t1.current.pokeinfo.name x in
                    let str_list = Str.split (Str.regexp "\.") txt in
                    List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                    updatehealth1 ()
  | _ -> failwith "unimplements");
  if !endTurnEarly then
    (endTurnEarly := false; turn_end ())
  else
  (match !m2 with
  | Pl1 AttackMove a -> secondaryEffect := `P1;
                   let atk_string = getAttackString t1.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\.") atk_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim1 poke1_img poke1x poke1y poke2x poke2y moveanim move_img a;
                   updatetools ();
                   pre_process ()
  | Pl2 AttackMove a -> secondaryEffect := `P2;
                   let atk_string = getAttackString t2.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\.") atk_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim2 poke2_img poke2x poke2y poke1x poke1y moveanim move_img a;
                   updatetools ();
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
  (* | Pl1 Faint -> text_buffer#set_text "Player One Pokemon has fainted. Choosing a new Pokemon.";
                  (match get_game_status battle_status with
                  | Random1p | Preset1p _ -> busywait (); updatetools (); current_screen := Battle (P1 Faint);
                                switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                                move3;move4;switch] back_button ()
                  | _ -> failwith "Faulty Game Logic: Debug 008"
                  ) *)
  | Pl1 Continue -> turn_end ()
  | Pl2 Continue -> turn_end ()
  | Pl1 Next | Pl2 Next -> simple_move ()
  | Pl1 FaintNext | Pl2 FaintNext | Pl2 Faint -> ()
  | Pl1 NoAction -> pre_process ()
  | Pl2 NoAction -> pre_process ()
  | Pl2 SPoke p -> text_buffer#set_text ("Player Two has switched to " ^ p); poke2_img#set_file ("../data/sprites/" ^ p ^ ".gif"); busywait (); updatetools (); pre_process ()
  | Pl2 EndMove x -> let txt = getEndString t2.current.pokeinfo.name x in
                    let str_list = Str.split (Str.regexp "\.") txt in
                    List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                    updatehealth2 (); turn_end ()
  | _ -> failwith "unimplement")

 let process_command engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text
  (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button =
 let battle_buttons = [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch]  in
 match !current_command with
 | (None, None) -> failwith "Faulty Game Logic: Debug 03"
 | (Some _, None) -> (match get_game_status battle_status with
                      | Random1p | Preset1p _-> List.iter (fun s -> s#misc#hide ()) battle_buttons; current_screen := Battle Processing; text_buffer#set_text "Both moves collected. Processing...";
                                          Ivar.fill !gui_ready !current_command; current_command := (None, None);
                                          upon (Ivar.read !ready_gui) (fun _ -> ready_gui := Ivar.create (); game_animation engine battle_buttons battle text
                                                                        (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button ())
                      | Random2p -> List.iter (fun s -> s#misc#hide ()) battle_buttons; current_screen := Battle (P2 ChooseMove); update_buttons engine move1 move2 move3 move4;
                                    List.iter (fun s -> s#misc#show ()) [move1;move2;move3;move4;switch;back_button]; text_buffer#set_text "Player Two's Chance to Choose a Move.";
                      | _ -> failwith "Faulty Game Logic: Debug 01")
 | (Some _, Some _) -> current_screen := Battle Processing; List.iter (fun s -> s#misc#hide ()) battle_buttons; current_screen := Battle Processing; text_buffer#set_text "Both moves collected. Processing...";
                                          Ivar.fill !gui_ready !current_command; current_command := (None, None);
                                          upon (Ivar.read !ready_gui) (fun _ -> ready_gui := Ivar.create (); game_animation engine battle_buttons battle text
                                                                        (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button ())

let poke_move_cmd button engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text
  (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button () =
  Printf.printf "Using a move!\n%!";
  let _ = match !current_command with
 | None, None -> current_command := (Some (UseAttack (button#label)), None)
 | None, Some x -> current_command := (Some (UseAttack (button#label)), Some x)
 | Some x, None -> current_command := Some x, Some (UseAttack button#label)
 | Some _, Some _ -> failwith "Faulty Game Logic: Debug 06" in
 process_command engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text
  (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button

 let switch_poke_cmd button engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text
  (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button () =
 Printf.printf "Switching Pokemon\n%!";
 let next () =
  process_command engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text
  (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button in
 match !current_screen with
  | Battle (P1 BothFaint) ->
        (match get_game_status battle_status with
        | Random1p | Preset1p _ -> current_command := (Some (FaintPoke (button#label)), Some (FaintPoke "")); next ()
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
		two_player, no_player, random, preset, touranment,
		back_button, main_menu_bg, load_screen = menu in
  let battler = make_battle_screen ~packing:(battle_screen#add) ()
  in let battle, text, bg_img, move1, move2, move3, move4, switch,
    poke1_img, poke2_img, move_img, text_buffer, poke1, poke2, poke3, poke4,
    poke5, health_holders, pokeanim1, pokeanim2, moveanim = battler in
  main_menu#pack move1#coerce; main_menu#pack move2#coerce;
  main_menu#pack move3#coerce; main_menu#pack move4#coerce;
  main_menu#pack switch#coerce; main_menu#pack poke1#coerce;
  main_menu#pack poke2#coerce; main_menu#pack poke3#coerce;
  main_menu#pack poke4#coerce; main_menu#pack poke5#coerce;
  (* One player Button *)
	one_player#connect#clicked ~callback:(load_menu engine [random;preset;
  touranment;back_button] [one_player; two_player;no_player] main_menu_bg
  Menu1P);
  (* Two player button *)
  two_player#connect#clicked ~callback:(load_menu engine [random;preset;
  back_button] [one_player; two_player; no_player] main_menu_bg Menu2P);
  (* No player button *)
  no_player#connect#clicked ~callback:(load_menu engine [random;preset;
  back_button] [one_player; two_player; no_player] main_menu_bg Menu0P);
 (* Back button *)
   back_button#connect#clicked
  ~callback:(go_back engine menu battler battle_engine);
  (* Random battle button *)
  random#connect#clicked ~callback:(load_random engine main_menu_bg bg_img load_screen
  battle text [random;preset;touranment] [move1; move2; move3; move4;
  switch] battle_engine main_menu battle_screen poke1_img poke2_img
  text_buffer health_holders);
  (* Preset battle button *)
  preset#connect#clicked ~callback:(load_preset engine main_menu_bg bg_img load_screen
  battle text [random;touranment] preset [move1; move2; move3; move4;
  switch] battle_engine main_menu battle_screen poke1_img poke2_img
  text_buffer health_holders);
  (* Switch button *)
  switch#connect#clicked ~callback:(switch_poke engine [poke1;poke2;poke3;
  poke4;poke5] [move1;move2;move3;move4;switch] back_button);
  (* Pokemon buttons *)
  poke1#connect#clicked ~callback:(switch_poke_cmd poke1 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button);
  poke2#connect#clicked ~callback:(switch_poke_cmd poke2 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button);
  poke3#connect#clicked ~callback:(switch_poke_cmd poke3 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button);
  poke4#connect#clicked ~callback:(switch_poke_cmd poke4 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button);
  poke5#connect#clicked ~callback:(switch_poke_cmd poke5 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button);
  (* Move buttons *)
  move1#connect#clicked ~callback:(poke_move_cmd move1 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button);
  move2#connect#clicked ~callback:(poke_move_cmd move2 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button);
  move3#connect#clicked ~callback:(poke_move_cmd move3 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button);
  move4#connect#clicked ~callback:(poke_move_cmd move4 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button);
  window#connect#destroy ~callback:(quit engine ready);
  window#event#connect#key_press ~callback:(space_press);
  window#show ();
	let thread = GtkThread.start () in ()