open Async.Std
open Info
open Tournament

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

let busywait_player = let ctr = ref 0 in fun () -> ctr := 0;
  for i = 1 to 200_000 do
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
  busywait_player ();
  continue := false

(* Initializes GtkMain*)
let locale = GtkMain.Main.init ()

(* randomize battle background *)
let bg_string = ref ("../data/backgrounds/bg-volcanocave.png")

let rec test_string n () =
  match n with
  | 0 -> []
  | n -> (string_of_int n)::test_string (n-1) ()

(* Global references to Poke-edit for fast and memory low destruction*)
let pokedit_screen = GPack.hbox ()
let pokedit_labels = GPack.vbox ~packing:(pokedit_screen#pack)()
let pokedit_combos = GPack.vbox ~packing:(pokedit_screen#pack)()
let pokedit_labels2 = GPack.vbox ~packing:(pokedit_screen#pack)()
let pokedit_scale = GPack.vbox ~packing:(pokedit_screen#pack) ~width:170 ()
let label1 = GMisc.label ~text:"Move 1" ~height:16 ~ypad:5 ~xpad:15 ~packing:(pokedit_labels#pack ~expand:true) ()
let label2 = GMisc.label ~text:"Move 2" ~height:16 ~ypad:5 ~xpad:15 ~packing:(pokedit_labels#pack ~expand:true) ()
let label3 = GMisc.label ~text:"Move 3" ~height:16 ~ypad:5 ~xpad:15 ~packing:(pokedit_labels#pack ~expand:true) ()
let label4 = GMisc.label ~text:"Move 4" ~height:16 ~ypad:5 ~xpad:15 ~packing:(pokedit_labels#pack ~expand:true) ()
let label5 = GMisc.label ~text:"Ability" ~height:16 ~ypad:5 ~xpad:15 ~packing:(pokedit_labels#pack ~expand:true) ()
let label6 = GMisc.label ~text:"Nature" ~height:16 ~ypad:5 ~xpad:15 ~packing:(pokedit_labels#pack ~expand:true) ()
let label7 = GMisc.label ~text:"Item" ~height:16 ~ypad:5 ~xpad:15 ~packing:(pokedit_labels#pack ~expand:true) ()
let label8 = GMisc.label ~text:"HP EVs" ~height:16 ~ypad:8 ~xpad:15 ~packing:(pokedit_labels2#pack ~expand:true) ()
let label9 = GMisc.label ~text:"Attack EVs" ~height:16 ~ypad:8 ~xpad:15 ~packing:(pokedit_labels2#pack ~expand:true) ()
let label10 = GMisc.label ~text:"Defense EVs" ~height:16 ~ypad:8 ~xpad:15 ~packing:(pokedit_labels2#pack ~expand:true) ()
let label11 = GMisc.label ~text:"Special Attack EVs" ~height:16 ~ypad:8 ~xpad:15 ~packing:(pokedit_labels2#pack ~expand:true) ()
let label12 = GMisc.label ~text:"Special Defense EVs" ~height:16 ~ypad:8 ~xpad:15 ~packing:(pokedit_labels2#pack ~expand:true) ()
let label13 = GMisc.label ~text:"Speed EVs" ~height:16 ~ypad:8 ~xpad:15 ~packing:(pokedit_labels2#pack ~expand:true) ()
let hp_evs = GRange.scale `HORIZONTAL ~digits:0 ~adjustment:(GData.adjustment ~upper:252. ~page_size:0. ()) ~draw_value:true ~packing:(pokedit_scale#pack ~expand:true) ()
let atk_evs = GRange.scale `HORIZONTAL ~digits:0 ~adjustment:(GData.adjustment ~upper:252. ~page_size:0. ()) ~draw_value:true ~packing:(pokedit_scale#pack ~expand:true) ()
let def_evs = GRange.scale `HORIZONTAL ~digits:0 ~adjustment:(GData.adjustment ~upper:252. ~page_size:0. ()) ~draw_value:true ~packing:(pokedit_scale#pack ~expand:true) ()
let special_attack_evs = GRange.scale `HORIZONTAL ~digits:0 ~adjustment:(GData.adjustment ~upper:252. ~page_size:0. ()) ~draw_value:true ~packing:(pokedit_scale#pack ~expand:true) ()
let special_defense_evs = GRange.scale `HORIZONTAL ~digits:0 ~adjustment:(GData.adjustment ~upper:252. ~page_size:0. ()) ~draw_value:true ~packing:(pokedit_scale#pack ~expand:true) ()
let speed_evs = GRange.scale `HORIZONTAL ~digits:0 ~adjustment:(GData.adjustment ~upper:252. ~page_size:0. ()) ~draw_value:true ~packing:(pokedit_scale#pack ~expand:true) ()

(* Global references to an object for easy destruction *)
let selecttext = ref (GMisc.label ())
let select1 = ref (GEdit.combo ~popdown_strings:(test_string 750 ()) ())
let select2 = ref (GEdit.combo ~popdown_strings:(test_string 750 ()) ())
let select3 = ref (GEdit.combo ~popdown_strings:(test_string 750 ()) ())
let select4 = ref (GEdit.combo ~popdown_strings:(test_string 750 ()) ())
let select5 = ref (GEdit.combo ~popdown_strings:(test_string 750 ()) ())
let select6 = ref (GEdit.combo ~popdown_strings:(test_string 750 ()) ())
let select7 = ref (GEdit.combo ~popdown_strings:(test_string 750 ()) ())
let selectimg = GMisc.image ~file:"../data/backgrounds/PokemonLogo.png" ()
let editimg = GMisc.image ~file:"../data/backgrounds/pokeedit.jpg" ()
let () = !select1#destroy (); !select2#destroy (); !select3#destroy ();
          !select4#destroy (); !select5#destroy (); !select6#destroy ();
          !select7#destroy ()
(* Holds similar information to the engine, but acts differently in battle
  In battle, engine holds the current battle state, but current_screen holds
  the information on which player is selecting move/pokemon/etc...
*)
let current_screen = ref MainMenu

(* PUT THIS IN ANOTHER FILE LATER  *)
(* ---------------------------------------------------------------------------*)
let x = ref 7
let y = ref 5

let spriteanim = GPack.fixed ~width:screen_width ~height:(2 * screen_height/3) ()
let bossanim = GPack.fixed ~width:screen_width ~height:(2 * screen_height/3) ()
let opp1anim = GPack.fixed ~width:screen_width ~height:(2 * screen_height/3) ()
let opp2anim = GPack.fixed ~width:screen_width ~height:(2 * screen_height/3) ()
let tilemap = GMisc.image ~file:"../data/tournament/tilemap1.png" ()
(* 600 x 320 *)
let gameBoard = GPack.table ~rows:4 ~columns: 4 ~height: (2* screen_height/3) ~width:screen_width  ()
let sprite = GMisc.image ~file:"../data/tournament/Player/Down.png" ()
let gameText = GEdit.entry ~width:600 ~height:80
              ~text:"Use W,A,S,D to move. Press H to interact with Prof. Oak and space bar to talk." ~editable:false ()
let boss = GMisc.image ~file:"../data/tournament/NPC/ProfOak.png" ()
let opp1 = GMisc.image ~file:"../data/tournament/NPC/dragontamer.png" ()
let opp2 = GMisc.image ~file:"../data/tournament/NPC/psychic.png" ()
let () =  (spriteanim#put sprite#coerce (40 * !x) (40 * !y); bossanim#put boss#coerce 280 100;
           opp1anim#put opp1#coerce 160 20; opp2anim#put opp2#coerce 400 20;
           gameBoard#attach ~left:0 ~top:0 ~right:4 ~bottom:4 ~fill:`BOTH spriteanim#coerce;
           gameBoard#attach ~left:0 ~top:0 ~right:4 ~bottom:4 ~fill:`BOTH bossanim#coerce;
           gameBoard#attach ~left:0 ~top:0 ~right:4 ~bottom:4 ~fill:`BOTH opp1anim#coerce;
           gameBoard#attach ~left:0 ~top:0 ~right:4 ~bottom:4 ~fill:`BOTH opp2anim#coerce;
           gameBoard#attach ~left:0 ~top:0 ~right:4 ~bottom:4 ~fill:`BOTH tilemap#coerce)

type gameMovement = Up | Down | Left | Right | Interact

let move_ivar = ref (Ivar.create ())
let playerDirection = ref Down

let obstacle_coordinates = ref (getObstacleCoordinates ())

let rec move_up () =
  (if List.mem (!x, !y - 1) !obstacle_coordinates then
    ((match (!x, !y) with
    | (7,2) -> opp1#misc#hide (); opp2#misc#hide ();
               tilemap#set_file "../data/tournament/tilemap2.png";
               x := 7; y := 7; spriteanim#move sprite#coerce (40 * !x) (40 * !y);
               bossanim#move boss#coerce 280 20;
               boss#misc#show ();
               obstacle_coordinates := ice_obstacles;
    | _ -> () );(sprite#set_file "../data/tournament/Player/Up.png"))
  else
    (sprite#set_file "../data/tournament/Player/Up1.png";
    for i = 0 to 20 do
      spriteanim#move sprite#coerce (40 * !x) (40 * !y - i);
      busywait_player ()
    done;
    sprite#set_file "../data/tournament/Player/Up2.png";
    for i = 20 to 40 do
      spriteanim#move sprite#coerce (40 * !x) (40 * !y - i);
      busywait_player ()
    done;
    sprite#set_file "../data/tournament/Player/Up.png";
    y := !y - 1)); playerDirection := Up

let rec move_down () =
  (if List.mem (!x, !y + 1) !obstacle_coordinates then
    ((match (!x, !y) with
    | (7,7) -> opp1#misc#show (); opp2#misc#show ();
               tilemap#set_file "../data/tournament/tilemap1.png";
               x := 7; y := 2; spriteanim#move sprite#coerce (40 * !x) (40 * !y);
               bossanim#move boss#coerce 280 100;
               boss#misc#hide ();
               obstacle_coordinates := tilemap_nooak;
    | _ -> () );(sprite#set_file "../data/tournament/Player/Down.png"))
  else
    (sprite#set_file "../data/tournament/Player/Down1.png";
    for i = 0 to 20 do
      spriteanim#move sprite#coerce (40 * !x) (40 * !y + i);
      busywait_player ()
    done;
    sprite#set_file "../data/tournament/Player/Down2.png";
    for i = 20 to 40 do
      spriteanim#move sprite#coerce (40 * !x) (40 * !y + i);
      busywait_player ()
    done;
    sprite#set_file "../data/tournament/Player/Down.png";
    y := !y + 1)); playerDirection := Down

let rec move_right () =
  (if List.mem (!x + 1, !y) !obstacle_coordinates then
    (sprite#set_file "../data/tournament/Player/Right.png")
  else
    (sprite#set_file "../data/tournament/Player/Right1.png";
    for i = 0 to 20 do
      spriteanim#move sprite#coerce (40 * !x + i) (40 * !y);
      busywait_player ()
    done;
    sprite#set_file "../data/tournament/Player/Right2.png";
    for i = 20 to 40 do
      spriteanim#move sprite#coerce (40 * !x + i) (40 * !y);
      busywait_player ()
    done;
    sprite#set_file "../data/tournament/Player/Right.png";
    x := !x + 1)); playerDirection := Right

let rec move_left () =
  (if List.mem (!x - 1, !y) !obstacle_coordinates then
    (sprite#set_file "../data/tournament/Player/Left.png")
  else
    (sprite#set_file "../data/tournament/Player/Left1.png";
    for i = 0 to 20 do
      spriteanim#move sprite#coerce (40 * !x - i) (40 * !y);
      busywait_player ()
    done;
    sprite#set_file "../data/tournament/Player/Left2.png";
    for i = 20 to 40 do
      spriteanim#move sprite#coerce (40 * !x - i) (40 * !y);
      busywait_player ()
    done;
    sprite#set_file "../data/tournament/Player/Left.png";
    x := !x - 1)); playerDirection := Left

let talk tournament =
  match (!x, !y) with
  | (7,3) -> if !playerDirection = Up then (List.iter (fun s -> gameText#set_text s; busywait ()) profOakQuotes; gameText#set_text "Use W,A,S,D to move. Press H to interact with Prof. Oak and space bar to talk.") else ()
  | (7, 1) -> if !playerDirection = Up then (List.iter (fun s -> gameText#set_text s; busywait ()) (getProfOakQuotes ()); Save.incPlayOak (); tournament#clicked ()) else ()
  | (4,1) -> if !playerDirection = Up then (List.iter (fun s -> gameText#set_text s; busywait ()) (opp1Quotes ()); tournament#clicked ()) else ()
  | (10,1) -> if !playerDirection = Up then (List.iter (fun s -> gameText#set_text s; busywait ()) (opp2Quotes ()); tournament#clicked ()) else ()
  | _ -> ()

let rec move () =
  upon (Ivar.read !move_ivar) (fun (s,tournament) -> (match s with
  | Up -> move_up ()
  | Down -> move_down ()
  | Left -> move_left ()
  | Right -> move_right ()
  | Interact -> talk tournament); move_ivar := Ivar.create (); move ())

let handle_key_press tournament s =
  let key = GdkEvent.Key.keyval s in
  Printf.printf "Key value pressed: %d\n%!" key;
  match !current_screen with
  | Battle Loading -> false
  | Battle _ -> (match key with
                        | 32 -> continue := true; true
                        | _ -> false)
  | Tourney -> (match key with
                | 119 -> Ivar.fill_if_empty !move_ivar (Up,tournament); true
                | 115 -> Ivar.fill_if_empty !move_ivar (Down,tournament); true
                | 100 -> Ivar.fill_if_empty !move_ivar (Right,tournament); true
                | 97 -> Ivar.fill_if_empty !move_ivar (Left,tournament); true
                | 104 -> Ivar.fill_if_empty !move_ivar (Interact,tournament); true
                | 32 -> continue := true; true
                | _ -> false )
  | _ -> false

let () = move ()
(* ---------------------------------------------------------------------------*)


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
  | _ -> failwith "Does Not Happen"

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
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  (* hbox1 is known as main_menu outside of this function *)
  let hbox1 = GPack.hbox ~homogeneous:true ~packing:(vbox#pack) ~height:
    (screen_height/6) ()in
  let factory = new GMenu.factory menubar in
  let help_menu = factory#add_submenu "Help" in
  let _ = factory#add_item "Stats" ~callback:(fun () ->
    let error_win = GWindow.message_dialog ~message:(Save.getFileMessage ())
    ~buttons:GWindow.Buttons.close  ~message_type:`INFO () in ignore(error_win#connect#close ~callback:(error_win#destroy));
    ignore (error_win#connect#response ~callback:(fun s -> error_win#destroy ())); error_win#show ()) in
  (* Help menu *)
  let factory = new GMenu.factory help_menu in
  ignore(factory#add_item "About" ~callback: (fun () ->
    let error_win = GWindow.message_dialog ~message:("To find more about this game, please read the Documentation that comes along with it. The most common key commands are W, A, S, D for movement and Space bar for skipping text.")
    ~buttons:GWindow.Buttons.close  ~message_type:`INFO () in ignore(error_win#connect#close ~callback:(error_win#destroy));
    ignore (error_win#connect#response ~callback:(fun s -> error_win#destroy ())); error_win#show ()));
  ignore(factory#add_item "Errors" ~callback: (fun () ->let error_win = GWindow.message_dialog  ~message:("Instructions to fix corrupted save file are in the Documentation. Please do not edit the save files yourself.")
    ~buttons:GWindow.Buttons.close  ~message_type:`INFO () in ignore(error_win#connect#close ~callback:(error_win#destroy));
    ignore (error_win#connect#response ~callback:(fun s -> error_win#destroy ())); error_win#show ()));
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
  (* button 8 is known as poke_edit outside of this function *)
  let button8 = GButton.button ~label:"Poke-Editor"
    ~packing:(hbox1#pack ~expand:true ~fill:true) ~show: false () in
  (* img is known as main_menu_bg outside of this code *)
  let img = GMisc.image ~file:"./gui_pics/main.gif" ~packing:(hbox2#pack)
    ~width:screen_width ~height:(5*screen_height /6) () in
  (* load_screen is a gif that plays before battle (during initialization)*)
  let load_screen = GMisc.image ~file:"../data/backgrounds/button.png"
    ~show:false ~packing:(vbox#pack) () in
  (* Return all objects created *)
  (vbox, hbox1, hbox2, button1, button2, button3, button4,
    button5, button6, button7, button8, img, load_screen)

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
  let move1, move2, move3, move4 = match buttonshow with
  | [m1;m2;m3;m4;_] -> (m1, m2, m3, m4)
  | _ -> failwith "Faulty Game Logic: Debug 278" in
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

let load_tournament engine img bg_img load_screen battle text buttonhide buttonshow
  (battle_status, gui_ready, ready, ready_gui) main_menu (battle_screen : GPack.box)
  poke1_img poke2_img text_buffer health_holders () =
  let tournament = match buttonhide with
    | [_;_;tournament;_] -> tournament
    | _ -> failwith "Faulty Game Logic: Debug 508" in
  match !current_screen with
  | Menu1P -> (gameText#set_text "Use W,A,S,D to move. Press H to interact with Prof. Oak and space bar to talk.";
              current_screen := Tourney; List.iter (fun s -> s#misc#hide ()) buttonhide;
              img#misc#hide ();
              obstacle_coordinates := getObstacleCoordinates ();
              x := 7; y := 5;
              spriteanim#move sprite#coerce (40 * !x) (40 * !y);
              tilemap#set_file "../data/tournament/tilemap1.png";
              opp1#set_file ("../data/tournament/NPC/" ^ (getRandomOpp1 ()) ^ ".png");
              opp2#set_file ("../data/tournament/NPC/" ^ (getRandomOpp2 ()) ^ ".png");
              opp1#misc#show (); opp2#misc#show ();
              bossanim#move boss#coerce 280 100;
              (if !obstacle_coordinates = tilemap_nooak then
                boss#misc#hide ()
              else
                boss#misc#show ());
              battle_screen#pack gameBoard#coerce; battle_screen#pack gameText#coerce)
  | Tourney -> (current_screen := TourneyChoose;
                tournament#set_label "Continue";
                tournament#misc#show ();
                battle_screen#remove gameBoard#coerce;
                battle_screen#remove gameText#coerce;
                selecttext := GMisc.label ~text:"Tournament Time: Choose your Pokemon." ~packing:(battle_screen#pack) ();
                select1 := GEdit.combo ~popdown_strings:(Pokemon.unlocked_poke_string_list ()) ~case_sensitive:false ~allow_empty:false ~packing:(battle_screen#pack) ();
                select2 := GEdit.combo ~popdown_strings:(Pokemon.unlocked_poke_string_list ()) ~case_sensitive:false ~allow_empty:false ~packing:(battle_screen#pack) ();
                select3 := GEdit.combo ~popdown_strings:(Pokemon.unlocked_poke_string_list ()) ~case_sensitive:false ~allow_empty:false ~packing:(battle_screen#pack) ();
                select4 := GEdit.combo ~popdown_strings:(Pokemon.unlocked_poke_string_list ()) ~case_sensitive:false ~allow_empty:false ~packing:(battle_screen#pack) ();
                select5 := GEdit.combo ~popdown_strings:(Pokemon.unlocked_poke_string_list ()) ~case_sensitive:false ~allow_empty:false ~packing:(battle_screen#pack) ();
                select6 := GEdit.combo ~popdown_strings:(Pokemon.unlocked_poke_string_list ()) ~case_sensitive:false ~allow_empty:false ~packing:(battle_screen#pack) ();
                battle_screen#pack selectimg#coerce;
                ())
  | TourneyChoose -> (try (
                          let poke1 = Pokemon.getPresetPokemon (!select1#entry#text) in
                          let poke2 = Pokemon.getPresetPokemon (!select2#entry#text) in
                          let poke3 = Pokemon.getPresetPokemon (!select3#entry#text) in
                          let poke4 = Pokemon.getPresetPokemon (!select4#entry#text) in
                          let poke5 = Pokemon.getPresetPokemon (!select5#entry#text) in
                          let poke6 = Pokemon.getPresetPokemon (!select6#entry#text) in
                          battle_screen#remove selectimg#coerce; !selecttext#destroy (); tournament#misc#hide ();
                          !select1#destroy (); !select2#destroy (); !select3#destroy ();
                          !select4#destroy (); !select5#destroy (); !select6#destroy ();
                          tournament#set_label "Tournament";
                          load_battle_load engine img bg_img load_screen battle text buttonhide buttonshow
                          (battle_status, gui_ready, ready, ready_gui) (TournBattle [poke1;poke2;poke3;poke4;poke5;poke6]) main_menu battle_screen
                          poke1_img poke2_img text_buffer health_holders ()
                      ) with _ -> let error_win = GWindow.message_dialog ~message:"Error in your Pokemon selection. Try making sure everything is spelled correctly."
                                  ~buttons:GWindow.Buttons.close  ~message_type:`ERROR () in ignore(error_win#connect#close ~callback:(error_win#destroy));
                                  ignore (error_win#connect#response ~callback:(fun s -> error_win#destroy ())); error_win#show ())
  | _ -> failwith "Faulty Game Logic: Debug 524"


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

let load_poke_edit engine img bg_img load_screen battle text buttonhide (poke_edit : GButton.button)
  buttonshow (battle_status, gui_ready, ready, ready_gui) main_menu (battle_screen : GPack.box)
  poke1_img poke2_img text_buffer health_holders () =
  match !current_screen with
  | Menu1P -> current_screen := PokeEdit;
              List.iter (fun s -> s#misc#hide ()) buttonhide;
              selecttext := GMisc.label ~text:"Choose One of Your Unlocked Pokemon to Edit:" ~packing:(battle_screen#pack) ();
              poke_edit#set_label "Continue";
              img#misc#hide ();
              select1 := GEdit.combo ~popdown_strings:(Pokemon.unlocked_poke_string_list ()) ~case_sensitive:false ~allow_empty:false ~packing:(battle_screen#pack) ();
              editimg#set_file "../data/backgrounds/pokeedit.jpg"; battle_screen#pack editimg#coerce;
              ()
  | PokeEdit -> (try (let pokename = !select1#entry#text in
                      let poke = Pokemon.getPresetPokemon pokename in
                      let move_lst = Pokemon.getAllMoves pokename in
                      let abil_lst = Pokemon.getAllAbilities pokename in
                      !select1#destroy (); !selecttext#set_text ("Now editing " ^ pokename ^ "! Your current values are already loaded in.");
                      editimg#set_file ("../data/sprites/" ^ pokename ^ ".gif");
                      battle_screen#pack pokedit_screen#coerce;
                      select1 := GEdit.combo ~popdown_strings:(move_lst) ~case_sensitive:false ~allow_empty:false ~packing:(pokedit_combos#pack ~expand:true) ();
                      !select1#entry#set_text poke.move1.name;
                      select2 := GEdit.combo ~popdown_strings:(move_lst) ~case_sensitive:false ~allow_empty:false ~packing:(pokedit_combos#pack ~expand:true) ();
                      !select2#entry#set_text poke.move2.name;
                      select3 := GEdit.combo ~popdown_strings:(move_lst) ~case_sensitive:false ~allow_empty:false ~packing:(pokedit_combos#pack ~expand:true) ();
                      !select3#entry#set_text poke.move3.name;
                      select4 := GEdit.combo ~popdown_strings:(move_lst) ~case_sensitive:false ~allow_empty:false ~packing:(pokedit_combos#pack ~expand:true) ();
                      !select4#entry#set_text poke.move4.name;
                      select5 := GEdit.combo ~popdown_strings:(abil_lst) ~case_sensitive:false ~allow_empty:false ~packing:(pokedit_combos#pack ~expand:true) ();
                      !select5#entry#set_text poke.ability;
                      select6 := GEdit.combo ~popdown_strings:(Pokemon.nature_list) ~case_sensitive:false ~allow_empty:false ~packing:(pokedit_combos#pack ~expand:true) ();
                      !select6#entry#set_text (Pokemon.string_of_nature poke.nature);
                      select7 := GEdit.combo ~popdown_strings:(Pokemon.item_list) ~case_sensitive:false ~allow_empty:false ~packing:(pokedit_combos#pack ~expand:true) ();
                      !select7#entry#set_text (Pokemon.string_of_item poke.item);
                      poke_edit#set_label ("Save " ^ pokename);
                      hp_evs#set_adjustment (GData.adjustment ~upper:252. ~page_size:0. ~value:(float_of_int poke.evs.hp) ());
                      atk_evs#set_adjustment (GData.adjustment ~upper:252. ~page_size:0. ~value:(float_of_int poke.evs.attack) ());
                      def_evs#set_adjustment (GData.adjustment ~upper:252. ~page_size:0. ~value:(float_of_int poke.evs.defense) ());
                      special_attack_evs#set_adjustment (GData.adjustment ~upper:252. ~page_size:0. ~value:(float_of_int poke.evs.special_attack) ());
                      special_defense_evs#set_adjustment (GData.adjustment ~upper:252. ~page_size:0. ~value:(float_of_int poke.evs.special_defense) ());
                      speed_evs#set_adjustment (GData.adjustment ~upper:252. ~page_size:0. ~value:(float_of_int poke.evs.speed) ()); current_screen := PokeEditor;
                  ()
                ) with _ -> let error_win = GWindow.message_dialog ~message:"Error in your Pokemon selection. Try making sure everything is spelled correctly."
                                  ~buttons:GWindow.Buttons.close  ~message_type:`ERROR () in ignore(error_win#connect#close ~callback:(error_win#destroy));
                                  ignore (error_win#connect#response ~callback:(fun s -> error_win#destroy ())); error_win#show ())
  | PokeEditor -> (try (let pokename = String.sub poke_edit#label 5 (String.length poke_edit#label - 5) in
                  Save.createSavePokeEdit pokename !select1#entry#text !select2#entry#text
                    !select3#entry#text !select4#entry#text !select5#entry#text !select6#entry#text
                    !select7#entry#text (int_of_float hp_evs#adjustment#value) (int_of_float atk_evs#adjustment#value)
                    (int_of_float def_evs#adjustment#value) (int_of_float special_attack_evs#adjustment#value) (int_of_float special_defense_evs#adjustment#value)
                    (int_of_float speed_evs#adjustment#value);
                    let success_win = GWindow.message_dialog ~message:"Save successful!"
                                  ~buttons:GWindow.Buttons.close  ~message_type:`INFO () in ignore(success_win#connect#close ~callback:(success_win#destroy));
                                  ignore (success_win#connect#response ~callback:(fun s -> success_win#destroy ())); success_win#show ())
                   with | err ->  (let message = match err with
                                  | Save.FaultyGameSave -> "Corrupted Save File."
                                  | Save.BadFieldOption -> "Error in Moves/Items/Ability/Nature. Make sure everything is spelled correctly."
                                  | Save.BadEVInput -> "The maximum EVs can add up to is 510."
                                  | _ -> "Unknown Error" in
                                  let error_win = GWindow.message_dialog ~message:message
                                  ~buttons:GWindow.Buttons.close  ~message_type:`ERROR () in ignore(error_win#connect#close ~callback:(error_win#destroy));
                                  ignore (error_win#connect#response ~callback:(fun s -> error_win#destroy ())); error_win#show ()))
  | _ -> failwith "Faulty Game Logic: Debug 550"

let load_preset engine img bg_img load_screen battle text buttonhide preset buttonshow
  (battle_status, gui_ready, ready, ready_gui) main_menu (battle_screen : GPack.box)
  poke1_img poke2_img text_buffer health_holders () =
  (match !current_screen with
    | Menu1P -> (current_screen := Preset1PChoose;
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
                battle_screen#pack selectimg#coerce;
                ())
    | Menu2P -> ()
    | Preset1PChoose -> (try (
                          let poke1 = Pokemon.getPresetPokemon (!select1#entry#text) in
                          let poke2 = Pokemon.getPresetPokemon (!select2#entry#text) in
                          let poke3 = Pokemon.getPresetPokemon (!select3#entry#text) in
                          let poke4 = Pokemon.getPresetPokemon (!select4#entry#text) in
                          let poke5 = Pokemon.getPresetPokemon (!select5#entry#text) in
                          let poke6 = Pokemon.getPresetPokemon (!select6#entry#text) in
                          battle_screen#remove selectimg#coerce; !selecttext#destroy (); preset#misc#hide ();
                          !select1#destroy (); !select2#destroy (); !select3#destroy ();
                          !select4#destroy (); !select5#destroy (); !select6#destroy ();
                          preset#set_label "Preset Battle";
                          load_battle_load engine img bg_img load_screen battle text buttonhide buttonshow
                          (battle_status, gui_ready, ready, ready_gui) (Preset1p [poke1;poke2;poke3;poke4;poke5;poke6]) main_menu battle_screen
                          poke1_img poke2_img text_buffer health_holders ()
                      ) with _ -> let error_win = GWindow.message_dialog ~message:"Error in your Pokemon selection. Try making sure everything is spelled correctly."
                                  ~buttons:GWindow.Buttons.close  ~message_type:`ERROR () in ignore(error_win#connect#close ~callback:(error_win#destroy));
                                  ignore (error_win#connect#response ~callback:(fun s -> error_win#destroy ())); error_win#show ())
    | _ -> failwith "Faulty Game Logic: Debug 314")

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

let go_back engine (menu_holder, main_menu, battle_screen, one_player,
    two_player, no_player, random, preset, touranment, back_button,
    poke_edit, main_menu_bg, load_screen) (battle, text, bg_img, move1, move2,
    move3, move4, switch, poke1_img, poke2_img, move_img, text_buffer, poke1, poke2,
    poke3, poke4, poke5, health_holders, pokeanim1, pokeanim2, moveanim) battle_engine () =
  (if !current_screen = Menu1P || !current_screen = Menu2P || !current_screen = Menu0P then
    load_menu engine [one_player;two_player;no_player]
    [random; preset ;touranment; poke_edit; back_button] main_menu_bg
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
    battle_screen#remove selectimg#coerce; main_menu_bg#misc#show (); !selecttext#destroy ();
    !select1#destroy (); !select2#destroy (); !select3#destroy ();
    !select4#destroy (); !select5#destroy (); !select6#destroy ();
    load_menu engine [random; touranment; poke_edit] [] main_menu_bg Menu1P ());
  if (!current_screen = Tourney) then
    (current_screen := Menu1P; battle_screen#remove gameBoard#coerce;
      battle_screen#remove gameText#coerce; main_menu_bg#misc#show ();
      random#misc#show (); preset#misc#show (); poke_edit#misc#show ();
      touranment#misc#show ());
  if (!current_screen = TourneyChoose) then
    (current_screen := Menu1P;
    touranment#set_label "Tournament";
    battle_screen#remove selectimg#coerce; main_menu_bg#misc#show (); !selecttext#destroy ();
    !select1#destroy (); !select2#destroy (); !select3#destroy ();
    !select4#destroy (); !select5#destroy (); !select6#destroy ();
    load_menu engine [random; preset ; poke_edit] [] main_menu_bg Menu1P ());
  if (!current_screen = PokeEdit) then
    (current_screen := Menu1P;
    poke_edit#set_label "Poke Editor";
    battle_screen#remove editimg#coerce; main_menu_bg#misc#show ();
    !selecttext#destroy (); !select1#destroy ();
    load_menu engine [random;touranment;preset] [] main_menu_bg Menu1P ());
  if (!current_screen = PokeEditor) then
    (current_screen := Menu1P;
    poke_edit#set_label "Poke Editor";
    battle_screen#remove pokedit_screen#coerce;
    !select1#destroy (); !select2#destroy (); !select3#destroy ();
    !select4#destroy (); !select5#destroy (); !select6#destroy ();
    !select7#destroy (); !selecttext#destroy (); main_menu_bg#misc#show ();
    battle_screen#remove editimg#coerce;
    load_menu engine [random;touranment;preset] [] main_menu_bg Menu1P ());
  ()

let rec findTrapped = function
  | (PartialTrapping _)::t -> true
  | h::t -> findTrapped t
  | [] -> false

let switch_poke engine pokebuttons battlebuttons back_button () =
  let poke1,poke2,poke3,poke4,poke5 = match pokebuttons with
  | [poke1;poke2;poke3;poke4;poke5] -> (poke1, poke2, poke3, poke4, poke5)
  | _ -> failwith "Faulty Game Logic: Debug 469" in
  let move1, move2, move3, move4, switch = match battlebuttons with
  | [move1;move2;move3;move4;switch] -> (move1, move2, move3, move4, switch)
  | _ -> failwith "Faulty Game Logic: Debug 472" in
  let switch_poke_helper i a =
    let big_help poke = poke#set_label a.pokeinfo.name; poke#misc#show () in
    match i with
    | 0 -> big_help poke1
    | 1 -> big_help poke2
    | 2 -> big_help poke3
    | 3 -> big_help poke4
    | 4 -> big_help poke5
    | _ -> failwith "Invariant is not having more than five pokemon" in
  let team = (match get_game_status engine with
  | Battle (InGame (t1, t2, _, _, _)) ->
      (match !current_screen with
      | Battle (P1 BothFaint) -> back_button#misc#hide (); t1
      | Battle (P1 Faint) | Battle (P1 SwitchPokeF) -> back_button#misc#hide (); t1
      | Battle (P2 Faint) | Battle (P2 SwitchPokeF) -> back_button#misc#hide (); t2
      | Battle (P1 _) -> current_screen := Battle (P1 SwitchPoke); t1
      | Battle (P2 _) -> current_screen := Battle (P2 SwitchPoke); t2
      | _ -> failwith "Faulty Game logic")
  | _ -> failwith "Fauly game Logic") in
  if findTrapped (snd team.current.curr_status) && (!current_screen = Battle (P1 SwitchPoke) || !current_screen = Battle (P2 SwitchPoke)) then
    (
      let error_win = GWindow.message_dialog ~message:"You're Pokemon is trapped!"
       ~buttons:GWindow.Buttons.close  ~message_type:`ERROR () in ignore(error_win#connect#close ~callback:(error_win#destroy));
       ignore (error_win#connect#response ~callback:(fun s -> error_win#destroy ())); error_win#show ();
       (match !current_screen with
       | Battle (P1 _) -> current_screen := Battle (P1 ChooseMove)
       | Battle (P2 _) -> current_screen := Battle (P2 ChooseMove)
       | _ -> failwith "Faulty Game Logic")
    )
  else (
    List.iter (fun s -> s#misc#hide ()) [move1;move2;move3;move4;switch];
    List.iteri switch_poke_helper team.alive
  )

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
  | NormMove s -> `DontMiss s
  | Crit s -> getMoveString s
  | SEff s -> getMoveString s
  | NoEff s -> getMoveString s
  | NoEffAll s -> `DontMiss s
  | StatBoostA (stat, i, s) -> getMoveString s
  | StatAttackA (stat, i, s) -> getMoveString s
  | HitMult (n, s) -> getMoveString s
  | BurnMove s -> getMoveString s
  | FreezeMove s -> getMoveString s
  | ParaMove s -> getMoveString s
  | SleepMove s -> getMoveString s
  | MissMove s ->  `Miss s
  | Asleep -> `SleepMiss
  | Wake s -> getMoveString s
  | FrozenSolid -> `FrozenMiss
  | Thaw s-> getMoveString s
  | NoFreeze s -> getMoveString s
  | NoBurn s -> getMoveString s
  | NoPara s -> getMoveString s
  | Para -> `ParaMiss
  | OHKill s -> getMoveString s
  | FlinchA -> `DontMove
  | PoisonMove s -> getMoveString s
  | Recoil s -> getMoveString s
  | NoRecoil s -> getMoveString s
  | BreakConfuse s -> getMoveString s
  | Confused -> `ConfuseMiss
  | DrainA s-> getMoveString s
  | ConfuseMoveA s -> getMoveString s
  | UserFaintA s -> getMoveString s
  | DrainSleepFail s -> `DontMove
  | BreakSub s -> getMoveString s
  | SubDmg s -> getMoveString s
  | ProtectedA s -> `DontMiss s
  | ChargingMove (s, n) -> `DontMove
  | SwitchOutA s -> getMoveString s
  | Recharging s -> getMoveString s
  | FalseSwipeA s -> getMoveString s
  | ConfuseUserA s -> getMoveString s
  | SleepAttack s -> getMoveString s
  | SleepAttackFail _ -> `DontMove
  | TrappingMove s -> getMoveString s
  | LifeOrbA s -> getMoveString s
  | KnockedOff (_, s) -> getMoveString s

let rec getMoveStringStatus a =
  match a with
  | NormStatus s -> `DontMissStatus
  | StatBoost (stat, i, s) -> `DontMissStatus
  | StatAttack (stat, i, s) -> `DontMissStatus
  | MissStatus s -> `DontMove
  | FrozenSolidS -> `FrozenMiss
  | PoisonStatus s-> `DontMissStatus
  | BurnStatus s -> `DontMissStatus
  | BadPoisonStatus s -> `DontMissStatus
  | ParaStatus s -> `DontMissStatus
  | ThawS s -> getMoveStringStatus s
  | NoFreezeS s -> getMoveStringStatus s
  | NoBurnS s -> getMoveStringStatus s
  | NoParaS s -> getMoveStringStatus s
  | ParaS -> `ParaMiss
  | AsleepS -> `SleepMiss
  | WakeS s -> getMoveStringStatus s
  | MakeSleep s -> `DontMissStatus
  | FlinchS -> `DontMove
  | BreakConfuseS s-> getMoveStringStatus s
  | ConfusedS -> `ConfuseMiss
  | ConfuseMove s -> `DontMissStatus
  | LeechS s -> `LeechStatus
  | HealHealth s -> `HealStatus
  | LightScreenS s -> `ShieldStatus
  | HazeS s -> getMoveStringStatus s
  | ReflectS s -> getMoveStringStatus s
  | RestS s -> `HealStatus
  | SubBlock s -> getMoveStringStatus s
  | SubFail s -> getMoveStringStatus s
  | SubMake s -> `SubStatus
  | ProtectedS s -> `DontMove
  | ProtectS s-> `ShieldStatus
  | ProtectFail s-> `DontMove
  | SpikesS s -> `SpikesStatus
  | ToxicSpikesS s -> `TSpikesStatus
  | HealBellS s -> `DontMissStatus
  | RefreshS s -> `DontMissStatus
  | Fail s -> `DontMove
  | PsychUpS s -> `DontMissStatus
  | SunnyDayS s | RainDanceS s | SandStormS s | HailS s -> `DontMove
  | EncoreS s -> `DontMissStatus
  | EncoreFail -> `DontMove
  | CopyPrevMoveS s -> getMoveStringStatus s
  | CopyPrevMoveA s -> getMoveString s
  | CopyFail -> `DontMove
  | TauntS _ -> `DontMissStatus
  | UserFaintS s -> getMoveStringStatus s
  | TauntFail -> `DontMove
  | Taunted _ -> `DontMove
  | StealthRockS _ -> `RockStatus
  | StickyWebS _-> `DontMissStatus
  | SleepTalkA (_, a) -> getMoveString a
  | SleepTalkS (_, s) -> getMoveStringStatus s
  | SleepAttackS s -> getMoveStringStatus s
  | SwitchOut s -> `DontMove

let rec getMoveStringEnd a =
  match a with
  | BurnDmg -> `BurnDmg
  | PoisonDmg -> `PoisonDmg
  | _ -> `DontMove

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
  | BurnMove s -> getAttackString starter s ^ "The opponent has been burned."
  | FreezeMove s -> getAttackString starter s ^ "The opponent is frozen solid."
  | ParaMove s -> getAttackString starter s ^ "The opponent has been paralyzed."
  | SleepMove s -> getAttackString starter s ^ "The opponent has been put to sleep."
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
  | ConfuseUserA s -> getAttackString starter s ^ starter ^ " has confused itself."
  | KnockedOff (item, s) -> getAttackString starter s ^ starter ^ " has knocked off the opponent's " ^ (Pokemon.string_of_item item)
  | SleepAttack s -> starter ^ " is fast asleep." ^ getAttackString starter s
  | SleepAttackFail s -> starter ^ " used " ^ s ^ " but it wasn't asleep."
  | TrappingMove s -> getAttackString starter s ^ "The opponent has been trapped."
  | NoRecoil s -> getAttackString starter s ^ starter ^ "'s ability prevented the recoil damage."
  | LifeOrbA s -> getAttackString starter s ^ starter ^ " has lost some health from its life orb."
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
  | SleepAttackS s -> starter ^ " was asleep." ^ getStatusString starter s
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
  | EncoreS s -> getStatusString starter s ^ starter ^ " has trapped the opponent in an encore."
  | UserFaintS s -> getStatusString starter s ^ starter ^ " took damage from the move and is about to faint."
  | EncoreFail -> starter ^ " used encore but it failed."
  | CopyPrevMoveS s -> starter ^ " copied the opponent's move." ^ getStatusString starter s
  | CopyPrevMoveA s -> starter ^ " copied the opponent's move." ^ getAttackString starter s
  | CopyFail -> starter ^ " tried to copy the previous move but failed."
  | TauntS s -> getStatusString starter s ^ "The opponent has been taunted."
  | ToxicSpikesS s -> getStatusString starter s ^ starter ^ " has deposited a layer of toxic spikes."
  | TauntFail -> starter ^ " used Taunt but it failed."
  | Taunted s -> starter ^ " couldn't use " ^ s ^ " because it was taunted."
  | StealthRockS s -> getStatusString starter s ^ "Rocks were put on the opponent's side."
  | StickyWebS s -> getStatusString starter s ^ starter ^ " has placed a sticky web on the opponent's side."
  | SleepTalkA (s1, s2) -> getStatusString starter s1 ^ getAttackString starter s2
  | SleepTalkS (s1, s2) -> getStatusString starter s1 ^ getStatusString starter s2
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
  | TrapDamage (s, s') -> starter ^ " has taken damage from " ^ s ^ "." ^ getEndString starter s'
  | LightScreenFade s -> getEndString starter s ^ starter ^ "'s Light Screen has faded."
  | TauntFade s -> getEndString starter s ^ starter ^ "'s Taunt has faded."
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
  | LeftOversHeal s -> getEndString starter s ^ starter ^ " has healed from the leftovers."


let animate_attack (animbox : GPack.fixed) img startx starty nextx' nexty (moveanim : GPack.fixed) move_img movestring =
  match movestring with
  | `SleepMiss ->
      ( move_img#misc#show ();
      moveanim#move move_img#coerce startx (starty-30);
      for i = 0 to 2 do
        move_img#set_file "../data/fx/sleep1.png";
        for i = 0 to 40 do
          busywait_small ()
        done;
        move_img#set_file "../data/fx/sleep2.png";
        for i = 0 to 40 do
          busywait_small ()
        done;
        move_img#set_file "../data/fx/sleep3.png";
        for i = 0 to 40 do
          busywait_small ()
        done
      done; move_img#misc#hide ())
  | `FrozenMiss -> ()
  | `ParaMiss ->
      ( move_img#misc#show ();
      moveanim#move move_img#coerce (startx-30) starty;
      for i = 0 to 2 do
        move_img#set_file "../data/fx/paralysis1.png";
        for i = 0 to 40 do
          busywait_small ()
        done;
        move_img#set_file "../data/fx/paralysis2.png";
        for i = 0 to 40 do
          busywait_small ()
        done;
        move_img#set_file "../data/fx/paralysis3.png";
        for i = 0 to 40 do
          busywait_small ()
        done
      done; move_img#misc#hide ())
  | `FlinchMiss -> ()
  | `ConfuseMiss -> ()
  | `DontMissStatus -> ()
  | `SubStatus -> ()
  | `ShieldStatus -> ()
  | `SpikesStatus -> ()
  | `TSpikesStatus -> ()
  | `RockStatus -> ()
  | `HealStatus -> ()
  | `LeechStatus -> ()
  | `BurnDmg ->
    ( move_img#misc#show ();
      moveanim#move move_img#coerce (startx-30) starty;
      for i = 0 to 2 do
        move_img#set_file "../data/fx/burn1.png";
        for i = 0 to 40 do
          busywait_small ()
        done;
        move_img#set_file "../data/fx/burn2.png";
        for i = 0 to 40 do
          busywait_small ()
        done;
        move_img#set_file "../data/fx/burn3.png";
        for i = 0 to 40 do
          busywait_small ()
        done
      done; move_img#misc#hide ())
  | `PoisonDmg ->
    ( move_img#misc#show ();
      moveanim#move move_img#coerce (startx-30) starty;
      for i = 0 to 2 do
        move_img#set_file "../data/fx/poison1.png";
        for i = 0 to 40 do
          busywait_small ()
        done;
        move_img#set_file "../data/fx/poison2.png";
        for i = 0 to 40 do
          busywait_small ()
        done;
        move_img#set_file "../data/fx/poison3.png";
        for i = 0 to 40 do
          busywait_small ()
        done
      done; move_img#misc#hide ())
  | `DontMove -> ()
  | `DontMiss s | `Miss s ->
    (let themove = Pokemon.getMoveFromString s in
     let nextx = (if movestring = `DontMiss s then nextx' else screen_width / 2) in
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

let rec game_animation engine buttons (battle: GPack.table) text
  (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer (health_bar_holder1, health_bar_holder2, health_bar1, health_bar2)  pokeanim1 pokeanim2 moveanim back_button () =
  let move1, move2, move3, move4, poke1, poke2, poke3, poke4, poke5, switch = match buttons with
 | [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] -> move1, move2, move3, move4, poke1, poke2, poke3, poke4, poke5, switch
 | _ -> failwith "Faulty Game Logic: Debug 982" in
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
    poke1_img#set_file ("../data/back-sprites/" ^ t1.current.pokeinfo.name  ^ ".gif");
    poke2_img#set_file ("../data/sprites/" ^ t2.current.pokeinfo.name  ^ ".gif");
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
    else if recharging1 then
      (current_command := (Some NoMove, snd !current_command))
    else
      ());
    (if (charging2) then
      (current_command := (fst !current_command, Some (UseAttack s2)))
    else if recharging2 then
      (current_command := (fst !current_command, Some NoMove))
    else
      ());
    (t1.current.curr_status <- (fst t1.current.curr_status, List.filter (fun s -> s <> RechargingStatus) (snd t1.current.curr_status)));
    (t2.current.curr_status <- (fst t2.current.curr_status, List.filter (fun s -> s <> RechargingStatus) (snd t2.current.curr_status))) in
  let skipturn () =
    update_current_command ();
    match get_game_status battle_status with
    | Random1p | Preset1p _| TournBattle _ -> (match !current_command with
                  | (None, _) -> text_buffer#set_text (Pokemon.string_of_weather w.weather); List.iter (fun s -> s#misc#show ()) battle_buttons; current_screen := Battle (P1 ChooseMove); update_buttons engine move1 move2 move3 move4
                  | _ -> Ivar.fill !gui_ready !current_command; current_command := (None, None); game_step ())
    | Random2p -> (match !current_command with
                  | (None, _) -> text_buffer#set_text (Pokemon.string_of_weather w.weather); List.iter (fun s -> s#misc#show ()) battle_buttons; current_screen := Battle (P1 ChooseMove); update_buttons engine move1 move2 move3 move4
                  | (_, None) -> text_buffer#set_text (Pokemon.string_of_weather w.weather); List.iter (fun s -> s#misc#show ()) battle_buttons; current_screen := Battle (P2 ChooseMove); update_buttons engine move1 move2 move3 move4
                  | _ -> Ivar.fill !gui_ready !current_command; current_command := (None, None); game_step ()) in
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
  | Pl1 SPoke (p,mess) -> (let switch_string = ("Player One has switched to " ^ p ^ mess) in updatetools ();
                   let str_list = Str.split (Str.regexp "\\.") switch_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list)
  | Pl2 SPoke (p,mess) ->  (let switch_string = ("Player One has switched to " ^ p ^ mess) in updatetools ();
                           let str_list = Str.split (Str.regexp "\\.") switch_string in
                           List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list)
  | Pl1 AttackMove a ->secondaryEffect := `P1;
                   let atk_string = getAttackString t1.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\\.") atk_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim1 poke1_img poke1x poke1y poke2x poke2y moveanim move_img (getMoveString a);
                   updatehealth2 ()
  | Pl2 AttackMove a -> secondaryEffect := `P2;
                   let atk_string = getAttackString t2.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\\.") atk_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim2 poke2_img poke2x poke2y poke1x poke1y moveanim move_img (getMoveString a);
                   updatehealth1 ()
  | Pl1 Status s ->secondaryEffect := `P1;
                   let status_string = getStatusString t1.current.pokeinfo.name s in
                   let str_list = Str.split (Str.regexp "\\.") status_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim1 poke1_img poke1x poke1y poke2x poke2y moveanim move_img (getMoveStringStatus s)
  | Pl2 Status s ->secondaryEffect := `P2;
                   let status_string = getStatusString t2.current.pokeinfo.name s in
                   let str_list = Str.split (Str.regexp "\\.") status_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim2 poke2_img poke2x poke2y poke1x poke1y moveanim move_img (getMoveStringStatus s)
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
  | Pl1 ForceChoose a ->
                  let atk_string = getAttackString t1.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\\.") atk_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim1 poke1_img poke1x poke1y poke2x poke2y moveanim move_img (getMoveString a);
                   updatehealth2 ();
                   text_buffer#set_text "Player One Pokemon has switched out. Choosing a new Pokemon.";
                   (match !m2 with
                   | Pl2 ForceMove a -> current_command := (fst !current_command, Some (UseAttack a))
                   | Pl2 ForceNone -> current_command := (fst !current_command, Some NoMove)
                   | _ -> failwith "Faulty Game Logic: Debug 1353" ); busywait (); updatetools (); current_screen := Battle (P1 SwitchPokeF);
                   switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                   move3;move4;switch] back_button ()
  | Pl2 ForceChoose a ->
                  let atk_string = getAttackString t2.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\\.") atk_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim2 poke2_img poke2x poke2y poke1x poke1y moveanim move_img (getMoveString a);
                   updatehealth1 ();
                   text_buffer#set_text "Player Two Pokemon has switched out. Choosing a new Pokemon.";
                   (match !m2 with
                   | Pl1 ForceMove a -> current_command := (Some (UseAttack a), snd !current_command)
                   | Pl1 ForceNone -> current_command := (Some NoMove, snd !current_command)
                   | _ -> failwith "Faulty Game Logic: Debug 1353" );
                   (match get_game_status battle_status with
                    | Random1p | Preset1p _ | TournBattle _ -> busywait (); current_command := (fst !current_command), Some (Poke "random");
                              simple_move()
                    | Random2p -> current_screen := Battle (P2 SwitchPokeF); busywait (); updatetools ();
                              current_command := (fst !current_command, snd !current_command);
                              switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                              move3;move4;switch] back_button ())
  | Pl1 Faint ->  if List.length t1.dead = 5 then (text_buffer#set_text "Player Two wins!";
                  current_screen := Battle (P1 ChooseMove); back_button#misc#show ()) else
                  (text_buffer#set_text "Player One Pokemon has fainted. Choosing a new Pokemon.";
                  (match !m2 with
                  | Pl2 Faint -> current_screen := Battle (P1 BothFaint)
                  | _ -> current_screen := Battle (P1 Faint); current_command := (fst !current_command, Some NoMove));
                  busywait (); updatetools ();
                  switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                  move3;move4;switch] back_button ())
  | Pl2 Faint -> if List.length t2.dead = 5 then (text_buffer#set_text "Player One wins!";
                  (match get_game_status battle_status with
                      | TournBattle _ -> (try (let newpoke = unlockPokemon () in let success_win = GWindow.message_dialog ~message:("Unlocked " ^ newpoke)
                                          ~buttons:GWindow.Buttons.close  ~message_type:`INFO () in ignore(success_win#connect#close ~callback:(success_win#destroy));
                                          ignore (success_win#connect#response ~callback:(fun s -> success_win#destroy ())); success_win#show ())
                                          with | err ->  (let message = match err with
                                              | Save.FaultyGameSave -> "Corrupted Save File."
                                              | Save.OwnPokemonAlready -> "You already own the unlocked Pokemon. Better luck next time."
                                              | _ -> "Unknown Error" in
                                          let error_win = GWindow.message_dialog ~message:message
                                          ~buttons:GWindow.Buttons.close  ~message_type:`ERROR () in ignore(error_win#connect#close ~callback:(error_win#destroy));
                                          ignore (error_win#connect#response ~callback:(fun s -> error_win#destroy ())); error_win#show ()))
                      | _ -> ()); current_screen := Battle (P1 ChooseMove); back_button#misc#show ()) else
                  (text_buffer#set_text "Player Two Pokemon has fainted. Choosing a new Pokemon.";
                 (match get_game_status battle_status with
                 | Random1p | Preset1p _ | TournBattle _ -> busywait (); current_command := ((if fst !current_command = None then Some (NoMove) else fst !current_command), Some (FaintPoke ""));
                               simple_move()
                 | Random2p -> current_screen := Battle (P2 Faint); busywait (); updatetools ();
                              current_command := (Some NoMove, snd !current_command);
                              switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                              move3;move4;switch] back_button ()))
  | Pl2 SFaint ->if List.length t2.dead = 5 then (text_buffer#set_text "Player One wins!";
                  (match get_game_status battle_status with
                      | TournBattle _ -> (try (let newpoke = unlockPokemon () in let success_win = GWindow.message_dialog ~message:("Unlocked " ^ newpoke)
                                          ~buttons:GWindow.Buttons.close  ~message_type:`INFO () in ignore(success_win#connect#close ~callback:(success_win#destroy));
                                          ignore (success_win#connect#response ~callback:(fun s -> success_win#destroy ())); success_win#show ())
                                          with | err ->  (let message = match err with
                                              | Save.FaultyGameSave -> "Corrupted Save File."
                                              | Save.OwnPokemonAlready -> "You already own the unlocked Pokemon. Better luck next time."
                                              | _ -> "Unknown Error" in
                                          let error_win = GWindow.message_dialog ~message:message
                                          ~buttons:GWindow.Buttons.close  ~message_type:`ERROR () in ignore(error_win#connect#close ~callback:(error_win#destroy));
                                          ignore (error_win#connect#response ~callback:(fun s -> error_win#destroy ())); error_win#show ()))
                      | _ -> ()); current_screen := Battle (P1 ChooseMove); back_button#misc#show ()) else
                 (poke2_img#set_file ("../data/sprites/" ^ t2.current.pokeinfo.name ^ ".gif");
                 text_buffer#set_text (t2.current.pokeinfo.name ^ " has fainted. Choosing a new Pokemon.");
                 (match get_game_status battle_status with
                 | Random1p | Preset1p _ | TournBattle _ -> busywait (); current_command := (Some (NoMove), Some (FaintPoke ""));
                               simple_move()
                 | Random2p -> current_screen := Battle (P2 Faint); busywait (); updatetools ();
                                current_command := (Some (NoMove), snd !current_command);
                                switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                                move3;move4;switch] back_button ()))
  | Pl1 EndMove x -> let txt = getEndString t1.current.pokeinfo.name x in
                    let str_list = Str.split (Str.regexp "\\.") txt in
                    List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                    animate_attack pokeanim1 poke1_img poke1x poke1y poke2x poke2y moveanim move_img (getMoveStringEnd x);
                    updatehealth1 ()
  | _ -> failwith "unimplements");
  if !endTurnEarly then
    (endTurnEarly := false; turn_end ())
  else
  (match !m2 with
  | Pl1 AttackMove a ->  secondaryEffect := `P1;
                   let atk_string = getAttackString t1.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\\.") atk_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim1 poke1_img poke1x poke1y poke2x poke2y moveanim move_img (getMoveString a);
                   updatetools ();
                   pre_process ()
  | Pl2 AttackMove a -> secondaryEffect := `P2;
                   let atk_string = getAttackString t2.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\\.") atk_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim2 poke2_img poke2x poke2y poke1x poke1y moveanim move_img (getMoveString a);
                   updatetools ();
                   pre_process ()
  | Pl1 Status s ->secondaryEffect := `P1;
                   let status_string = getStatusString t1.current.pokeinfo.name s in
                   let str_list = Str.split (Str.regexp "\\.") status_string in
                   updatetools ();
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim1 poke1_img poke1x poke1y poke2x poke2y moveanim move_img (getMoveStringStatus s);
                   pre_process ()
  | Pl2 Status s ->secondaryEffect := `P2;
                   let status_string = getStatusString t2.current.pokeinfo.name s in
                   let str_list = Str.split (Str.regexp "\\.") status_string in
                   updatetools ();
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim2 poke2_img poke2x poke2y poke1x poke1y moveanim move_img (getMoveStringStatus s);
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
  | Pl1 FaintNext | Pl2 FaintNext | Pl2 Faint | Pl1 ForceMove _| Pl2 ForceMove _ | Pl1 ForceNone | Pl2 ForceNone -> ()
  | Pl1 NoAction -> pre_process ()
  | Pl2 NoAction -> pre_process ()
  | Pl2 SPoke (p,mess) -> (let switch_string = ("Player One has switched to " ^ p  ^ mess) in updatetools ();
                           let str_list = Str.split (Str.regexp "\\.") switch_string in
                           List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list; pre_process ())
  | Pl2 EndMove x -> let txt = getEndString t2.current.pokeinfo.name x in
                    let str_list = Str.split (Str.regexp "\\.") txt in
                    List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                    animate_attack pokeanim2 poke2_img poke2x poke2y poke1x poke1y moveanim move_img (getMoveStringEnd x);
                    updatehealth2 (); turn_end ()
   | Pl1 ForceChoose a ->
                  let atk_string = getAttackString t1.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\\.") atk_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim1 poke1_img poke1x poke1y poke2x poke2y moveanim move_img (getMoveString a);
                   updatehealth2 ();
                   text_buffer#set_text "Player One Pokemon has switched out. Choosing a new Pokemon.";
                   current_command := (fst !current_command, Some NoMove); busywait (); updatetools (); current_screen := Battle (P1 SwitchPokeF);
                   switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                   move3;move4;switch] back_button ()
  | Pl2 ForceChoose a ->
                  let atk_string = getAttackString t2.current.pokeinfo.name a in
                   let str_list = Str.split (Str.regexp "\\.") atk_string in
                   List.iter (fun s -> text_buffer#set_text s; busywait ()) str_list;
                   animate_attack pokeanim2 poke2_img poke2x poke2y poke1x poke1y moveanim move_img (getMoveString a);
                   updatehealth1 ();
                   text_buffer#set_text "Player Two Pokemon has switched out. Choosing a new Pokemon.";
                   current_command := (Some NoMove, snd !current_command);
                   (match get_game_status battle_status with
                    | Random1p | Preset1p _ | TournBattle _ -> busywait (); current_command := (fst !current_command), Some (Poke "random");
                              simple_move()
                    | Random2p -> current_screen := Battle (P2 SwitchPokeF); busywait (); updatetools ();
                              switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                              move3;move4;switch] back_button ())
  | _ -> failwith "unimplement")

 let process_command engine buttons battle text
  (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button =
  let move1, move2, move3, move4, poke1, poke2, poke3, poke4, poke5, switch = match buttons with
 | [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] -> move1, move2, move3, move4, poke1, poke2, poke3, poke4, poke5, switch
 | _ -> failwith "Faulty Game Logic: Debug 982" in
 let battle_buttons = [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch]  in
 match !current_command with
 | (None, None) | (None, Some _) -> failwith "Faulty Game Logic: Debug 03"
 | (Some _, None) -> (match get_game_status battle_status with
                      | Random1p | Preset1p _ | TournBattle _-> List.iter (fun s -> s#misc#hide ()) battle_buttons; current_screen := Battle Processing; text_buffer#set_text "Both moves collected. Processing...";
                                          Ivar.fill !gui_ready !current_command; current_command := (None, None);
                                          upon (Ivar.read !ready_gui) (fun _ -> ready_gui := Ivar.create (); game_animation engine battle_buttons battle text
                                                                        (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button ())
                      | Random2p -> List.iter (fun s -> s#misc#hide ()) battle_buttons; current_screen := Battle (P2 ChooseMove); update_buttons engine move1 move2 move3 move4;
                                    List.iter (fun s -> s#misc#show ()) [move1;move2;move3;move4;switch;back_button]; text_buffer#set_text "Player Two's Chance to Choose a Move.")
 | (Some _, Some _) -> current_screen := Battle Processing; List.iter (fun s -> s#misc#hide ()) battle_buttons; current_screen := Battle Processing; text_buffer#set_text "Both moves collected. Processing...";
                                          Ivar.fill !gui_ready !current_command; current_command := (None, None);
                                          upon (Ivar.read !ready_gui) (fun _ -> ready_gui := Ivar.create (); game_animation engine battle_buttons battle text
                                                                        (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button ())

let poke_move_cmd button engine buttons battle text
  (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button () =
  let move1, move2, move3, move4, poke1, poke2, poke3, poke4, poke5, switch = match buttons with
 | [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] -> move1, move2, move3, move4, poke1, poke2, poke3, poke4, poke5, switch
 | _ -> failwith "Faulty Game Logic: Debug 982" in
  let _ = match !current_command with
 | None, None -> current_command := (Some (UseAttack (button#label)), None)
 | None, Some x -> current_command := (Some (UseAttack (button#label)), Some x)
 | Some x, None -> current_command := Some x, Some (UseAttack button#label)
 | Some _, Some _ -> failwith "Faulty Game Logic: Debug 06" in
 process_command engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text
  (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button

 let switch_poke_cmd button engine buttons battle text
  (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button () =
 let move1, move2, move3, move4, poke1, poke2, poke3, poke4, poke5, switch = match buttons with
 | [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] -> move1, move2, move3, move4, poke1, poke2, poke3, poke4, poke5, switch
 | _ -> failwith "Faulty Game Logic: Debug 982" in
 let next () =
  process_command engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text
  (battle_status, gui_ready, ready, ready_gui) bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button in
 match !current_screen with
  | Battle (P1 BothFaint) ->
        (match get_game_status battle_status with
        | Random1p | Preset1p _ | TournBattle _ -> current_command := (Some (FaintPoke (button#label)), Some (FaintPoke "")); next ()
        (* In two player, you would call switch_poke command again *)
        | Random2p -> text_buffer#set_text "Player Twos' Pokemon has fainted. Choosing...";
                      current_command := (Some (FaintPoke (button#label)), snd !current_command); current_screen := Battle (P2 Faint); switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
                              move3;move4;switch] back_button ())
  | Battle (P1 Faint) -> current_command := (Some (FaintPoke (button#label)), snd !current_command); next ()
  | Battle (P2 Faint) -> current_command := (fst !current_command, Some (FaintPoke (button#label))); next ()
  | Battle (P1 SwitchPoke) -> current_command := (Some (Poke (button#label)), snd !current_command); next ()
  | Battle (P2 SwitchPoke) -> current_command := (fst !current_command, Some (Poke (button#label))); next ()
  | Battle (P1 SwitchPokeF) -> current_command := (Some (Poke (button#label)), snd !current_command); next ()
  | Battle (P2 SwitchPokeF) -> current_command := (fst !current_command, Some (Poke (button#label))); next ()
  | _ -> failwith "Faulty Game Logic: Debug 449"


let quit engine ready () =
  match !current_screen with
  | Battle _ -> engine := Ivar.create (); Thread.delay 0.1; Ivar.fill !engine Quit; Ivar.fill !ready true
  | _ -> Ivar.fill !engine Quit

(* The main gui *)
let main_gui engine battle_engine () =
  let window = GWindow.window ~width: screen_width ~height:(screen_height+24)
    ~title: "Pokemon Snowdown" ~resizable:false () in
  (* menu = menu_holder, main_menu, one_player, two_player, no_player,
    one_player_menu, random_1p, preset_1p, touranment, buffer_area,
    back_button *)
  let battle_status, gui_ready, ready, ready_gui = battle_engine in
  let menu = make_menu ~packing:(window#add) () in
  let menu_holder, main_menu, battle_screen, one_player,
    two_player, no_player, random, preset, touranment,
    back_button, poke_edit, main_menu_bg, load_screen = menu in
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
  ignore(one_player#connect#clicked ~callback:(load_menu engine [random;preset;
  touranment;poke_edit;back_button] [one_player; two_player;no_player]
  main_menu_bg Menu1P));
  (* Two player button *)
  ignore(two_player#connect#clicked ~callback:(load_menu engine [random;preset;
  back_button] [one_player; two_player; no_player] main_menu_bg Menu2P));
  (* No player button *)
  ignore(no_player#connect#clicked ~callback:(load_menu engine [random;preset;
  back_button] [one_player; two_player; no_player] main_menu_bg Menu0P));
 (* Back button *)
   ignore(back_button#connect#clicked
  ~callback:(go_back engine menu battler battle_engine));
  (* Random battle button *)
  ignore(random#connect#clicked ~callback:(load_random engine main_menu_bg bg_img load_screen
  battle text [random;preset;touranment;poke_edit] [move1; move2; move3; move4;
  switch] battle_engine main_menu battle_screen poke1_img poke2_img
  text_buffer health_holders));
  (* Preset battle button *)
  ignore(preset#connect#clicked ~callback:(load_preset engine main_menu_bg bg_img load_screen
  battle text [random;touranment;poke_edit] preset [move1; move2; move3; move4;
  switch] battle_engine main_menu battle_screen poke1_img poke2_img
  text_buffer health_holders));
  (* tourney battle button *)
  ignore(touranment#connect#clicked ~callback:(load_tournament engine main_menu_bg bg_img load_screen
  battle text [random;preset;touranment;poke_edit] [move1; move2; move3; move4;
  switch] battle_engine main_menu battle_screen poke1_img poke2_img
  text_buffer health_holders));
  (* poke edit button *)
  ignore(poke_edit#connect#clicked ~callback:(load_poke_edit engine main_menu_bg
  bg_img load_screen battle text [random;preset;touranment] poke_edit
  [move1; move2; move3; move4; switch] battle_engine main_menu battle_screen
  poke1_img poke2_img text_buffer health_holders));
  (* Switch button *)
  ignore(switch#connect#clicked ~callback:(switch_poke engine [poke1;poke2;poke3;
  poke4;poke5] [move1;move2;move3;move4;switch] back_button));
  (* Pokemon buttons *)
  ignore(poke1#connect#clicked ~callback:(switch_poke_cmd poke1 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button));
  ignore(poke2#connect#clicked ~callback:(switch_poke_cmd poke2 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button));
  ignore(poke3#connect#clicked ~callback:(switch_poke_cmd poke3 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button));
  ignore(poke4#connect#clicked ~callback:(switch_poke_cmd poke4 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button));
  ignore(poke5#connect#clicked ~callback:(switch_poke_cmd poke5 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button));
  (* Move buttons *)
  ignore(move1#connect#clicked ~callback:(poke_move_cmd move1 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button));
  ignore(move2#connect#clicked ~callback:(poke_move_cmd move2 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button));
  ignore(move3#connect#clicked ~callback:(poke_move_cmd move3 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button));
  ignore(move4#connect#clicked ~callback:(poke_move_cmd move4 engine [move1; move2; move3; move4; poke1; poke2; poke3; poke4; poke5; switch] battle text battle_engine bg_img poke1_img poke2_img move_img text_buffer health_holders pokeanim1 pokeanim2 moveanim back_button));
  ignore(window#connect#destroy ~callback:(quit engine ready));
  ignore(window#event#connect#key_press ~callback:(handle_key_press touranment));
  window#show ();
  let _ = GtkThread.start () in ()