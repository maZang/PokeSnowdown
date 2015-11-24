open Async.Std
open Info

(*
  Communication Details:

*)

(* Screen width and height parameters *)
let screen_width = 600
let screen_height= 480

(* Initializes GtkMain*)
let locale = GtkMain.Main.init ()

(* Holds similar information to the engine, but different in battle *)
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
*)
let make_battle_screen ?packing () =
  let battle = GPack.table ~rows:4 ~columns: 4 ?packing ~width:screen_width
    ~height: (2 * screen_height / 3) ~show:false
     () in
  let text = GPack.hbox ?packing ~height: (1 * screen_height / 6) () in
  let bg_img = GMisc.image ~file:"../data/backgrounds/bg-volcanocave.jpg"
     () in
  let text_buffer = GEdit.entry ~width:600 ~height:80 ~text:"Chirag's Mom"
    ~packing:(text#pack ~expand:true) ~editable:false () in
  let poke1_img = GMisc.image ~file:"../data/back-sprites/charizard-mega-x.gif" () in
  let poke2_img = GMisc.image ~file:"../data/sprites/blaziken-mega.gif" () in
  let move1 = GButton.button ~label:"Move1" ~show:false () in
  let move2 = GButton.button ~label:"Move2" ~show:false () in
  let move3 = GButton.button ~label:"Move3" ~show:false () in
  let move4 = GButton.button ~label:"Move4" ~show:false () in
  let switch = GButton.button ~label:"Switch" ~show: false() in
  let poke1 = GButton.button ~label:"Poke1" ~show:false () in
  let poke2 = GButton.button ~label:"Poke2" ~show:false () in
  let poke3 = GButton.button ~label:"Poke3" ~show:false () in
  let poke4 = GButton.button ~label:"Poke4" ~show:false () in
  let poke5 = GButton.button ~label:"Poke5" ~show:false () in
  text_buffer#misc#modify_font (Pango.Font.from_string "arial,monospace condensed 10");
  battle#attach ~left:0 ~top:1 ~bottom:4 poke1_img#coerce;
  battle#attach ~left:3 ~top:0 ~bottom:3 poke2_img#coerce;
  battle#attach ~left:0 ~top:0 ~right:4 ~bottom:4 ~fill:`BOTH bg_img#coerce;
  battle, text, bg_img, move1, move2, move3, move4, switch, poke1_img, poke2_img
  , text_buffer, poke1, poke2, poke3, poke4, poke5

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
	let vbox = GPack.vbox ?packing () in
	let hbox1 = GPack.hbox ~homogeneous:true ~packing:(vbox#pack) ~height:
    (screen_height/6) ()in
	let hbox2 = GPack.vbox ~packing:(vbox#pack) () in
	let button1 = GButton.button ~label:"1-Player"
		~packing:(hbox1#pack ~expand:true ~fill:true) () in
	let button2 = GButton.button ~label:"2-Player"
		~packing:(hbox1#pack ~expand:true ~fill:true) () in
	let button3 = GButton.button ~label:"No Player"
		~packing:(hbox1#pack ~expand:true ~fill:true) ()  in
	let button4 = GButton.button ~label:"Random Battle"
		~packing:(hbox1#pack ~expand:true ~fill:true) ~show:false () in
	let button5 = GButton.button ~label:"Preset Battle"
		~packing:(hbox1#pack ~expand:true ~fill:true) ~show:false () in
	let button6 = GButton.button ~label:"Tournament"
		~packing:(hbox1#pack ~expand:true ~fill:true) ~show:false () in
	let button7 = GButton.button ~label:"Back"
		~packing:(hbox1#pack ~from:`END) () ~show:false in
  let img = GMisc.image ~file:"./gui_pics/main.gif" ~packing:(hbox2#pack)
    ~width:screen_width ~height:(5*screen_height /6) () in
  let img2 = GMisc.image ~file:"./gui_pics/1p.jpg" ~packing:(hbox2#pack)
    ~width:screen_width ~height:(5*screen_height /6) ~show:false () in
  let load_screen = GMisc.image ~file:"../data/backgrounds/background.gif" ~show:false
    ~packing:(vbox#pack) () in
  (vbox, hbox1, hbox2, button1, button2, button3, button4,
		button5, button6, button7, img, img2, load_screen)

(* This is the main game loop for the battle. The game states will be
  engine -- Battle InGame _
  current_screen -- Battle P1 _ || Battle P2 _
  current_command -- Some _, Some _
*)
let load_battle_screen engine img battle text buttonhide buttonshow
  (battle_status, gui_ready) poke1_img poke2_img text_buffer () =
  let team1, team2, weather = match get_game_status engine with
  | Battle InGame (t1, t2, w) -> (t1, t2, w)
  | _ -> failwith "Impossible" in
  let poke1 = (team1.current).pokeinfo.name in
  let poke2 = (team2.current).pokeinfo.name in
  img#misc#hide (); battle#misc#show (); text#misc#show ();
  List.iter (fun s -> s#misc#hide ()) buttonhide;
  List.iter (fun s -> s#misc#show ()) buttonshow; current_screen := Battle (P1 ChooseMove);
  poke1_img#set_file ("../data/back-sprites/" ^ poke2 ^ ".gif");
  poke2_img#set_file ("../data/sprites/" ^ poke1 ^ ".gif");
  gui_ready := Ivar.create ()

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
  (battle_status, gui_ready, ready) mode main_menu battle_screen poke1_img poke2_img
  text_buffer () =
  if (Ivar.is_empty (!engine)) then
    ((main_menu#misc#hide (); battle_screen#misc#hide (); load_screen#misc#show ();
      current_screen :=
    (Battle Loading); Ivar.fill !engine (Battle Loading); Printf.printf "Initializing gui\n%!";
    Ivar.fill !battle_status mode); let rec load_helper () =
    upon (Ivar.read !engine) (fun s -> match s with | Battle InGame _ ->
    (main_menu#misc#show (); battle_screen#misc#show ();
    load_screen#misc#hide ();load_battle_screen engine img battle text buttonhide buttonshow
    (battle_status, gui_ready) poke1_img poke2_img text_buffer()) | _ -> load_helper ()) in load_helper())
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
 main_menu_bg battle text (battle_status, gui_ready, ready) () =
 if (match Deferred.peek (Ivar.read (!engine)) with
      | Some Battle InGame _ -> true
      | _ -> false) then
  (engine := Ivar.create (); Thread.delay 0.1;
  List.iter (fun s -> s#misc#hide ()) button_hide;
  List.iter (fun s -> s#misc#show ())[one_player; two_player; no_player];
  battle#misc#hide (); text#misc#hide (); main_menu_bg#misc#show ();
  current_screen := MainMenu; Ivar.fill !engine MainMenu; Ivar.fill !ready true;
  battle_status := Ivar.create (); gui_ready := Ivar.create ();
  current_command := (None, None))
else
  ()

let go_back engine (menu_holder, main_menu, battle_scren, one_player,
    two_player, no_player, random_1p, preset_1p, touranment,
    back_button, main_menu_bg, one_bg, load_screen) (battle, text, bg_img, move1, move2,
    move3, move4, switch, poke1_img, poke2_img, text_buffer, poke1, poke2,
    poke3, poke4, poke5) battle_engine () =
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
      current_screen := Battle (P2 ChooseMove))

let switch_poke engine [poke1;poke2;poke3;poke4;poke5] [move1;move2;
  move3;move4;switch] back () =
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
  | Battle (InGame (t1, t2, w)) ->
      match !current_screen with
      | Battle (P1 _) -> current_screen := Battle (P1 SwitchPoke); t1
      | Battle (P2 _) -> current_screen := Battle (P2 SwitchPoke); t2
      | _ -> failwith "Faulty Game logic"
  | _ -> failwith "Fauly game Logic") in
  List.iteri switch_poke_helper team.alive

 let switch_poke_cmd poke1 gui_ready () =
 match !current_command with
 | None, None -> current_command := (Some (Poke (poke1#label)), None)
 | Some x, None -> current_command := Some x, Some (Poke poke1#label);
                  Ivar.fill !gui_ready !current_command
 | Some _, Some _ -> failwith "Should not happen"

(* The main gui *)
let main_gui engine battle_engine () =
	let window = GWindow.window ~width: screen_width ~height: screen_height
		~title: "Pokemon Snowdown" ~resizable:false () in
	(* menu = menu_holder, main_menu, one_player, two_player, no_player,
		one_player_menu, random_1p, preset_1p, touranment, buffer_area,
		back_button *)
  let battle_status, gui_ready, ready = battle_engine in
	let menu = make_menu ~packing:(window#add) () in
	let menu_holder, main_menu, battle_screen, one_player,
		two_player, no_player, random_1p, preset_1p, touranment,
		back_button, main_menu_bg, one_bg, load_screen = menu in
  let battler = make_battle_screen ~packing:(battle_screen#add) ()
  in let battle, text, bg_img, move1, move2, move3, move4, switch,
    poke1_img, poke2_img, text_buffer, poke1, poke2, poke3, poke4,
    poke5 = battler in
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
  text_buffer);
  (* Switch button *)
  switch#connect#clicked ~callback:(switch_poke engine [poke1;poke2;poke3;
  poke4;poke5] [move1;move2;move3;move4;switch] back_button);
  (* Pokemon buttons *)
  poke1#connect#clicked ~callback:(switch_poke_cmd poke1 gui_ready);
	window#show ();
	let thread = GtkThread.start () in ()