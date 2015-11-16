open Async.Std
open Info

(*
  Communication Details:

*)


let screen_width = 600
let screen_height= 480

let locale = GtkMain.Main.init ()

let current_screen = ref MainMenu

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
  text_buffer#misc#modify_font (Pango.Font.from_string "arial,monospace condensed 10");
  battle#attach ~left:0 ~top:1 ~bottom:4 poke1_img#coerce;
  battle#attach ~left:3 ~top:0 ~bottom:3 poke2_img#coerce;
  battle#attach ~left:0 ~top:0 ~right:4 ~bottom:4 ~fill:`BOTH bg_img#coerce;
  battle, text, bg_img, move1, move2, move3, move4, switch

(* Make all the menu items for the game loading screen *)
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

let load_battle_screen engine img battle text buttonhide buttonshow
  (battle_status, gui_ready) screen () =
  img#misc#hide (); battle#misc#show (); text#misc#show ();
  List.iter (fun s -> s#misc#hide ()) buttonhide;
  List.iter (fun s -> s#misc#show ()) buttonshow; current_screen := screen;
  gui_ready := Ivar.create ()

let load_battle_load engine img load_screen battle text buttonhide buttonshow
  (battle_status, gui_ready) mode main_menu battle_screen() =
  if (Ivar.is_empty (!engine)) then
    ((main_menu#misc#hide (); battle_screen#misc#hide (); load_screen#misc#show ();
      current_screen :=
    (Battle Loading); Ivar.fill !engine (Battle Loading);
    Ivar.fill !battle_status mode); let rec load_helper () =
    upon (Ivar.read !engine) (fun s -> match s with | Battle InGame _ ->
    (load_screen#misc#hide (); load_battle_screen engine img battle text buttonhide buttonshow
    (battle_status, gui_ready) s (); main_menu#misc#show ();
    battle_screen#misc#show ()) | _ -> load_helper ()) in load_helper())
  else
    ()

let load_menu engine button_show button_hide img1 img2 () =
	if Ivar.is_empty (!engine) then
		(List.iter (fun s -> s#misc#hide ()) button_hide;
    List.iter (fun s -> s#misc#show ()) button_show;
    img1#misc#hide (); img2#misc#show ();
    current_screen := Menu1P; Ivar.fill !engine Menu1P)
	else
		()

let load_main_menu_from_battle engine one_player two_player no_player button_hide
 main_menu_bg battle text (battle_status, gui_ready) () =
 if (match Deferred.peek (Ivar.read (!engine)) with
      | Some Battle InGame _ -> true
      | _ -> false) then
  (engine := Ivar.create (); Thread.delay 0.5;
  List.iter (fun s -> s#misc#hide ()) button_hide;
  List.iter (fun s -> s#misc#show ())[one_player; two_player; no_player];
  battle#misc#hide (); text#misc#hide (); main_menu_bg#misc#show ();
  current_screen := MainMenu; Ivar.fill !engine MainMenu;
  battle_status := Ivar.create (); gui_ready := Ivar.create ())
else
  ()

let go_back engine (menu_holder, main_menu, battle_scren, one_player,
    two_player, no_player, random_1p, preset_1p, touranment,
    back_button, main_menu_bg, one_bg, load_screen) (battle, text, bg_img, move1, move2,
    move3, move4, switch) battle_engine () =
	(if !current_screen = Menu1P then
		load_menu engine [one_player;two_player;no_player]
    [random_1p; preset_1p ;touranment; back_button] one_bg main_menu_bg());
  if (match !current_screen with
    | Battle InGame _ -> true
    | _ -> false ) then
    (load_main_menu_from_battle engine one_player two_player no_player [move1;
    move2; move3; move4; switch; back_button] main_menu_bg battle text
    battle_engine ())


(* The main gui *)
let main_gui engine battle_engine () =
	let window = GWindow.window ~width: screen_width ~height: screen_height
		~title: "Pokemon Snowdown" ~resizable:false () in
	(* menu = menu_holder, main_menu, one_player, two_player, no_player,
		one_player_menu, random_1p, preset_1p, touranment, buffer_area,
		back_button *)
	let menu = make_menu ~packing:(window#add) () in
	let menu_holder, main_menu, battle_screen, one_player,
		two_player, no_player, random_1p, preset_1p, touranment,
		back_button, main_menu_bg, one_bg, load_screen = menu in
  let battler = make_battle_screen ~packing:(battle_screen#add) ()
  in let battle, text, bg_img, move1, move2, move3, move4, switch = battler in
  main_menu#pack move1#coerce; main_menu#pack move2#coerce;
  main_menu#pack move3#coerce; main_menu#pack move4#coerce;
  main_menu#pack switch#coerce;
  (* One player Button *)
	one_player#connect#clicked ~callback:(load_menu engine [random_1p;preset_1p;
  touranment;back_button] [one_player; two_player;no_player] main_menu_bg one_bg);
 (* Back button *)
   back_button#connect#clicked
  ~callback:(go_back engine menu battler battle_engine);
  (* Random 1p battle butotn *)
  random_1p#connect#clicked ~callback:(load_battle_load engine one_bg load_screen
  battle text [random_1p;preset_1p;touranment] [move1; move2; move3; move4;
  switch] battle_engine Random1p main_menu battle_screen);
	window#show ();
	let thread = GtkThread.start () in ()