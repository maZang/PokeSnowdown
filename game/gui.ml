open Async.Std

let screen_width = 800
let screen_height= 480

let locale = GtkMain.Main.init ()

let current_screen = ref Info.MainMenu

let make_battle_screen ?packing () = ()

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
  (vbox, hbox1, hbox2, button1, button2, button3, button4,
		button5, button6, button7, img, img2)

let load_not_main_menu engine button1 button2 button3 button4 button5 button6
  back_button img1 img2 () =
	if Ivar.is_empty (!engine) then
		(List.iter (fun s -> s#misc#hide ())[button1; button2; button3];
    List.iter (fun s -> s#misc#show ()) [button4;button5;button6;
    back_button]; img1#misc#hide (); img2#misc#show ();
    current_screen := Info.Menu1P; Ivar.fill !engine Info.Menu1P)
	else
		()

let load_main_menu engine one_player two_player no_player button4 button5
  button6 back_button main_menu_bg img2 () =
	if Ivar.is_empty (!engine) then
		(List.iter (fun s -> s#misc#hide ()) [button4;button5;button6;
      back_button];
    List.iter (fun s -> s#misc#show ())[one_player; two_player; no_player];
    img2#misc#hide (); main_menu_bg#misc#show ();
		current_screen := Info.MainMenu; Ivar.fill !engine Info.MainMenu)
	else
		()

let go_back engine (menu_holder, main_menu, battle_scren, one_player,
    two_player, no_player, random_1p, preset_1p, touranment,
    back_button, main_menu_bg, one_bg) () =
	if !current_screen = Info.Menu1P then
		load_main_menu engine one_player two_player no_player random_1p preset_1p
      touranment back_button main_menu_bg one_bg ()


(* The main gui *)
let main_gui engine () =
	let window = GWindow.window ~width: screen_width ~height: screen_height
		~title: "Pokemon Snowdown" ~resizable:false () in
	(* menu = menu_holder, main_menu, one_player, two_player, no_player,
		one_player_menu, random_1p, preset_1p, touranment, buffer_area,
		back_button *)
	let menu = make_menu ~packing:(window#add) () in
	let menu_holder, main_menu, battle_screne, one_player,
		two_player, no_player, random_1p, preset_1p, touranment,
		back_button, main_menu_bg, one_bg = menu in
	one_player#connect#clicked ~callback:(load_not_main_menu engine one_player
  two_player no_player random_1p preset_1p touranment back_button main_menu_bg
  one_bg); back_button#connect#clicked ~callback:(go_back engine menu);
	window#show ();
	let thread = GtkThread.start () in ()