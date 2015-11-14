open Async.Std

let screen_width = 642
let screen_height= 480

let locale = GtkMain.Main.init ()

let current_screen = ref Info.MainMenu

let make_battle_screen ~file ?packing () =

(* Make all the menu items for the game loading screen *)
let make_menu ~file1 ~file2?packing () =
	let hbox = GPack.hbox ~homogeneous:true ?packing () in
	let vbox1 = GPack.vbox ~packing:(hbox#pack) () in
	let vbox2 = GPack.vbox ~packing:(hbox#pack) () in
	let vbox3 = GPack.vbox ~packing:(hbox#pack) () in
	let _ = GMisc.image ~file:file1 ~packing:(vbox1#pack) () in
	let _ = GMisc.image ~file:file2 ~height:460
		~packing:(vbox3#pack ~expand:true) () in
	let button1 = GButton.button ~label:"1-Player"
		~packing:(vbox2#pack ~expand:true ~fill:true) () in
	let button2 = GButton.button ~label:"2-Player"
		~packing:(vbox2#pack ~expand:true ~fill:true) () in
	let button3 = GButton.button ~label:"No Player"
		~packing:(vbox2#pack ~expand:true ~fill:true) ()  in
	let button4 = GButton.button ~label:"Random Battle"
		~packing:(vbox2#pack ~expand:true ~fill:true) ~show:false () in
	let button5 = GButton.button ~label:"Preset Battle"
		~packing:(vbox2#pack ~expand:true ~fill:true) ~show:false () in
	let button6 = GButton.button ~label:"Tournament"
		~packing:(vbox2#pack ~expand:true ~fill:true) ~show:false () in
	let button7 = GButton.button ~label:"Back"
		~packing:(vbox3#pack ~from:`END) () ~show:false in
  hbox, vbox1, vbox2, vbox3, button1, button2, button3, button4,
		button5, button6, button7

let load_not_main_menu engine button1 button2 button3 button4 button5 button6
  back_button() =
	if Ivar.is_empty (!engine) then
		(List.iter (fun s -> s#misc#hide ())[button1; button2; button3];
    List.iter (fun s -> s#misc#show ()) [button4;button5;button6;
    back_button]; current_screen := Info.Menu1P; Ivar.fill !engine Info.Menu1P)
	else
		()

let load_main_menu engine one_player two_player no_player button4 button5
  button6 back_button () =
	if Ivar.is_empty (!engine) then
		(List.iter (fun s -> s#misc#hide ()) [button4;button5;button6;
      back_button];
    List.iter (fun s -> s#misc#show ())[one_player; two_player; no_player];
		current_screen := Info.MainMenu; Ivar.fill !engine Info.MainMenu)
	else
		()

let go_back engine (menu_holder, left_side, main_menu, right_side, one_player,
    two_player, no_player, random_1p, preset_1p, touranment,
    back_button) () =
	if !current_screen = Info.Menu1P then
		load_main_menu engine one_player two_player no_player random_1p preset_1p
      touranment back_button ()


(* The main gui *)
let main_gui engine () =
	let window = GWindow.window ~width: screen_width ~height: screen_height
		~title: "Pokemon Snowdown" ~resizable:false () in
	(* menu = menu_holder, main_menu, one_player, two_player, no_player,
		one_player_menu, random_1p, preset_1p, touranment, buffer_area,
		back_button *)
	let menu = make_menu
		~file1:"./gui_pics/bg1.png" ~file2:"./gui_pics/bg2.png"
		~packing:(window#add) () in
	let menu_holder, left_side, main_menu, right_side, one_player,
		two_player, no_player, random_1p, preset_1p, touranment,
		back_button = menu in
	one_player#connect#clicked ~callback:(load_not_main_menu engine one_player
  two_player no_player random_1p preset_1p touranment back_button);
	back_button#connect#clicked ~callback:(go_back engine menu);
	window#show ();
	let thread = GtkThread.start () in ()