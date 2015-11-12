open GMain
open Async.Std 

let screen_width = 642
let screen_height= 480

let quit engine () = 
	if Ivar.is_empty (!engine) then 
		(Main.quit; Ivar.fill !engine Info.Quit)
	else 
		() 
(* Make all the menu items for the game loading screen *)
let make_menu ~file1 ~file2?packing () = 
	let hbox = GPack.hbox ~homogeneous:true ?packing () in 
	let vbox1 = GPack.vbox ~packing:(hbox#pack) () in 
	let vbox2 = GPack.vbox ~packing:(hbox#pack) () in 
	let vbox3 = GPack.vbox ~packing:(hbox#pack) () in 
	let _ = GMisc.image ~file:file1 ~packing:(vbox1#pack) () in
	let _ = GMisc.image ~file:file2 ~packing:(vbox3#pack) () in  
	let button1 = GButton.button ~label:"1-Player"
		~packing:(vbox2#pack ~expand:true ~fill:true) () in 
	let button2 = GButton.button ~label:"2-Player"
		~packing:(vbox2#pack ~expand:true ~fill:true) () in 
	let button3 = GButton.button ~label:"No Player"
		~packing:(vbox2#pack ~expand:true ~fill:true) ()  in 
	let vbox4 = GPack.vbox () in 
	let button4 = GButton.button ~label:"Random Battle"
		~packing:(vbox4#pack ~expand:true ~fill:true) ()  in 
	let button5 = GButton.button ~label:"Preset Battle"
		~packing:(vbox4#pack ~expand:true ~fill:true) ()  in 
	let button6 = GButton.button ~label:"Tournament" 
		~packing:(vbox4#pack ~expand:true ~fill:true) ()  in 
	hbox, vbox2, button1, button2, button3, vbox4, button4,
		button5, button6, vbox3 

let load_1p_menu engine menu_holder buffer menu1 menu2 () = 
	menu_holder#remove buffer#coerce;
	menu_holder#remove menu1#coerce;
	menu_holder#add menu2#coerce;
	menu_holder#add buffer#coerce 

(* The main gui *)
let main_gui engine () = 

	let window = GWindow.window ~width: screen_width ~height: screen_height
		~title: "Pokemon Snowdown" ~resizable:false () in 
	let menu_holder, main_menu, one_player, two_player, no_player, 
		one_player_menu, random_1p, preset_1p, tournament,
		buffer_area = make_menu 
		~file1:"./gui_pics/bg1.png" ~file2:"./gui_pics/bg2.png" 
		~packing:(window#add) () in 
	one_player#connect#clicked ~callback:(load_1p_menu engine menu_holder 
		buffer_area main_menu one_player_menu);
	window#connect#destroy 	~callback: (quit engine);
	window#show ();
	Main.main ()
