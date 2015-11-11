open GMain

let main_gui () = 
	let window = GWindow.window ~width: 640 ~height: 480 
		~title: "Pokemon Snowdown" () in 
	window#show ();
	Main.main ()

let () = main_gui () 