(* Info contains all the data types needed for the handling of the game *)

type game_state = MainMenu | Menu1P | Quit 

type gamemove = SwitchPoke of int | UseAttack of int 

type cmd = Nothing | GameMove of gamemove