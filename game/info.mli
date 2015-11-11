(* Info contains all the data types needed for the handling of the game *)

type cmd = Nothing | GameMove of gamemove

type gamemove = SwitchPoke of int | UseAttack of int 