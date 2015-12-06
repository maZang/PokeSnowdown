PokemonSnowdown

Chirag Bharadwaj (cb625)
Matthew Zang (msz45)
Nicholas Shieh (ns665)
Young-chan Kim (yk524)

Hi! Welcome to Pokemon Snowdown: The new winter-themed Pokemon game. To compile our game, use the following commands when within the `game` directory of our project.

We hope you enjoy it. :)

COMPILE COMMANDS (bottom one is for executable file):

1. ocamlfind ocamlc -g -thread -package lablgtk2 -package async -package yojson -package str -linkpkg info.mli pokemon.mli gui.mli battle_controller.mli game.mli pokemon.ml ai.ml save.ml tournament.ml gui.ml battle_controller.ml game.ml -o game

OR

cs3110 compile -thread -p lablgtk2,async,yojson,str game.ml

2. ocamlfind ocamlc -g -thread -package lablgtk2 -package async -package yojson -package str -linkpkg info.mli pokemon.mli gui.mli battle_controller.mli game.mli pokemon.ml ai.ml gui.ml battle_controller.ml game.ml -custom -o game.exe
