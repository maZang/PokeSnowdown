##Week 3 Assignments - Pokemon Snowdown

Jacob - 

Add 5 NPCs

Make all Pokémon unlockable by editing the npc json files

- edit each npc jason file, change this line 

"unlockable": ["umbreon", "houndoom", "tyranitar", "mightyena", "shiftry", "sableye", "sharpedo", "cacturne", "crawdaunt", "absol", "honchkrow", "skuntank", "spiritomb", "drapion", "weavile", "liepard", "krookodile", "scrafty", "zoroark", "bisharp", "mandibuzz", "hydreigon", "greninja", "pangoro", "malamar", "venusaur", "beedrill", "arbok", "nidoqueen", "nidoking", "vileplume", "venomoth", "victreebel", "tentacruel", "muk", "gengar", "weezing", "ariados", "crobat", "qwilfish", "dustox", "swalot", "seviper", "roserade", "skuntank", "drapion", "toxicroak", "scolipede", "garbodor", "amoonguss", "dragalge"].

The unlockable pokemon should go in order of their pokedex entries.

Trainer 1 in the list ( check tournamnet.mil) is 0 -> RoughNeck which corresponds to baldman.json

the contents of the unlock able field should be the 0 - x Pokémon going by their pokedex entry order, where x is the total number of Pokémon divided by the total number of trainers ( we are aiming for 30 trainers total so x is around 24 but the last person may have extra Pokémon, which is fine).
so the correct baldman.json is

  "unlockable": ["bulbasaur", "ivysaur", "venusaur", "mightyena", "charmander", "charmeleon", "charizard", "squirtle", "wartortle", "blastoise", "caterpie", "metapod", "butterfree", "weedle", "kakuna", "beedrill", "pidgey", "pidgeotto", "pidgeot", "rattata", "raticate", "spearow", "fearow", "ekans", "arkbok"]

trainer 2 is 1 -> Beauty which corresponds to beauty.json etc.

Make sure all the Pokémon names are spelled correctly and are lowercase.


Alvin/matt-

Add 5 NPCs

Add textbox containing trainer names -

![lindsey](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/Weekly-Assignments/Lindsey.jpg)

you will need to look at the file gui.ml, specifically the part where it registers the key stroke H and reads the trainerquotes string.  Just display this textbox that will read in the trainer name based on the enemy string from the tournamnet.mli file.

A psedocode example

if key_down = h

	if name = RoughNeck
	
		texbox.show "Rough Neck" at location (above the quote string textbox)
		
	execute normal code here
	
Adeet -

Add 5 NPCs

Work on Adding items and randomizing moves weighted towards the Pokémon types.
