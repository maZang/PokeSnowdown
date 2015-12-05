import json 
import csv 

NUM_ABILITIES = 188
NUM_MOVES = 621

def generate_flag_dictionary ():
	flagdict = {}
	csvfile = open("../move_flags.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("id", "identifier")
	reader =  csv.DictReader(csvfile, fieldnames)
	for row in reader:
		flagdict[row["id"]] = row["identifier"]
	return flagdict 

def generate_move_dictionary ():
	movedict = {}
	csvfile = open("../moves.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("id", "identifier", "generation_id", "type_id", "power", "pp",
		"accuracy", "priority", "target_id", "damage_class_id", "effect_id", 
		"effect_chance", "contest_type_id", "contest_effect_id", 
		"super_contest_effect_id")
	reader = csv.DictReader(csvfile, fieldnames)
	for row in reader:
		movedict[row["id"]] = row["identifier"]
	return movedict

def get_stat_dictionary ():
	statdict = {}
	csvfile = open("../stats.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("id", "damage_class_id","identifier", "is_battle_only", "game_index")
	reader = csv.DictReader(csvfile, fieldnames)
	for row in reader:
		statdict[row["id"]] = row["identifier"]
	return statdict

def generate_ability_dictionary ():
	ability_dict = {} 
	csvfile = open("../abilities.csv", "r")
	jsonfile = open("../abilities.json", "w")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("id", "identifier", "generation_id", "is_main_series")
	reader = csv.DictReader(csvfile, fieldnames)
	for row in reader: 
		ability_dict[row['id']] = row["identifier"]
	return ability_dict 

def generate_move_target_dictionary ():
	movedmgdict = {}
	csvfile = open("../move_targets.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("id", "identifier")
	reader = csv.DictReader(csvfile, fieldnames)
	for row in reader:
		movedmgdict[row["id"]] = row["identifier"]
	return movedmgdict

def generate_move_dmg_dictionary ():
	movedmgdict = {}
	csvfile = open("../move_damage_classes.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("id", "identifier")
	reader = csv.DictReader(csvfile, fieldnames)
	for row in reader:
		movedmgdict[row["id"]] = row["identifier"]
	return movedmgdict

def generate_type_dictionary ():
	typedict = {}
	csvfile = open("../types.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("id", "identifier", "generation_id", "damage_class_id")
	reader = csv.DictReader(csvfile, fieldnames)
	for row in reader:
		typedict[row["id"]] = row["identifier"]
	return typedict 

def generate_pokemon_json ():
	#ability dict
	ability_dict = generate_ability_dictionary () 
	#pokemon ability dict
	pokemon_ability = {}
	csvfile = open("../pokemon_abilities.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("pokemon_id", "ability_id", "is_hidden", "slot")
	reader = csv.DictReader(csvfile, fieldnames) 
	for row in reader:
		abil_list = [] 
		if (row["pokemon_id"] in pokemon_ability):
			abil_list = pokemon_ability[row["pokemon_id"]]
		abil_list.append(ability_dict[row["ability_id"]])
		pokemon_ability[row["pokemon_id"]] = abil_list 
	#stat dict
	statdict = get_stat_dictionary () 
	#pokemon stat dict
	pokemon_stat = {} 
	csvfile = open("../pokemon_stats.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("pokemon_id", "stat_id", "base_stat", "effort")
	reader = csv.DictReader(csvfile, fieldnames) 
	for row in reader:
		stat_dict = {}
		if (row["pokemon_id"] in pokemon_stat):
			stat_dict = pokemon_stat[row["pokemon_id"]]
		stat_dict[statdict[row["stat_id"]]] = row["base_stat"]
		pokemon_stat[row["pokemon_id"]] = stat_dict 
	#pokemon type dict 
	type_dict = generate_type_dictionary () 
	pokemon_type = {}
	csvfile = open("../pokemon_types.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("pokemon_id", "type_id", "slot")
	reader = csv.DictReader(csvfile, fieldnames) 
	for row in reader:
		type_list = [] 
		if (row["pokemon_id"] in pokemon_type):
			type_list = pokemon_type[row["pokemon_id"]]
		type_list.append(type_dict[row["type_id"]])
		pokemon_type[row["pokemon_id"]] = type_list 
	#pokemon move dict
	move_dict = generate_move_dictionary () 
	pokemon_move = {} 
	csvfile = open("../pokemon_moves.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("pokemon_id", "version_group_id", "move_id", "pokemon_move_method", "level", "order")
	reader = csv.DictReader(csvfile, fieldnames) 
	for row in reader:
		move_list = [] 
		if (row["pokemon_id"] in pokemon_move):
			move_list = pokemon_move[row["pokemon_id"]]
		if move_dict[row["move_id"]] not in move_list:
			move_list.append(move_dict[row["move_id"]])
			pokemon_move[row["pokemon_id"]] = move_list
	#fix move_dict to include pre-evolution moves
	csvfile = open("../pokemon_species.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("id", "identifier", "gen_id", "evovles_from_id", "evo_chain", "color_id", "shape_id", "habitat_id", "gender_rate", "capture_rate", "base_happiness")
	reader = csv.DictReader(csvfile, fieldnames)
	for row in reader:
		if (row['evovles_from_id'] != ''):
			pokemon_move[row["id"]] = list(set(pokemon_move[row["id"]]) | set (pokemon_move[row["evovles_from_id"]]))
	#final pokemon dict 
	pokemon = {} 
	csvfile = open("../pokemon.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("id", "identifier", "species_id", "height", "weight", "base_experience"
		, "order", "is_default")
	reader = csv.DictReader(csvfile, fieldnames) 
	pokearr = {}
	for row in reader:
		if int(row["id"]) >= 10080 and int(row["id"]) <= 10085:
			continue 
		pokedict = {} 
		pokedict["type"] = pokemon_type[row["id"]]
		pokedict["ability"] = pokemon_ability[row["id"]]
		pokedict["stats"] = pokemon_stat[row["id"]]
		pokedict["moves"] = pokemon_move[row["id"]]
		pokey = row["identifier"]
		if pokey == "mime-jr":
			pokey = "mimejr" 
		if pokey == "ho-oh":
			pokey = "hooh"
		if pokey == "gourgeist-average":
			pokey = "gourgeist"
		if pokey == "darmanitan-standard":
			pokey = "darmanitan"
		if pokey == "meowstic-male":
			pokey = "meowstic"
		if pokey == "wormadam-plant":
			pokey = "wormadam"
		if pokey == "nidoran-m":
			pokey = "nidoranm"
		if pokey == "nidoran-f":
			pokey = "nidoranf"
		if pokey == "meloetta-aria":
			pokey = "meloetta"
		if pokey == "shaymin-land":
			pokey = "shaymin"
		if pokey == "tornadus-incarnate":
			pokey = "tornadus"
		if pokey == "basculin-blue-striped":
			pokey = "basculin-bluestriped"
		if pokey == "porygon-z":
			pokey = "porygonz"
		if pokey == "mr-mime":
			pokey = "mrmime"
		if pokey == "thundurus-incarnate":
			pokey = "thundurus"
		if pokey == "landorus-incarnate":
			pokey = "landorus"
		if pokey == "pumpkaboo-average":
			pokey = "pumpkaboo"
		if pokey == "floette-eternal":
			pokey = "floette-eternalflower"
		if pokey == "giratina-altered":
			pokey = "giratina-origin"
		if pokey == "deoxys-normal":
			pokey = "deoxys"
		if pokey == "aegislash-shield":
			pokey = "aegislash"
		if pokey == "basculin-red-striped":
			pokey = "basculin"
		if pokey == "keldeo-ordinary":
			pokey = "keldeo"
		if pokey == "meowstic-female":
			pokey = "meowstic-f"
		pokemon[pokey] = pokedict
		pokearr[row["id"]] = pokey 
	with open("../pokemon.json", "w") as outfile:
		json.dump(pokemon, outfile)
	with open("../pokemonlist.json", "w") as outfile:
		json.dump(pokearr, outfile)


def generate_moves_json ():
	move_list = {} 
	effect_list = {} 
	flags = {}
	flag_dict = generate_flag_dictionary () 
	csvfile = open("../move_flag_map.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("move_id", "flag")
	reader = csv.DictReader(csvfile, fieldnames)
	for row in reader:
		flag_list = []
		if (row["move_id"] in flags):
			flag_list = flags[row["move_id"]]
		flag_list.append(flag_dict[row["flag"]])
		flags[row["move_id"]] = flag_list
	typedict = generate_type_dictionary () 
	movedmgdict = generate_move_dmg_dictionary () 
	movetarget = generate_move_target_dictionary () 
	csvfile1 = open("../move_effect_prose.csv")
	csvfile1.seek(0)
	next(csvfile1)
	fieldnames1 = ("move_effect_id", "local_language_id", "short_effect", "effect")
	reader1 = csv.DictReader(csvfile1, fieldnames1)
	for row in reader1:
		effect_list[row["move_effect_id"]] = row["short_effect"]
	csvfile2 = open("../moves.csv")
	csvfile2.seek(0)
	next(csvfile2)
	fieldnames2 = ("id", "identifier", "generation_id", "type_id", "power", "pp",
		"accuracy", "priority", "target_id", "damage_class_id", "effect_id", 
		"effect_chance", "contest_type_id", "contest_effect_id", 
		"super_contest_effect_id")
	reader2 = csv.DictReader(csvfile2, fieldnames2)
	for row in reader2:
		move_dict = {}
		move_dict["type"] = typedict[row["type_id"]]
		move_dict["power"] = row["power"]
		move_dict["pp"] = row["pp"]
		move_dict["accuracy"] = row["accuracy"]
		move_dict["priority"] = row["priority"]
		move_dict["target"] = movetarget[row["target_id"]]
		move_dict["dmg_class"] = movedmgdict[row["damage_class_id"]]
		move_dict["effect"] = effect_list[row["effect_id"]]
		if row["id"] in flags: 
			move_dict["flags"] = flags[row["id"]]
		move_dict["effect_chance"] = row["effect_chance"]
		move_list[row["identifier"]] = move_dict 
	with open("../moves.json", "w") as outfile:
		json.dump(move_list, outfile)


def generate_abilities_json ():
	ability_list = {} 
	csvfile = open("../ability_prose.csv", "r")
	jsonfile = open("../abilities.json", "w")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("ability_id", "local_language_id", "short_effect", "effect")
	reader = csv.DictReader(csvfile, fieldnames)
	for row in reader: 
		if int(row["ability_id"]) > NUM_ABILITIES:
			break 
		ability_list[row['ability_id']] = row["short_effect"]
	csvfile2 = open("../abilities.csv", "r")
	csvfile2.seek(0)
	next(csvfile2)
	fieldnames2 = ("id", "identifier", "generation_id", "is_main_series")
	reader2 = csv.DictReader(csvfile2, fieldnames2)
	for row in reader2:
		if int(row["id"]) > NUM_ABILITIES:
			break 
		ability_list[row["identifier"]] = ability_list[row["id"]]
		del ability_list[row["id"]]
	with open("../abilities.json", "w") as outfile:
		json.dump(ability_list, outfile)

def generate_unlock_json(): 
	#pokemon type dict 
	type_dict = generate_type_dictionary () 
	pokemon_type = {}
	csvfile = open("../pokemon_types.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("pokemon_id", "type_id", "slot")
	reader = csv.DictReader(csvfile, fieldnames) 
	for row in reader:
		type_list = [] 
		if (row["pokemon_id"] in pokemon_type):
			type_list = pokemon_type[row["pokemon_id"]]
		type_list.append(type_dict[row["type_id"]])
		pokemon_type[row["pokemon_id"]] = type_list 
	#fix move_dict to include pre-evolution moves
	csvfile = open("../pokemon_species.csv")
	csvfile.seek(0)
	next(csvfile)
	fieldnames = ("id", "identifier", "gen_id", "evovles_from_id", "evo_chain", "color_id", "shape_id", "habitat_id", "gender_rate", "capture_rate", "base_happiness")
	reader = csv.DictReader(csvfile, fieldnames)
	list_evolves_from = [] 
	for row in reader:
		if (row['evovles_from_id'] != ''):
			list_evolves_from.append (row["evovles_from_id"])
	csvfile.seek(0)
	next(csvfile) 
	fieldnames = ("id", "identifier", "gen_id", "evovles_from_id", "evo_chain", "color_id", "shape_id", "habitat_id", "gender_rate", "capture_rate", "base_happiness")
	reader = csv.DictReader(csvfile, fieldnames)
	unlock_dict = {} 
	for row in reader:
		if row["id"] not in list_evolves_from:
			for i in range(len(pokemon_type[row["id"]])):
				poke_list = [] 
				if (pokemon_type[row["id"]][i] in unlock_dict):
					poke_list = unlock_dict[pokemon_type[row["id"]][i]]
				poke_list.append(row["identifier"])
				unlock_dict[pokemon_type[row["id"]][i]] = poke_list 
	with open("../unlock_list.json", "w") as outfile:
		json.dump(unlock_dict, outfile)

def main():
	generate_abilities_json () 
	generate_moves_json () 
	generate_pokemon_json () 
	generate_unlock_json () 

if __name__ == "__main__":
	main() 