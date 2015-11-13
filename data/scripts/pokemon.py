import urllib.request
import json 

NUM_POKEMON = 718 

def change_json_list_to_list (string, list):
	final = [] 
	for item in list:
		final.append(item[string])
	return final 

def preprocess_poke (poke):
	del poke["growth_rate"]
	abilities = change_json_list_to_list("name", poke["abilities"])
	poke["abilities"] = abilities 
	del poke["catch_rate"]
	types = change_json_list_to_list("name", poke["types"])
	poke["types"] = types 
	del poke["ev_yield"]
	del poke["happiness"]
	del poke["weight"]
	del poke["height"]
	del poke["created"]
	del poke["descriptions"]
	del poke["egg_cycles"]
	del poke["egg_groups"]
	del poke["sprites"]
	del poke["pkdx_id"]
	del poke["evolutions"]
	moves = change_json_list_to_list("name", poke["moves"])
	poke["moves"] = moves 
	del poke["species"]
	del poke["exp"]
	del poke["total"]
	del poke["modified"]
	del poke["male_female_ratio"]

def generate_pokemon_json(): 
	pokelist = {}
	for x in range(1, NUM_POKEMON + 1):
		poke_url = "http://pokeapi.co/api/v1/pokemon/" + str(x) + "/"
		poke = json.loads(str( (urllib.request.urlopen(poke_url).read()), "utf-8"))
		preprocess_poke (poke)
		pokelist[x] = poke
		print("num={:d}".format(x))
	with open("../pokemon.json", "w") as outfile:
		json.dump(pokelist, outfile)

def main():
	generate_pokemon_json()

if __name__ == "__main__":
	main() 