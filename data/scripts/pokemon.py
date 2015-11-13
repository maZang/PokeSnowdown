import urllib.request
import json 

NUM_POKEMON = 718 

def preprocess_poke (poke):
	

def generate_pokemon_json(): 
	pokelist = {}
	for x in range(1, 2):
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