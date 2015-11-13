from urllib.request import Request, urlopen 
import json 

def get_front_sprites ():
	with open('../pokemon.json') as infile:
		pokemon = json.load(infile)
	for pokey in pokemon:
		print(pokey)
		url = "http://play.pokemonshowdown.com/sprites/xyani/" + pokey + ".gif"
		req = Request(url, headers={'User-Agent': 'Mozilla/5.0'})
		name_of_file = "../sprites/" + url.split('/')[-1]
		gif = urlopen(req)
		gif_file = open(name_of_file, 'wb')
		block_size = 8192
		while True:
			buffer = gif.read(block_size)
			if not buffer:
				break
			gif_file.write(buffer)
		gif_file.close()

def get_back_sprites ():
	with open('../pokemon.json') as infile:    
		pokemon = json.load(infile)
	for pokey in pokemon:
		print(pokey)
		url = "http://play.pokemonshowdown.com/sprites/xyani-back/" + pokey + ".gif"

		req = Request(url, headers={'User-Agent': 'Mozilla/5.0'})
		name_of_file = "../back-sprites/" + url.split('/')[-1]
		gif = urlopen(req)
		gif_file = open(name_of_file, 'wb')

		block_size = 8192
		while True:
			buffer = gif.read(block_size)
			if not buffer:
				break
			gif_file.write(buffer)
		gif_file.close()

def main():
	get_back_sprites ()
	get_front_sprites () 

if __name__ == "__main__":
	main() 