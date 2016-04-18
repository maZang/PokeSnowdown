<center> <h1>How to Add an NPC to Pokemon Snowdown </h1> </center>
<center> <h1>By Adeet Phanse</h1> </center>

Before anything, make sure you are running a virtual machine that can run the game. You can instructions on how to do so in the "How to Install Pokémon Snowdown" guide. 
This guide is assuming you have cloned the repository from https://github.com/Phansa/PokeSnowdown or https://github.com/maZang/PokeSnowdown and have basic knowledge about how linux works. 
Adding an NPC to the Pokémon Snowdown game is a great way to contribute to this project!  Just follow these steps and you be will on your way to making your own NPC in this game and hopefully getting it accepted into the main project!
For this example we will be adding Bob.npc.  If you look in the tournament.mli file you will find him there.  You will need an image edition program, for this example I will be using Paint.Net which you can find here.
http://www.getpaint.net/download.html
You will also need a sprite and trainer "portrait".  You can find sprite sheets for trainers online.

![sprite sheet](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/new_trainers.png)

I will be using this sprite sheet I found online for this example. I will be adding the top left sprite into the game.

This will be the final result of these next two steps

![bobgif](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/bob.gif)

![bobpng](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/Bob.png)

***Please remember to cite the source you are using in the README.MD file under image sources!!!

The first thing you will have to do is crop the big trainer image and little trainer sprite.  Starting with the big trainer image -

![paint1](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint1.png)
![paint2](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint2.png)

You will then have to edit the cropped image and make the background transparent. This video tutorial will show you how to do this in Paint.net. Save the images you make in PNG format so they do not regain their white background.

https://www.youtube.com/watch?v=cdFpS-AvNCE

I have clicked the magic wand button and also clicked the white background of this image. The next step will be to press delete while the background is selected.
![paint3](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint3.png)

The results of doing this is

![paint4](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint4.png)

Now the trainer image is good to go! The final step is to save this image as a GIF format image.

Go to File -> Save As -> bob and select format GIF. 

![corrected_bob](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/corrected_bobgif.png)

Note: Save the image as lowercase

Just press ok on the next screen

![paint5](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint5.png)

Congratulations, you have successfully made the trainer battle sprite! 

For the little sprite do the same exact steps up to saving it as a GIF. So crop the image from the main sprite sheet and use the magic wand to remove the white background. I will skip ahead to the that part.

![paint6](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint6.png)

We will want to resize this sprite so it will have the proper dimensions in game. In general you will want to resize the width to be around 32 pixels wide. It can be a bit lower, it really depends on how you cropped the image.  ~30 pixels for width should be fine.

![paint7](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint7.png)

Afterwards go to file -> save as -> bob.png

![corrected_bob_png](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/corrected_bobpng.png)

Note: Save the image as lowercase.

Just hit OK on the next screen 

![paint9](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint9.png)

Now we will have to add these two files to the game folders. Place bob.gif into PokeSnowdown -> data->backgrounds->player_versus

![paint10](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint10.png)

Place Bob.png into PokeSnowdown -> data->tournament->NPC

![paint11](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint11.png)


Now we can edit the tournament.mli file. It is important to pay attention to capitalization of the variables we are using so you do not get a compiler error.

Edit this line
type enemy = RoughNeck | Beauty | BugCatcher | CampNerd | DragonTamer | Falkner | FatMan | Psychic | Youngster | ProfOak | Chancellor
add | Bob 

so it becomes

type enemy = RoughNeck | Beauty | BugCatcher | CampNerd | DragonTamer | Falkner | FatMan | Psychic | Youngster | ProfOak | Chancellor | Bob

![paint39](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint39.png)

Next we will add bobQuotes which will be the things the NPC says before battling you.

in this case we add this code -

let bobQuotes =
  ["Tread carefully.";
    "Enforced equilibrium.";
    "You're already dead, you just haven't caught up yet."
  ] 

![paint12](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint12.png)

Next we will have to increment the Random.int integer and add bob to the list under getRandomEnemy

  match Random.int 11 with
  
  | 10 -> Bob

![paint14](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint14.png)

 
After this we have to edit the getStringFromEnemy line adding the variable we just made (Bob) and the file names we have decided to use (in this case we called the Bob png and Bob gif bob.png and bob.gif for ease of use but you can call those files anything you want." 

  | Bob -> "bob"

![paint15](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint15.png)

We will then have to add the ability for Bob to get his quotes. 

| Bob -> bobQuotes

Use the original Bob variable we declared at the start, the string from the last part is only used for the png and gif files. Once again it simpler to just call them the same thing as the NPC.

![paint16](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint16.png)

The final step will be to make a bob.json file containing the Pokémon that could be on his team and the Pokémon you unlock for beating him.

An easy way to start this process is to just copy the contents of an existing JSON file and just save it as ___.json. In this case it would be bob.json

![paint17](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint17.png)
![paint18](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint18.png)

For this part you have a lot of freedom as to what you want the trainer's team to be. You don't have to worry about the unlockable fields because we are making it so that all Pokémon will be unlockable. The unlockable field represents the Pokémon a player can unlock after beating the NPC in battle. You can add any Pokémon you want to this field! Just follow the format presented in this JSON file (honorable.json)

{

  "persian": {
  
    "name": "persian",
    
    "moves": [
    
      "pay-day",
      
      "swift",
      
      "night-slash",
      
      "fake-out"
      
    ],
    
    "ability": "limber",
    
    "nature": "jolly",
    
    "item": "life orb",
    
    "evs": {
    
      "hp": "0",
      
      "attack": "252",
      
      "special-attack": "0",
      
      "defense": "4",
      
      "special-defense": "0",
      
      "speed": "252"
      
    }
    
  },

  "pokemon": ["persian"],
  
  "unlockable": ["umbreon", "houndoom", "tyranitar", "mightyena", "shiftry", "sableye", "sharpedo", "cacturne", "crawdaunt", "absol", "honchkrow", "skuntank", "spiritomb", "drapion", "weavile", "liepard", "krookodile", "scrafty", "zoroark", "bisharp", "mandibuzz", "hydreigon", "greninja", "pangoro", "malamar", "venusaur", "beedrill", "arbok", "nidoqueen", "nidoking", "vileplume", "venomoth", "victreebel", "tentacruel", "muk", "gengar", "weezing", "ariados", "crobat", "qwilfish", "dustox", "swalot", "seviper", "roserade", "skuntank", "drapion", "toxicroak", "scolipede", "garbodor", "amoonguss", "dragalge"]
}

You can put a single Pokémon into the JSON file and the trainer will use 6 of them!

