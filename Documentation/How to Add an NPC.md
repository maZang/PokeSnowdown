<center> <h1>How to Add an NPC to Pokemon Snowdown </h1> </center>
<center> <h1>By Adeet Phanse</h1> </center>

Before anything, make sure you are running a virtual machine that can run the game. You can find instructions on how to do so in the "How to Install Pokémon Snowdown" guide located here
https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How%20to%20Install%20Pok%C3%A9mon%20Snowdown.md

NOTE: If at any point you get a Fatal:exception Out of Memory error just restart the virtual machine


This guide is assuming you have cloned the repository from https://github.com/Phansa/PokeSnowdown (preferred for now) or https://github.com/maZang/PokeSnowdown and have basic knowledge about how linux works. Do a git pull command before anything just to make sure you repository is up to date. This example is me running a git pull after making this guide, so my local repository did not have all the images I uploaded.

![gitpull](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/gitpull.png)

![gitpull2](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/gitpull2.png)

Adding an NPC to the Pokémon Snowdown game is a great way to contribute to this project!  Just follow these steps and you be will on your way to making your own NPC in this game and hopefully getting it accepted into the main project!
For this example we will be adding Bob.npc.  If you look in the tournament.mli file you will find him there.  You will need an image edition program, for this example I will be using Paint.Net which you can find here.
http://www.getpaint.net/download.html

If you are on a mac you can use the program GIMP to do a similar image editing process.

http://www.gimp.org/downloads/

Installing PAINT.NET Guide -

Click on this link above - http://www.getpaint.net/download.html

![new_paint](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/new_paint.png)

Click the link under Free Download Now which will provide you with a zip file

![new_paint2](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/new_paint2.png)


Extract the zip file

![new_paint3](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/new_paint3.png)

![new_paint4](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/new_paint4.png)

Enter the folder you have just made and run the installer

![new_paint5](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/new_paint5.png)

Hit yes when the administrator prompt show ups and just use express settings

![new_paint6](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/new_paint6.png)

Hit next on this screen

![new_paint7](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/new_paint7.png)

It has been installed at this point!

![new_paint8](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/new_paint8.png)


You will also need a sprite and trainer "portrait".  You can find sprite sheets for trainers online.

![sprite sheet](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/new_trainers.png)

I will be using this sprite sheet I found online for this example. I will be adding the top left sprite into the game.

You can find a list of sprite sheets currently being used/currently having their artists credited here -

https://github.com/Phansa/PokeSnowdown/tree/master/data/tournament/NPC/Sprite-Sheet

And a list of trainer gif images here -

https://github.com/Phansa/PokeSnowdown/tree/master/data/tournament/NPC/Trainer-Portraits

These are the Sprites/Trainer Gifs current being used

![Current_NPC_Sprites](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/Current_NPC_Sprites.png)

![Current_NPC_Portraits](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/Current_NPC_Portraits.png)

\*\*\*Please remember to cite the source you are using in the README.MD file under image sources!!!

Put the link to the specific image you are using as well as a link to the artist's deviant art page. Also contact the artists through deviant art / email to let them know you are using their images. Ask for permission first and if they say no, please change the images.

Here is a template deviant art note you can use. You will sign up for a deviant art account before doing this step.

Deviant Art Email Template -

Subject - Using your image for an open source game Pokemon Snowdown

Body text - 

Hi I am working on an open source game located here - https://github.com/Phansa/PokeSnowdown and we are using your image
(\*insert image url here \*). We have given you credit in the README.md https://github.com/Phansa/PokeSnowdown/blob/master/README.md. If you would like us to not use your image, let me know!

Click send note afterwards.

![paint49](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint49.png)

How to credit the artist -

I add the link to the specific image I am using to the README.md as well as a link to the deviant art page of disneyfreak007

![paint46](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint46.png)
![paint47](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint47.png)

The in the README.md I will put

![paint48](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint48.png)

Add your sprite sheet to the sprite sheet folder located at PokeSnowdown -> game data -> tournament -> NPC -> Sprite-Sheet if it does not exist there already.

You can do this directly in your local repository, and git add the file or do it through github.com if you are a contributor to the project. If you are not, you will have to directly add the images to your local repository.

Adding the sprite sheet to your local repository -

Save it into the sprite sheet folder then run

git add naruto_sprite.jpg

git commit -m "adding naruto_sprite.jpg"

git push (I use git push -u origin master)

![paint43](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint43.png)

![paint44](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint44.png)

Your changes will have to be pulled by the main repository but having all the sprite sheets helps out a lot.

Adding the sprite sheet through github.com -

Navigate to https://github.com/Phansa/PokeSnowdown/tree/master/data/tournament/NPC/Sprite-Sheet and click upload files, find the sprite sheet and click commit changes. 

![paint40](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint40.png)

![paint41](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint41.png)

![paint42](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint42.png)

The final result of these steps

![paint45](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint45.png)

Repeat these steps if you are using a trainer portrait that is not on the sprite sheet.

Adding Carlton Spongebob through github.com -

![paint50](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint50.png)
![paint51](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint51.png)

The final result of adding Spongebob

![paint55](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint55.png)

Adding Dab Squidward through command line from the folder located at PokeSnowdown -> data -> tournament -> NPC -> Trainer-Portraits

git add squidward.jpg

git commit -m "add squidware (\*should have been squidward\*) source"

git push

![paint52](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint52.png)
![paint53](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint53.png)

The final result of adding Squidward

![paint54](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint54.png)

This will be the final result of these next two steps

![bobgif](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/bob.gif)

![bobpng](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/Bob.png)

The first thing you will have to do is crop the big trainer image and little trainer sprite.  Starting with the big trainer image -

![paint1](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint1.png)
![paint2](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint2.png)

You will then have to edit the cropped image and make the background transparent. This video tutorial will show you how to do this in Paint.net. Save the images you make in PNG format so they do not regain their white background.

https://www.youtube.com/watch?v=cdFpS-AvNCE

I have clicked the magic wand button and also clicked the white background of this image. The next step will be to press delete while the background is selected.
![paint3](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint3.png)

The results of doing this is

![paint4](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint4.png)

This trainer image can be any height/width really it depends on the source file you find them from. Test it in the game and if it doesn't look good do a resize operation in paint.net/your image editing software.

Now the trainer image is good to go! The final step is to save this image as a GIF format image.

Go to File -> Save As -> bob and select format GIF. 

![corrected_bob](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/corrected_bobgif.png)

Note: Save the image name as lowercase


Just press ok on the next screen

![paint5](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint5.png)

Congratulations, you have successfully made the trainer battle sprite! 

![bobgif](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/bob.gif)

For the little sprite do the same exact steps up to saving it as a GIF. So crop the image from the main sprite sheet and use the magic wand to remove the white background. I will skip ahead to the that part.

![paint6](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint6.png)

We will want to resize this sprite so it will have the proper dimensions in game. In general you will want to resize the width to be around 32 pixels wide. It can be a bit lower, it really depends on how you cropped the image.  ~30 pixels for width should be fine.

![paint7](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint7.png)

Afterwards go to file -> save as -> bob.png

![corrected_bob_png](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/corrected_bobpng.png)

Note: Save the image name as lowercase.

Just hit OK on the next screen 

![paint9](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint9.png)

Congratulations, you have successfully made the in game sprite!

![bobpng](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/Bob.png)


In this example I am working with Paint.net in windows and I email these two images, bob.png and bob.gif, to myself then open them on my virtual machine and redownload them.

#Adding Characters Using GIMP:

#Adding Files to the Game Folders:

Now we will have to add these two files to the game folders. Place bob.gif into PokeSnowdown -> data->backgrounds->player_versus

![paint10](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint10.png)

Place Bob.png into PokeSnowdown -> data->tournament->NPC

![paint11](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint11.png)


Now we can edit the tournament.mli file. It is important to pay attention to capitalization of the variables we are using so you do not get a compiler error.

Edit this line and add | Bob 
type enemy = RoughNeck | Beauty | BugCatcher | CampNerd | DragonTamer | Falkner | FatMan | Psychic | Youngster | ProfOak | Chancellor

so it becomes

type enemy = RoughNeck | Beauty | BugCatcher | CampNerd | DragonTamer | Falkner | FatMan | Psychic | Youngster | ProfOak | Chancellor | Bob

![paint39](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint39.png)

Next we will add bobQuotes which will be the things the NPC says before battling you.

in this case we add this code -

let bobQuotes =

  [
    "Tread carefully.";
  
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

The JSON Files are located at PokeSnowdown\data\Tournament\NPCjson

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

![sample_json](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/sample_json.png)

A good resource to find competitive teams is - http://www.smogon.com/about.

More specifically - http://www.smogon.com/dex/xy/pokemon/

For this example I will show you how to find and add a single Pokémon from Smogon (you can add whole teams as well).

Following 

http://www.smogon.com/dex/xy/pokemon/

Click on any Pokémon that interests you. For this example I will be using Zebstrika.

Check the info.mli file for a list of current items. 

As of writing this guide (4/18/16), the current items are 

  Nothing | Leftovers | ChoiceBand | LifeOrb | ChoiceSpecs | ChoiceScarf
  
  | MegaStone | MegaStoneX | MegaStoneY | LightBall
  
You cannot directly put these items as strings into the game, you must refer for the strings below for what to set the "item": field to

  
Item Reference Guide :

Nothing - "nothing"

Leftovers - "leftovers"

ChoiceBand - "choice band"

LifeOrb - "life orb"

ChoiceSpecs - "choice specs"

ChoiceScarf - "choice scarf"

MeagStone - "MegaStone"

MegaStoneX - "MegaStoneX"

MegaStoneY - "MegaStoneY"
  
LightBall - "light ball"
  
Finding the current moves available will take a bit more work, but you can control f on the pokemon.json file.

![paint20](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint20.png)
![paint21](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint21.png)


Next scroll down through the strategies section and find a build you would like to use.

For this example, I will be using the All-Out Attack moveset/build because it is the only one available but there could be multiple 
builds like Latias has (shown below).

![paint22](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint22.png)
![paint23](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint23.png)

But back to Zebstrika, whose All-Out Attacker build looks like 

![paint24](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint24.png)

You will want to fill in the fields of the JSON file with the details from this Smogon guide. We will be replacing Absol with Zebstrika. For Hidden Power Ice we will just use hidden-power.

![absol_before](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/absol_before.png)
![zebstrika_after](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/zebstrika%20after.png)

I ran into an error with this JSON file because I spelled the item life orb as life-orb. This is the correct Zebstrike JSON file

![zebstrika_fixed](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/zebstrika_fixed.png)

Make sure the "pokemon": line stays updated with the changes you make . If you are replacing a Pokémon, delete the old Pokémon from this list and add the new one you made.
"pokemon": [ "absol", "ampharos", "arcanine", "beedrill", "chandelure", "chesnaught", "cobalion", "cresselia", "gengar", "nidoking", "nidoqueen", "sableye", "tyranitar", "venusaur", "weavile"],
becomes
"pokemon": [ " zebstrika", "ampharos", "arcanine", "beedrill", "chandelure", "chesnaught", "cobalion", "cresselia", "gengar", "nidoking", "nidoqueen", "sableye", "tyranitar", "venusaur", "weavile"],

![pokemon_before](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/pokemon-before.png)
![pokemon_after](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/pokemon-after.png)

otherwise you can just append the Pokémon you are adding to the end of the list.  For example
"pokemon": [ "absol", "ampharos", "arcanine", "beedrill", "chandelure", "chesnaught", "cobalion", "cresselia", "gengar", "nidoking", "nidoqueen", "sableye", "tyranitar", "venusaur", "weavile", "zebstrika"],

You can repeat this process for each Pokémon you would like to add! 

Note the ability lightningrod is spelled lightning-rod.

Also if you get this error, 

gardevoir

(((pid 2161) (thread_id 0))

 ((human_readable 2016-04-18T12:07:23-0400)
 
  (int63_ns_since_epoch 1460995643778754000))
  
 "unhandled exception in Async scheduler"
 
 ("unhandled exception"
 
  ((src/monitor.ml.Error_
  
    ((exn (Failure "Does not occur"))
    
     (backtrace
     
      ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
      
       "Called from file \"pokemon.ml\", line 807, characters 13-73"
       
       "Called from file \"list.ml\", line 55, characters 20-23"
       
       "Called from file \"battle_controller.ml\", line 130, characters 2-38"
       
       "Called from file \"battle_controller.ml\", line 2525, characters 14-39"
       
       "Called from file \"src/job_queue.ml\", line 164, characters 6-47" ""))
       
     (monitor
     
      (((name main) (here ()) (id 1) (has_seen_error true)
      
        (is_detached false) (kill_index 0))))))
        
   ((pid 2161) (thread_id 0)))))

You are trying to use an ability, move or item that does not exist in the game files.

\*\*\*\*\*I will redo these steps above for another Pokémon, you can skip this part if you want.
Now we will be using Gardevoir.

![paint28](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint28.png)

replacing Zebstrika

![paint30](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint30.png)

Note that instead of writing gardevoirite (or any mega stone) as an item you will need to use MegaStone/MegaStoneX/MegaStoneY.

![gardevoir_fixed](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/gardevor_fixed.png)
![gardevoir_before](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/pokemon-gardevoir.png)
![gardevoir_after](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/pokemon-gardevoir-replace.png)

The Pokémon listed on the "pokemon": line are all the possible Pokémon this npc can use. There needs to be at least one Pokémon in the JSON file, but you can have however many you want.  All moves/strings should be lowercase as you can see from this example.  Some moves/abilities may not exist in the game at the time, so if you get a compiler error that may be the reason why. Make sure that you put a '-' in between moves/abilities that contain two words, for example razor leaf is razor-leaf.

Once the JSON file is done, you will have to edit the factoryset.json file and put the line.
"bob":0 at the end of this list. 

![paint19](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint19.png)

At this point if you would like to test the NPC out before trying to push it to the main repository, which I highgly recommend you do, you should follow these steps.

Edit this line in tournament.ml

let getRandomEnemy () =

  match Random.int 11 with
  
to be

let getRandomEnemy () =

  (\*match Random.int 11 with\*)
  
  match 10 with (where 10 is the bob npc in this case)
  
\*the (\* .... \*) notation comments out a line in OCaml.

Now when tournament mode runs you will only encounter bob!

![paint25](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint25.png)


Recompile the game ( go to the game folder and run the command 
ocamlfind ocamlc -g -thread -package lablgtk2 -package async -package yojson -package str -linkpkg info.mli pokemon.mli gui.mli game.mli pokemon.ml mutableQ.ml ai.ml save.ml tournament.ml gui.ml battle_controller.ml game.ml -o game)
from the PokeSnowdown directory -

cd game

ocamlfind ocamlc -g -thread -package lablgtk2 -package async -package yojson -package str -linkpkg info.mli pokemon.mli gui.mli game.mli pokemon.ml mutableQ.ml ai.ml save.ml tournament.ml gui.ml battle_controller.ml game.ml -o game

![paint26](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint26.png)

run it using the command ./game

Click 1 player -> tournament then use the W A S D to move around and press H to talk to the npc you want to test.

![paint31](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint31.png)
![paint32](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint32.png)
![paint33](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint33.png)
![paint35](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint35.png)
![paint34](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint34.png)

If you make it to the battle screen then your NPC is fully working!

\*\*\*\*Make sure you edit the match random line check back to normal before you add or commit your tournament.mli file to the repository.

let getRandomEnemy () =

match Random.int 11 with
  
(\* match 10 with \*)


![paint36](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint36.png)

Also in factorysets.json make sure all of these values are set to 0.

"baldman":0,"beauty":0,"bugcatcher":0,"campernerd":0,"dragontamer":0,"falkner":0,"fatman":0,"psychic":0,"youngster":0,"professor oak":0,"play oak":0,"honorable":0,"bob":0}

If you beat a trainer in tournament mode during your testing, these values can go up which is not what you want for the main program.

Once you have done all that it's time to add, commit, and push your changes!
You will need to git add the following files :

bob.json located at PokeSnowdown->data->tournament->NPCjson

![gitaddbobjson](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/gitaddbobjson.png)

bob.png located at PokeSnowdown -> data -> tournament -> NPC

![gitaddbobpng](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/gitaddbobjson.png)

bob.gif located PokeSnowdown -> data->backgrounds->player_versus

![gitaddbobgif](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/gitaddbobgif.png)

factoryset.json located at PokeSnowdown->data

![gitaddfactoryset](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/gitaddfactoryset.png)

tournament.ml  located at PokeSnowdown -> game

![gitaddtournamentml](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/gitaddtournamnetml.png)

Now you can git commit -m "Type your message here".

For this example I will just make the message "Adding bob npc to the game".

![paint37](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint37.png)

Finally do a git push -u origin master and you are done!

![paint38](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Add-NPC-Images/paint38.png)

Congratulations, you have contributed back to the main repository! Your name will be added to the list of contributors if your changes get accepted!
