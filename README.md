# Pokémon: Snowdown

## Overview

![Concept Logo for Snowdown.](https://raw.githubusercontent.com/maZang/PokeSnowdown/master/data/backgrounds/PokemonLogo.png?token=AHfETxB_eZngHloYZgbLCWpaCV_-DF5lks5WfMCIwA%3D%3D)

Welcome to the next generation of Pokémon! This is Pokémon Snowdown, a winter-themed [showdown version](http://pokemonshowdown.com/) of the traditional Pokémon game.

In this iteration of Pokémon, you can fight your opponents, team up and play strategy to take down OP computer bots, and even edit your Pokémon with a fully-functional editor for hours of endless fun! Can you take down Professor Oak in what could be the hardest challenge yet for young gamers?

_Note: For now, Pokémon Snowdown has only been ported to Linux-compatible systems; the binary files will only run on such systems. We are working on making a more universal executable of the game or porting it so it can be functional cross-platform. Let us know if you can help!_

----

## Gameplay

Here is a basic gameplay demo video - https://www.youtube.com/watch?v=ACxjcFc824A

Pokémon Snowdown supports all of the newest-generation Pokémon (through X and Y), over 500 moves and abilities for these Pokémon, and various animations that try to stay true to the original.

However, this version of Pokémon is different from the traditional ones in that there are not different gyms to travel to; rather, there are three different modes of gameplay: single-player mode (like in the original game), double-player mode, and even no-player mode!

 - Single-player mode is the traditional fun. You can play a computer in a fight with 6 of the many Pokémon you have unlocked (initially, your selection is quite limited, but it will grow with time). If you feel up to the challenge, you can engage in Tournament mode, where you can fight several opponents in sequence, leading up to Professor Oak. This is a great way to unlock new Pokémon and arm yourself with stronger defences against future opponents. But be warned: Tournament mode is also quite challenging, and meant to pose a slight impediment for beginners as they try exploring their options with a limited subset of the Pokémon.

 - Double-player mode is much like what it sounds; you and your (only) friend can play each other! You can only use the Pokémon you've unlocked so far, though, so playing single-player for a while may be beneficial.

 - In no-player mode, you get to watch two overpowered AI battle it out (if you're bored and want to see what real fighting looks like while you're supposed to be studying for your CS final exams...).

_Warning: The computer is quite good, and it can be frustrating to take it down. Fun games include ones with hard-to-beat opponents. :)_

Interested? Download the executable and play for yourself! Let us know what you think.

----

## About the Team

Pokémon Snowdown was originally envisioned and created by four team members in Fall 2015 as part of a month-long [final project](http://www.cs.cornell.edu/courses/CS3110/2015fa/proj/proj.html) for [`CS 3110`](http://www.cs.cornell.edu/courses/CS3110/2015fa/) at Cornell University:

  * [Matthew Zang](https://github.com/maZang)
  * [Chirag Bharadwaj](https://github.com/chiragbharadwaj)
  * [Nicholas Shieh](https://github.com/nicholasshieh)
  * [Young Chan Kim](https://github.com/youngchankim)

We welcome any changes to the repository! Feel free to fork a branch and submit a pull request if you believe you could contribute some valuable changes. We will review your changes and merge them in if we feel it adds something substantial to the gameplay or user experience (or fixes any bugs, if you've caught some). Do let us know of your ideas!

The codebase is written mostly in `OCaml`, a functional programming language that supports imperative features. Some `Python` scripts that generate `JSON` files are also mixed in; the `JSON` files serve as the data hub that feeds into the gameplay and is modified as needed based on user input (i.e. statistics, unlocked Pokémon, etc.).

Contributors since the original submission of the project include:
  * [Adeet Phanse](https://github.com/phansa)
  * [Alvin Leung](https://github.com/aleung013)
  * [Jacob Arriola](https://github.com/Arrioj)
  * [Matt Bu](https://github.com/mbu13)

Image Sources

Source - http://f.tqn.com/y/animatedtv/1/L/D/d/1/spongebob_wide.jpg

Sprite Used - 

![Spongebob](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/spongebob.gif)

Source - http://rogue-lei.deviantart.com/art/Trainer-Sprites-for-my-Fanfic-347869729

Artist - http://rogue-lei.deviantart.com/

Sprites Used -

![Bob](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/bob.gif)

![Bob Sprite](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/bob.png)

![Suzie](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/suzie.gif)

![Suzie Sprite](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/suzie.png)

![Kyle](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/kyle.gif)

![Kyle Sprite](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/kyle.png)

![Dan](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/dan.gif)

![Dan Sprite](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/dan.png)

![Honorable](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/honorable.gif)

![Honorable Sprite](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/honorable.png)

Source - http://treeck0ia.deviantart.com/art/TrEEck0ia-s-Pokemon-Sprites-294127299

Artist - http://treeck0ia.deviantart.com/

Sprites Used - 

![Angela](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/angela.gif)

![Angela Sprite](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/angela.png)

![Freddrick](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/freddrick.gif)

![Freddrick Sprite](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/freddrick.png)

![J-Sean](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/j-sean.gif)

![J-Sean Sprite](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/j-sean.png)

![Reaper](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/reaper.gif)

Source - http://orig09.deviantart.net/0692/f/2008/008/1/7/naruto_sprites_by_marcani.png

Artist - http://marcani.deviantart.com/

Sprites Used - 

![Sakura](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/sakura.gif)

![Hinata](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/hinata.gif)

![Neji](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/neji_hyuga.gif)

Source - http://orig10.deviantart.net/35f2/f/2006/352/9/a/naruto_sprites_by_disneyfreak007.jpg 

Artist - http://disneyfreak007.deviantart.com/

Sprites Used -

![Sakura Sprite](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/sakura.png)

![Hinata Sprite](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/hinata.png)

![Neji Sprite](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/neji_hyuga.png)

Sources - http://gokou-sama.deviantart.com/art/Ultimate-Showdown-Sprites-06-101760346

http://gokou-sama.deviantart.com/art/Ultimate-Showdown-Sprites-12-119069989

http://gokou-sama.deviantart.com/art/Ultimate-Showdown-Sprites-17-133636939

http://gokou-sama.deviantart.com/art/Ultimate-Showdown-Sprites-34-200459922

Artist - http://gokou-sama.deviantart.com/

Sprites Used - 

![Reaper](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/reaper.png)

![Batman](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/batman.png)

![Pope](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/pope.png) 

![Spongebob](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/spongebob.png)

![Squidward](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/squidward.png)

![Beast Boy](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/beastboy.png)


Source - http://www.deviantart.com/art/Batman-Sprites-92996795

Artist - http://lizuka.deviantart.com/

Sprite Used - 

![Batman Sprite](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/batman.gif)

Source - http://tf2.gamebanana.com/sprays/62919

Artist - http://gamebanana.com/members/1194681

Sprite No Longer in Use

Source - http://mugenmultiverse.fanbb.net/t6411-edits-beast-boy-and-hawkeye

Artist - Fron84

Sprite Used -

![Beast Boy Sprite](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/beastboy.gif)

Source - http://ih1.redbubble.net/image.193615005.6278/flat,800x800,075,f.u2.jpg

Source - http://www.redbubble.com/people/emilyosman/works/20946278-squidward-dab?p=sticker

Artist - emilyosan

Sprite Used - 

![Squidward](https://github.com/Phansa/PokeSnowdown/blob/master/data/backgrounds/player_versus/squidward.gif)

Source - http://blzofozz.deviantart.com/art/Microsoft-Paint-DBZ-354200771

Artist - BLZofOZZ

![Goku](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/goku.png)

![Yamcha](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/yamcha.png)

![Freiza](https://github.com/Phansa/PokeSnowdown/blob/master/data/tournament/NPC/freiza.png)

Source - http://www.serebiiforums.com/showthread.php?275001-Coronis-s-PokeSpecial-Sprites-(DS-version)

Artist  - Coronis

Sprite Used - 

![Pokedex](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/Wiki-Guide-Images/Pokedex/pokedex_sprite.png)

----

#### Copyright Notice

<small><small> This project and repository are licensed under the [Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 License](https://creativecommons.org/licenses/by-nc-nd/4.0/). Most rights reserved. This means that this information is publicly viewable by all and allowed to be reuploaded/shared elsewhere by all, as long as the original authors are attributed publicly. You may not use ANY part of this repository for commercial purposes.

Copyright &copy; 2015 Matthew Zang, Chirag Bharadwaj, Nicholas Shieh, and Young Chan Kim at [Cornell University](http://www.cs.cornell.edu/).
</small></small>
