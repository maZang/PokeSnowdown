# Pokémon: Snowdown

## Overview

![Concept Logo for Snowdown.](https://raw.githubusercontent.com/maZang/PokeSnowdown/master/data/backgrounds/PokemonLogo.png?token=AHfETxB_eZngHloYZgbLCWpaCV_-DF5lks5WfMCIwA%3D%3D)

Welcome to the next generation of Pokémon! This is Pokémon Snowdown, a winter-themed [showdown version](http://pokemonshowdown.com/) of the traditional Pokémon game.

In this iteration of Pokémon, you can fight your opponents, team up and play strategy to take down OP computer bots, and even edit your Pokémon with a fully-functional editor for hours of endless fun! Can you take down Professor Oak in what could be the hardest challenge yet for young gamers?

_Note: For now, Pokémon Snowdown has only been ported to Linux-compatible systems; the binary files will only run on such systems. We are working on making a more universal executable of the game or porting it so it can be functional cross-platform. Let us know if you can help!_

----

## Gameplay

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

http://rogue-lei.deviantart.com/art/Trainer-Sprites-for-my-Fanfic-347869729

http://treeck0ia.deviantart.com/art/TrEEck0ia-s-Pokemon-Sprites-2941272993

http://orig09.deviantart.net/0692/f/2008/008/1/7/naruto_sprites_by_marcani.png



----

#### Copyright Notice

<small><small> This project and repository are licensed under the [Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 License](https://creativecommons.org/licenses/by-nc-nd/4.0/). Most rights reserved. This means that this information is publicly viewable by all and allowed to be reuploaded/shared elsewhere by all, as long as the original authors are attributed publicly. You may not use ANY part of this repository for commercial purposes.

Copyright &copy; 2015 Matthew Zang, Chirag Bharadwaj, Nicholas Shieh, and Young Chan Kim at [Cornell University](http://www.cs.cornell.edu/).
</small></small>
