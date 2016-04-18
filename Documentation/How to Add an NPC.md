<center> <h1>How to add an NPC to Pokemon Snowdown </h1> </center>
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







