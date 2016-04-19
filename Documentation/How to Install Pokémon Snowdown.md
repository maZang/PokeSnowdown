<center> <h1>How to Install Pokémon Snowdown </h1> </center>
<center> <h1>By Adeet Phanse</h1> </center>
So you have stumbled upon this game and are probably wondering, how can I Install it on my computer and play? The answer is it's not that simple. You will need to download and install the Cornell Virutal machine. I will walk through these steps with you.

http://www.cs.cornell.edu/courses/cs3110/2015fa/vm.html

For this tutorial I have created a new user account on my computer. The first step will be to download and install virtual box onto your computer. I am installing virtual box on windows in this example.

![paint2](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint2.png)
![paint1](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint1.png)

Run the installer and go through the set up like this.

![paint3](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint3.png)
![paint4](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint4.png)
![paint11](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint11.png)
![paint6](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint6.png)
![paint7](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint7.png)
![paint8](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint8.png)

Once virutalbox is installed the next step is to download the OVA file (the file that will help set up your virtual machine). This download may take a while.

![paint9](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint9.png)
![paint10](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint10.png)

Now we will move on to installing the Virtual Machine. Run Virtual Box

![paint12](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint12.png)

Rune virtual box and click file -> import appliance

![paint13](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint13.png)
![paint14](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint14.png)

Find the OVF file you just downloaded and select it.

![paint15](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint15.png)
![paint16](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint16.png)
![paint17](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint17.png)

Click next then click import and wait for it to finish

![paint18](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint18.png)
![paint19](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint19.png)

Once its done you can press start to run it.

![paint20](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint20.png)

If the virutal machine asks you for a password, shut down the machine(press x and click power off the machine) then start it again.

![paint21](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint21.png)
![paint22](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint22.png)

If everything worked up to this point you should get to a screen looking like this. A basic knowledge of linux commands would be very helpful at this point, but this tutorial will walk you through what to do. 

![paint23](https://github.com/Phansa/PokeSnowdown/blob/master/Documentation/How-To-Install-Game-Images/paint23.png)

Now you will want to type in the terminal. Type -
mkdir game 
cd game
git clone https://github.com/Phansa/PokeSnowdown.git
cd PokeSnowdown
cd game
ocamlfind ocamlc -g -thread -package lablgtk2 -package async -package yojson -package str -linkpkg info.mli pokemon.mli gui.mli game.mli pokemon.ml mutableQ.ml ai.ml save.ml tournament.ml gui.ml battle_controller.ml game.ml -o game
./game 
and you're done!.

To acces the game later one just do
cd game
cd PokeSnowdown
cd game
./game

