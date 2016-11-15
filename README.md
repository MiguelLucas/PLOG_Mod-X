# PLOG_Mod-X

This project is my application of the Mod-X board game (https://www.cryptozoic.com/games/mod-x) in Prolog. It is complete with all functionalities, including options to change number of pieces per player, number of points to be achieved and different modes. It includes a Player vs Player mode, a Player vs Computer mode and a Computer vs Computer mode.

There are 2 dificulties in the game with diferent levels of artificial intelligence (one easier, other harder). The first one will try its best to score as quickly as possible. The second one will do this, while at the same time will try to destroy your chances of scoring.

The game has 2 graphical interfaces, running simultaneously, for better comprehension of the current game state. The first is a text view where:

- Player 1 pieces are marked with a "x";
- Player 2 pieces are marked with a "o";
- Jokers are marked with a "J";
- Player 1 points are marked with a "1" and are painted green;
- Player 2 points are marked with a "2" and are painted blue;

The second is a "table" interface in which:

- Player 1 pieces are marked with a blue "x";
- Player 2 pieces are marked with a red "x";
- Jokers are marked with a yellow "x";
- Player 1 points are painted blue;
- Player 2 points are painted red/pink;

The main focus of this project is the logical implementation of the game, of its rules and the artificial intelligence, rather than the graphical interface. It was designed to run using SWI Prolog (http://www.swi-prolog.org/). 

<p align="center">
  <img src="https://github.com/MiguelLucas/PLOG_Mod-X/blob/master/imgs/img1.PNG">
  <span class="caption">
  <p align="center"><b>Fig. 1</b> - Initial menu</p>
        </span>
</p>

<p align="center">
  <img src="https://github.com/MiguelLucas/PLOG_Mod-X/blob/master/imgs/img2.PNG">
  <span class="caption">
  <p align="center"><b>Fig. 2</b> - Game begins. Overview of both graphical interfaces</p>
        </span>
</p>

<p align="center">
  <img src="https://github.com/MiguelLucas/PLOG_Mod-X/blob/master/imgs/img3.PNG">
  <span class="caption">
  <p align="center"><b>Fig. 3</b> - Both players placed their pieces in the first turn</p>
        </span>
</p>

<p align="center">
  <img src="https://github.com/MiguelLucas/PLOG_Mod-X/blob/master/imgs/img4.PNG">
  <span class="caption">
  <p align="center"><b>Fig. 4</b> - Player 2 scored some points</p>
        </span>
</p>

<p align="center">
  <img src="https://github.com/MiguelLucas/PLOG_Mod-X/blob/master/imgs/img5.PNG">
  <span class="caption">
  <p align="center"><b>Fig. 5</b> - Player 1 scored some points, and got one stolen from the other player</p>
        </span>
</p>
