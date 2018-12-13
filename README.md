# PF1 Final project: Chess-expect!

The idea behind this project is to develop a big-bang chess program which allows you to play against
a friend or colleague on one machine.


## Installation guide

  * Clone or download the repo
  * Open chess-expect.rkt with a Racket-compatible IDE (like DrRacket)
  * Run the program and call (main 0)
  * Enjoy!


## How to play

Every player moves its cursor with respectively WASD (player 1 - white pieces) and the arrow keys 
(player 2 - black pieces); the spacebar is used to pick up and drop the pieces. The turn ends automatically 
when a piece gets dropped on a valid tile.

Note: to win, you have to eat the king. Yup.


## Language and libraries

The program is in Advanced Student Language, made with DrRacket v7.0.

It uses the libraries racket/base (for struct-copy), 2htdp/image (for image manipulation and drawing) 
and 2htdp/universe (big-bang).
