# Chess-expect! <img src="img/icon.png" width="50">

The idea behind this project is to develop a big-bang chess program which allows you to play against
a friend or colleague on one machine.


## Installation guide

  * Clone or download the repo
  * Open Chess-expect! (Mac) or Chess-expect!.exe (Windows) from the dist folder
  * Enjoy!

  You can also directly open chess-expect.rkt with your favorite Racket editor and run it.


## How to play

Every player moves its cursor with respectively WASD (player 1 - white pieces) and the arrow keys 
(player 2 - black pieces); the spacebar is used to pick up and drop the pieces. The turn ends automatically 
when a piece gets dropped on a valid tile.

Notes:
 
  * To win, you have to eat the king. Yup. 
  * Castling and "en passant" have been intentionally left out of
     the game to make it harder. 100% intended.
  * You can jump over other pieces, unless you buy the Rules DLC (coming soon)


## Language and libraries

The program is in Advanced Student Language, made with DrRacket v7.0.

It uses the libraries racket/base (for struct-copy), 2htdp/image (for image manipulation and drawing) 
and 2htdp/universe (big-bang).

Icon by [Good Ware](https://www.flaticon.com/authors/good-ware) from [www.flaticon.com](www.flaticon.com).
