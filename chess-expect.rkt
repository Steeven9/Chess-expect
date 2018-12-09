;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname chess-expect) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; PF1 final project: chess-expect!
; Main file
; Author: Stefano Taillefert

;   0 1 2 3 4 5 6 7
; 0 R K B K Q B K R
; 1 P P P P P P P P
; 2
; 3
; 4
; 5
; 6 P P P P P P P P
; 7 R K B K Q B K R
;   0 1 2 3 4 5 6 7

;;;;;;;;;;;;;;;;;;;;;;;;;
; Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/base)
(require 2htdp/image)
(require 2htdp/universe)


;;;;;;;;;;;;;;;;;;;;;;;;;
; Data definition
;;;;;;;;;;;;;;;;;;;;;;;;;

; A Coord is a NonNegativeInteger between 0 and 7 (included).
; Interpretation: a coordinate on the chessboard.

; A World is a (make-world pieces pos1x pos1y pos2x pos2y turn pick1 pick2) where:
; - pieces is a List<Piece>,
; - pos1x, pos1y, pos2x, pos2y are Coord,
; - turn is either 1 or 2,
; - pick1, pick2 are Boolean.
; Interpretation: the world status with the position of the two players'
; (white is 1 and black is 2) pointers and movement status.
(define-struct world [pieces pos1x pos1y pos2x pos2y turn pick1 pick2])

; A Piece is a (make-piece color type x y) where:
; - color, type are Strings,
; - x, y are Coord.
; Interpretation: a piece with its data and coordinates.
(define-struct piece [color type x y])


;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;

; Screen width
(define WIDTH 800)

; Screen height
(define HEIGHT 800)

; The default piece list
(define PIECE-LIST (list (make-piece "white" "Pawn" 0 6)
                         (make-piece "white" "Pawn" 1 6)
                         (make-piece "white" "Pawn" 2 6)
                         (make-piece "white" "Pawn" 3 6)
                         (make-piece "white" "Pawn" 4 6)
                         (make-piece "white" "Pawn" 5 6)
                         (make-piece "white" "Pawn" 6 6)
                         (make-piece "white" "Pawn" 7 6)
                         (make-piece "white" "Rook" 0 7)
                         (make-piece "white" "Knight" 1 7)
                         (make-piece "white" "Bishop" 2 7)
                         (make-piece "white" "King" 3 7)
                         (make-piece "white" "Queen" 4 7)
                         (make-piece "white" "Bishop" 5 7)
                         (make-piece "white" "Knight" 6 7)
                         (make-piece "white" "Rook" 7 7)
                         (make-piece "black" "Pawn" 0 1)
                         (make-piece "black" "Pawn" 1 1)
                         (make-piece "black" "Pawn" 2 1)
                         (make-piece "black" "Pawn" 3 1)
                         (make-piece "black" "Pawn" 4 1)
                         (make-piece "black" "Pawn" 5 1)
                         (make-piece "black" "Pawn" 6 1)
                         (make-piece "black" "Pawn" 7 1)
                         (make-piece "black" "Rook" 0 0)
                         (make-piece "black" "Knight" 1 0)
                         (make-piece "black" "Bishop" 2 0)
                         (make-piece "black" "King" 3 0)
                         (make-piece "black" "Queen" 4 0)
                         (make-piece "black" "Bishop" 5 0)
                         (make-piece "black" "Knight" 6 0)
                         (make-piece "black" "Rook" 7 0)))
                            

; Initial empty world.
; Starting positions are the king's coordinates, player1 (white) moves first.
(define INITIAL-WORLD (make-world PIECE-LIST
                                  3 7
                                  3 0
                                  1
                                  #false
                                  #false))

; A white tile
(define W-TILE (square (/ WIDTH 8) 'solid 'white))

; A black tile
(define B-TILE (square (/ WIDTH 8) 'solid 'brown))

; The white cursor
(define W-CURSOR (square (/ WIDTH 8) 'outline (pen 'red 5 "dot" "round" "bevel")))

; The white active cursor (when moving a piece)
(define W-CURSOR-ACTIVE (square (/ WIDTH 8) 'outline (pen 'red 5 "solid" "round" "round")))

; The black cursor
(define B-CURSOR (square (/ WIDTH 8) 'outline (pen 'blue 5 "dot" "round" "bevel")))

; The black active cursor (when moving a piece)
(define B-CURSOR-ACTIVE (square (/ WIDTH 8) 'outline (pen 'blue 5 "solid" "round" "round")))



;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Drawing functions ;;;;

; draw-board-line: Coord Coord Image -> Image
; Draws one chessboard line.
(define (draw-board-line x y img)
  (cond [(< 7 x) img]
        [else
         (place-image B-TILE
                      (tile-x x)
                      (tile-y y)
                      (draw-board-line (+ 2 x) y img))]))


; draw-board: Coord Image -> Image
; Draws the whole chessboard.
(define (draw-board y img)
  (cond [(< 7 y) img]
        [(member y '(1 3 5 7))
         (draw-board (add1 y) (draw-board-line 1 y img))]
        [else (draw-board (add1 y) (draw-board-line 0 y img))]))


; draw-cursors: World Image -> Image
; Draws the players cursors on a given image.
(define (draw-cursors w img)
  (place-image (if (= 1 (world-turn w))
                   W-CURSOR-ACTIVE
                   W-CURSOR)
               (tile-x (world-pos1x w))
               (tile-y (world-pos1y w))
               (place-image (if (= 2 (world-turn w))
                                B-CURSOR-ACTIVE
                                B-CURSOR)
                            (tile-x (world-pos2x w))
                            (tile-y (world-pos2y w))
                            img)))


; draw-piece: Piece Image -> Image
; Draws a single piece on a given image.
(define (draw-piece p img)
  (local [(define source (string-append "img/" (piece-color p) (piece-type p) ".png"))]
    (place-image (bitmap/file source)
                 (tile-x (piece-x p))
                 (tile-y (piece-y p))
                 img)))


; draw-pieces: List<Piece> Image -> Image
; Draws a list of pieces on a given image.
(define (draw-pieces pl img)
  (cond [(empty? pl) img]
        [else (draw-pieces (rest pl) (draw-piece (first pl) img))]))


; draw-text: World Image -> Image
; Prints the text according to the given world.
(define (draw-text w img)
  (place-image (if (= 1 (world-turn w))
                   (text "Player 1 moving" 24 'black)
                   (text "Player 2 moving" 24 'black))
               100
               850
               img))


; draw-world: World -> Image
; Draws the chessboard, cursors and pieces.
(define (draw-world w)
  (draw-text w
             (draw-pieces (world-pieces w)
                          (draw-cursors w
                                        (draw-board 0
                                                    (empty-scene WIDTH (+ 100 HEIGHT)))))))


;;;; Calculation functions ;;;;

; tile-x: Coord -> NonNegNumber
; Return the x coordinate corresponding to the given column number.
(define (tile-x col)
  (+ (/ WIDTH 16) (* col (/ WIDTH 8))))

; Tests
(check-expect (tile-x 0) 50)
(check-expect (tile-x 1) 150)
(check-expect (tile-x 6) 650)

  
; tile-y: Coord -> NonNegNumber
; Return the y coordinate corresponding to the given row number.
(define (tile-y row)
  (+ (/ HEIGHT 16) (* row (/ HEIGHT 8))))

; Tests
(check-expect (tile-y 0) 50)
(check-expect (tile-y 1) 150)
(check-expect (tile-y 6) 650)


;;;; Big-bang and handlers ;;;;

; handle-key: World Key -> World
; Handles keypresses.
(define (handle-key w key)
  (cond
    ; Reset
    [(key=? key "escape")
     INITIAL-WORLD]
    ; Player 1 movement
    [(and (key=? key "w") (= 1 (world-turn w)) (< 0 (world-pos1y w)))
     (struct-copy world w [pos1y (- (world-pos1y w) 1)])]
    [(and (key=? key "s") (= 1 (world-turn w)) (> 7 (world-pos1y w)))
     (struct-copy world w [pos1y (+ (world-pos1y w) 1)])]
    [(and (key=? key "a") (= 1 (world-turn w)) (< 0 (world-pos1x w)))
     (struct-copy world w [pos1x (- (world-pos1x w) 1)])]
    [(and (key=? key "d") (= 1 (world-turn w)) (> 7 (world-pos1x w)))
     (struct-copy world w [pos1x (+ (world-pos1x w) 1)])]
    ; Player 2 movement
    [(and (key=? key "up") (= 2 (world-turn w)) (< 0 (world-pos2y w)))
     (struct-copy world w [pos2y (- (world-pos2y w) 1)])]
    [(and (key=? key "down") (= 2 (world-turn w)) (> 7 (world-pos2y w)))
     (struct-copy world w [pos2y (+ (world-pos2y w) 1)])]
    [(and (key=? key "left") (= 2 (world-turn w)) (< 0 (world-pos2x w)))
     (struct-copy world w [pos2x (- (world-pos2x w) 1)])]
    [(and (key=? key "right") (= 2 (world-turn w)) (> 7 (world-pos2x w)))
     (struct-copy world w [pos2x (+ (world-pos2x w) 1)])]
    ; End turn (spacebar)
    [(and (key=? key " ") (= 1 (world-turn w)))
     (struct-copy world w [turn 2])]
    [(and (key=? key " ") (= 2 (world-turn w)))
     (struct-copy world w [turn 1])]
    ; Nothing
    [else w]))
  

; main: Nothing -> Nothing
; Launches big-bang.
(define (main _)
  (big-bang INITIAL-WORLD
    [name "Chess-expect!"]
    [to-draw draw-world]
    [on-key handle-key]))
