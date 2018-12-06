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

(require "config.rkt")
(require "pieces.rkt")



;;;;;;;;;;;;;;;;;;;;;;;;;
; Data definition
;;;;;;;;;;;;;;;;;;;;;;;;;

; A Coord is a NonNegativeInteger between 0 and 7 (included).
; Interpretation: a coordinate on the chessboard.

; A World is a (make-world scene pos1 pos2 mov1 mov2) where:
; - pieces is a List<Piece>,
; - pos1, pos2 are Coord,
; - mov1, mov2 are Boolean.
; Interpretation: the world status with the position of the two players'
; (white is 1 and black is 2) pointers and movement status (is he moving?).
(define-struct world [pieces pos1x pos1y pos2x pos2y mov1 mov2])

; A Piece is a (make-piece image x y) where:
; - image is an Image,
; - x, y are Coord.
; Interpretation: a piece with its sprite and coordinates.
(define-struct piece [image x y])



;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;

; The default piece list
(define PIECE-LIST (list (make-piece W-PAWN 0 6)
                         (make-piece W-PAWN 1 6)
                         (make-piece W-PAWN 2 6)
                         (make-piece W-PAWN 3 6)
                         (make-piece W-PAWN 4 6)
                         (make-piece W-PAWN 5 6)
                         (make-piece W-PAWN 6 6)
                         (make-piece W-PAWN 7 6)
                         (make-piece W-ROOK 0 7)
                         (make-piece W-KNIGHT 1 7)
                         (make-piece W-BISHOP 2 7)
                         (make-piece W-KING 3 7)
                         (make-piece W-QUEEN 4 7)
                         (make-piece W-BISHOP 5 7)
                         (make-piece W-KNIGHT 6 7)
                         (make-piece W-ROOK 7 7)
                         (make-piece B-PAWN 0 1)
                         (make-piece B-PAWN 1 1)
                         (make-piece B-PAWN 2 1)
                         (make-piece B-PAWN 3 1)
                         (make-piece B-PAWN 4 1)
                         (make-piece B-PAWN 5 1)
                         (make-piece B-PAWN 6 1)
                         (make-piece B-PAWN 7 1)
                         (make-piece B-ROOK 0 0)
                         (make-piece B-KNIGHT 1 0)
                         (make-piece B-BISHOP 2 0)
                         (make-piece B-KING 3 0)
                         (make-piece B-QUEEN 4 0)
                         (make-piece B-BISHOP 5 0)
                         (make-piece B-KNIGHT 6 0)
                         (make-piece B-ROOK 7 0)))
                            

; Initial empty world.
; Starting positions are the king for p1 and the topmost black tile for black,
; which is used to color the chessboard. After draw-map, it will be set to its king too.
(define INITIAL-WORLD (make-world PIECE-LIST
                                  3 7
                                  3 0
                                  #true
                                  #false))

; A white tile
(define W-TILE (square (/ WIDTH 8) 'solid 'white))

; A black tile
(define B-TILE (square (/ WIDTH 8) 'solid 'brown))

; The white cursor
(define W-CURSOR (square (/ WIDTH 8) 'outline (pen 'red 5 "dot" "round" "bevel")))

; The black cursor
(define B-CURSOR (square (/ WIDTH 8) 'outline (pen 'blue 5 "dot" "round" "bevel")))


; Nate's solution for the pieces list
(define test (list (list "1" 1) (list "2" 2) (list "3" 3)))



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


; draw-cursors: Coord x4 Image -> Image
; Draws the players cursors on a given image.
(define (draw-cursors x1 y1 x2 y2 img)
  (place-image W-CURSOR
               (tile-x x1)
               (tile-y y1)
               (place-image B-CURSOR
                            (tile-x x2)
                            (tile-y y2)
                            img)))


; draw-piece: Piece Image -> Image
; Draws a single piece on a given image.
(define (draw-piece p img)
  (place-image (piece-image p)
               (tile-x (piece-x p))
               (tile-y (piece-y p))
               img))


; draw-pieces: List<Piece> Image -> Image
; Draws a list of pieces on a given image.
(define (draw-pieces pl img)
  (cond [(empty? pl) img]
        [else (draw-pieces (rest pl) (draw-piece (first pl) img))]))


; draw-world: World -> Image
; Draws the chessboard, cursors and pieces.
(define (draw-world w)
  (draw-pieces (world-pieces w)
               (draw-cursors (world-pos1x w)
                             (world-pos1y w)
                             (world-pos2x w)
                             (world-pos2y w)
                             (draw-board 0 (empty-scene WIDTH HEIGHT)))))



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
    [(and (key=? key "w") (world-mov1 w) (< 0 (world-pos1y w)))
     (struct-copy world w [pos1y (- (world-pos1y w) 1)])]
    [(and (key=? key "s") (world-mov1 w) (> 7 (world-pos1y w)))
     (struct-copy world w [pos1y (+ (world-pos1y w) 1)])]
    [(and (key=? key "a") (world-mov1 w) (< 0 (world-pos1x w)))
     (struct-copy world w [pos1x (- (world-pos1x w) 1)])]
    [(and (key=? key "d") (world-mov1 w) (> 7 (world-pos1x w)))
     (struct-copy world w [pos1x (+ (world-pos1x w) 1)])]
    ; Player 1 end turn
    [(and (key=? key "shift") (world-mov1 w))
     (struct-copy world w [mov1 #false] [mov2 #true])]
    ; Player 2 movement
    [(and (key=? key "up") (world-mov2 w) (< 0 (world-pos2y w)))
     (struct-copy world w [pos2y (- (world-pos2y w) 1)])]
    [(and (key=? key "down") (world-mov2 w) (> 7 (world-pos2y w)))
     (struct-copy world w [pos2y (+ (world-pos2y w) 1)])]
    [(and (key=? key "left") (world-mov2 w) (< 0 (world-pos2x w)))
     (struct-copy world w [pos2x (- (world-pos2x w) 1)])]
    [(and (key=? key "right") (world-mov2 w) (> 7 (world-pos2x w)))
     (struct-copy world w [pos2x (+ (world-pos2x w) 1)])]
    ; Player 2 end turn
    [(and (key=? key "\r") (world-mov2 w))
     (struct-copy world w [mov1 #true] [mov2 #false])]
    ; Nothing
    [else w]))
  

; main: Nothing -> Nothing
; Launches big-bang.
(define (main _)
  (big-bang INITIAL-WORLD
    [name "Chess-expect!"]
    [to-draw draw-world]
    [on-key handle-key]))
