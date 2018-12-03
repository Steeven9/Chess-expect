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

(require 2htdp/image)
(require 2htdp/universe)

(require "pieces.rkt")



;;;;;;;;;;;;;;;;;;;;;;;;;
; Data definition
;;;;;;;;;;;;;;;;;;;;;;;;;

; A World is a (make-world scene pos1 pos2 mov1 mov2) where:
; - scene is an Image,
; - pos1, pos2 are NonNegIntegers,
; - mov1, mov2 are Boolean.
; Interpretation: the world status with the position of the two players'
; (white is 1 and black is 2) pointers and movement status (is he moving?).
(define-struct world [scene pos1x pos1y pos2x pos2y mov1 mov2])



;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;

; Screen width
(define WIDTH 800)

; Screen height
(define HEIGHT 800)

; Initial empty world.
; Starting positions are the king for p1 and the topmost black tile for black,
; which is used to color the chessboard. After draw-map, it will be set to its king too.
(define INITIAL-WORLD (make-world (empty-scene WIDTH HEIGHT)
                                  3 7
                                  1 0
                                  #false
                                  #false))

; A white tile
(define W-TILE (square (/ WIDTH 8) 'solid 'white))

; A black tile
(define B-TILE (square (/ WIDTH 8) 'solid 'black))

; The white cursor
(define W-CURSOR (square (/ WIDTH 8) 'outline (pen 'red 5 "dot" "round" "bevel")))

; The black cursor
(define B-CURSOR (square (/ WIDTH 8) 'outline (pen 'blue 5 "dot" "round" "bevel")))



;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;

; draw-board-line: World -> Image
; Draws one chessboard line.
(define (draw-board-line w)
  (cond [(< 7 (world-pos2x w)) (world-scene w)]
        [else
         (place-image/align B-TILE
                            (tile-x (world-pos2x w))
                            (tile-y (world-pos2y w))
                            "left" "top" 
                            (draw-board-line (make-world (world-scene w)
                                                         (world-pos1x w)
                                                         (world-pos1y w)
                                                         (+ 2 (world-pos2x w))
                                                         (world-pos2y w)
                                                         #false
                                                         #false)))]))


; draw-board: World -> Image
; Draws the whole chessboard and cursors, then set the cursors to starting position
; and enable p1 movement.
(define (draw-board w)
  (cond [(< 7 (world-pos2y w))
         (draw-cursors (make-world (draw-board-line w)
                                   (world-pos1x w)
                                   (world-pos1y w)
                                   3
                                   0
                                   #true
                                   #false))]
        [(member (world-pos2y w) '(1 3 5 7))
         (draw-board (make-world (draw-board-line w)
                                 (world-pos1x w)
                                 (world-pos1y w)
                                 0
                                 (+ 1 (world-pos2y w))
                                 #false
                                 #false))]
        [else (draw-board (make-world (draw-board-line w)
                                      (world-pos1x w)
                                      (world-pos1y w)
                                      1
                                      (+ 1 (world-pos2y w))
                                      #false
                                      #false))]))


; draw-cursors: World -> Image
; Draws the players cursors.
(define (draw-cursors w)
  (place-image/align W-CURSOR
                     (tile-x (world-pos1x w))
                     (tile-y (world-pos1y w))
                     "left" "top"
                     (place-image/align B-CURSOR
                                        (tile-x (world-pos2x w))
                                        (tile-y (world-pos2y w))
                                        "left" "top"
                                        (world-scene w))))


; tile-x: NonNegInteger -> NonNegNumber
; Return the x coordinate corresponding to the given column number.
(define (tile-x col)
  (* col (/ WIDTH 8)))

; Tests
(check-expect (tile-x 0) 0)
(check-expect (tile-x 1) 100)
(check-expect (tile-x 6) 600)

  
; tile-y: NonNegInteger -> NonNegNumber
; Return the y coordinate corresponding to the given row number.
(define (tile-y row)
  (* row (/ HEIGHT 8)))

; Tests
(check-expect (tile-y 0) 0)
(check-expect (tile-y 1) 100)
(check-expect (tile-y 6) 600)
  

; main: Nothing -> Nothing
; Launches big-bang.
(define (main _)
  (big-bang INITIAL-WORLD
    [name "Chess-expect!"]
    [to-draw draw-board]))
;    [on-mouse handle-mouse]
;    [on-key handle-key]))
