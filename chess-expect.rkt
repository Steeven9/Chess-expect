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

; Initial empty world.
; Starting positions are the king for p1 and the topmost black tile for black,
; which is used to color the chessboard. After draw-map, it will be set to its king too.
(define INITIAL-WORLD (make-world (empty-scene WIDTH HEIGHT)
                                  3 7
                                  1 0
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



;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;

; draw-board-line: World -> Image
; Draws one chessboard line.
(define (draw-board-line w)
  (cond [(< 7 (world-pos2x w)) (world-scene w)]
        [else
         (place-image B-TILE
                      (tile-x (world-pos2x w))
                      (tile-y (world-pos2y w))
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
                                 #true
                                 #false))]
        [else (draw-board (make-world (draw-board-line w)
                                      (world-pos1x w)
                                      (world-pos1y w)
                                      1
                                      (+ 1 (world-pos2y w))
                                      #true
                                      #false))]))


; draw-cursors: World -> Image
; Draws the players cursors.
(define (draw-cursors w)
  (place-image W-CURSOR
               (tile-x (world-pos1x w))
               (tile-y (world-pos1y w))
               (place-image B-CURSOR
                            (tile-x (world-pos2x w))
                            (tile-y (world-pos2y w))
                            (world-scene w))))


; tile-x: NonNegInteger -> NonNegNumber
; Return the x coordinate corresponding to the given column number.
(define (tile-x col)
  (+ (/ WIDTH 16) (* col (/ WIDTH 8))))

; Tests
(check-expect (tile-x 0) 50)
(check-expect (tile-x 1) 150)
(check-expect (tile-x 6) 650)

  
; tile-y: NonNegInteger -> NonNegNumber
; Return the y coordinate corresponding to the given row number.
(define (tile-y row)
  (+ (/ HEIGHT 16) (* row (/ HEIGHT 8))))

; Tests
(check-expect (tile-y 0) 50)
(check-expect (tile-y 1) 150)
(check-expect (tile-y 6) 650)


; handle-key: World Key -> World
; Handles keypresses.
(define (handle-key w key)
  (cond [(key=? key "escape") (draw-board INITIAL-WORLD)]
        [(and (key=? key "w") (world-mov1 w)) (draw-board INITIAL-WORLD)]
        [(and (key=? key "up") (world-mov2 w)) (draw-board INITIAL-WORLD)]
        [else w]))
  

; main: Nothing -> Nothing
; Launches big-bang.
(define (main _)
  (big-bang INITIAL-WORLD
    [name "Chess-expect!"]
    [to-draw draw-board]
    [on-key handle-key]))
