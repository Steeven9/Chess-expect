;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname chess-expect) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; PF1 final project: chess-expect!
; Author: Stefano Taillefert

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;
; Data definition
;;;;;;;;;;;;;;;;;;;;;;;;;

; A World is a (make-world scene pos1 pos2) where:
; - scene is an Image,
; - pos1, pos2 are List<Symbol>.
; Interpretation: the world status with the position of the two players'
; (white is 1 and black is 2) pointer.
(define-struct world [scene pos1 pos2])



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
(define INITIAL-WORLD (make-world (empty-scene WIDTH HEIGHT) (list 'D '1) (list 'B '8)))

; A white tile
(define W-TILE (square (/ WIDTH 8) 'solid 'white))

; A black tile
(define B-TILE (square (/ WIDTH 8) 'solid 'black))


; TODO: img of every piece ("img/thing.png")



;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;

; draw-board: World -> World
; Draws the inital chessboard.
(define (draw-board world)
  (place-image/align B-TILE
                     0 0 "left" "top" 
                     (place-image/align B-TILE
                                        200 0 "left" "top"
                                        (world-scene world))))    ; TODO: recursive


; TODO: helper func to retrieve x/y given a line/column


; main: Nothing -> Nothing
; Launches big-bang.
(define (main _)
  (big-bang INITIAL-WORLD
    [to-draw draw-board]))
;    [on-mouse handle-mouse]
;    [on-key handle-key]))



;;;;;;;;;;;;;;;;;;;;;;;;;
; Launch big-bang (for development purposes)
;;;;;;;;;;;;;;;;;;;;;;;;;

(main 0)