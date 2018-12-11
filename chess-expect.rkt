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

; A World is a (make-world pieces pos1x pos1y pos2x pos2y turn pick1 pick2, movingx, movingy) where:
; - pieces is a List<Piece>,
; - pos1x, pos1y, pos2x, pos2y are Coord,
; - turn is either 0 (menu), 1 or 2,
; - pick1, pick2 are Boolean,
; - movingx, movingy are Coord.
; Interpretation: the world status with the pieces list, the position of the two
; players' (white is 1 and black is 2) pointers and movement status.
(define-struct world [pieces pos1x pos1y pos2x pos2y turn pick1 pick2 movingx movingy] #:transparent)

; A Piece is a (make-piece color type x y) where:
; - color is either 'white or 'black,
; - type is one of 'Pawn, 'Rook, 'Knight, 'Bishop, 'King, 'Queen,
; - x, y are Coord.
; Interpretation: a piece with its data and coordinates.
(define-struct piece [color type x y] #:transparent)


;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;

; Screen width
(define WIDTH 800)

; Screen height
(define HEIGHT 800)

; The default piece list
(define PIECE-LIST (list (make-piece 'white 'Pawn 0 6)
                         (make-piece 'white 'Pawn 1 6)
                         (make-piece 'white 'Pawn 2 6)
                         (make-piece 'white 'Pawn 3 6)
                         (make-piece 'white 'Pawn 4 6)
                         (make-piece 'white 'Pawn 5 6)
                         (make-piece 'white 'Pawn 6 6)
                         (make-piece 'white 'Pawn 7 6)
                         (make-piece 'white 'Rook 0 7)
                         (make-piece 'white 'Knight 1 7)
                         (make-piece 'white 'Bishop 2 7)
                         (make-piece 'white 'King 3 7)
                         (make-piece 'white 'Queen 4 7)
                         (make-piece 'white 'Bishop 5 7)
                         (make-piece 'white 'Knight 6 7)
                         (make-piece 'white 'Rook 7 7)
                         (make-piece 'black 'Pawn 0 1)
                         (make-piece 'black 'Pawn 1 1)
                         (make-piece 'black 'Pawn 2 1)
                         (make-piece 'black 'Pawn 3 1)
                         (make-piece 'black 'Pawn 4 1)
                         (make-piece 'black 'Pawn 5 1)
                         (make-piece 'black 'Pawn 6 1)
                         (make-piece 'black 'Pawn 7 1)
                         (make-piece 'black 'Rook 0 0)
                         (make-piece 'black 'Knight 1 0)
                         (make-piece 'black 'Bishop 2 0)
                         (make-piece 'black 'King 3 0)
                         (make-piece 'black 'Queen 4 0)
                         (make-piece 'black 'Bishop 5 0)
                         (make-piece 'black 'Knight 6 0)
                         (make-piece 'black 'Rook 7 0)))

; Initial world.
; Starting positions are the king's coordinates, player1 (white) moves first.
(define INITIAL-WORLD (make-world PIECE-LIST
                                  3 7
                                  3 0
                                  1
                                  #false
                                  #false
                                  0
                                  0))

; Initial menu.
(define INITIAL-WORLD-MENU (struct-copy world INITIAL-WORLD [turn 0]))

; A white tile
(define W-TILE (square (/ WIDTH 8) 'solid 'white))

; A black tile
(define B-TILE (square (/ WIDTH 8) 'solid 'brown))

; The white cursor
(define W-CURSOR (square (/ WIDTH 8) 'outline (pen 'red 5 "solid" "round" "bevel")))

; The white active cursor (when moving a piece)
(define W-CURSOR-ACTIVE (square (/ WIDTH 8) 'outline (pen 'red 5 "dot" "round" "round")))

; The black cursor
(define B-CURSOR (square (/ WIDTH 8) 'outline (pen 'blue 5 "solid" "round" "bevel")))

; The black active cursor (when moving a piece)
(define B-CURSOR-ACTIVE (square (/ WIDTH 8) 'outline (pen 'blue 5 "dot" "round" "round")))

; The main menu text
(define MENU-TEXT (overlay (above (text "Welcome to Chess-expect!" 32 'black)
                                  (text "Press escape to begin." 24 'black))
                           (rectangle 500 300 'solid 'white)))

; Copyright text at the bottom
(define MENU-COPYRIGHT (text "Made by Stefano Taillefert - PF1 Final Project" 16 'black))


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
         (draw-board (add1 y) (draw-board-line 0 y img))]
        [else (draw-board (add1 y) (draw-board-line 1 y img))]))


; draw-cursors: World Image -> Image
; Draws the players cursors on a given image.
(define (draw-cursors w img)
  (if (= 1 (world-turn w))
      (if (world-pick1 w)
          (place-image W-CURSOR-ACTIVE
                       (tile-x (world-pos1x w))
                       (tile-y (world-pos1y w))
                       (place-image W-CURSOR-ACTIVE
                                    (tile-x (world-movingx w))
                                    (tile-y (world-movingy w))
                                    img))
          (place-image W-CURSOR
                       (tile-x (world-pos1x w))
                       (tile-y (world-pos1y w))
                       img))
      (if (world-pick2 w)
          (place-image B-CURSOR-ACTIVE
                       (tile-x (world-pos2x w))
                       (tile-y (world-pos2y w))
                       (place-image B-CURSOR-ACTIVE
                                    (tile-x (world-movingx w))
                                    (tile-y (world-movingy w))
                                    img))
          (place-image B-CURSOR
                       (tile-x (world-pos2x w))
                       (tile-y (world-pos2y w))
                       img))))


; draw-piece: Piece Image -> Image
; Draws a single piece on a given image.
(define (draw-piece p img)
  (local [(define source (string-append "img/"
                                        (symbol->string (piece-color p))
                                        (symbol->string (piece-type p))
                                        ".png"))]
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
  (place-image/align (if (= 1 (world-turn w))
                         (if (world-pick1 w)
                             (text "Player 1: choose destination" 24 'black)
                             (text "Player 1: choose piece" 24 'black))
                         (if (world-pick2 w)
                             (text "Player 2: choose destination" 24 'black)
                             (text "Player 2: choose piece" 24 'black)))
                     ; Bottom left
                     25
                     850
                     "left"
                     "middle"
                     (place-image/align (above (if (= 1 (world-turn w))
                                                   (if (world-pick1 w)
                                                       (text "WASD to move, space to drop" 24 'black)
                                                       (text "WASD to move, space to pick" 24 'black))
                                                   (if (world-pick2 w)
                                                       (text "Arrows to move, space to drop" 24 'black)
                                                       (text "Arrows to move, space to pick" 24 'black)))
                                               (text "Escape to reset" 24 'black))
                                        ; Bottom right
                                        775
                                        850
                                        "right"
                                        "middle"
                                        img)))


; draw-world: World -> Image
; Draws the chessboard, cursors and pieces.
(define (draw-world w)
  (if (= 0 (world-turn w))
      (place-image MENU-COPYRIGHT
                   400
                   850
                   (overlay MENU-TEXT
                            (draw-board 0
                                        (empty-scene WIDTH (+ 100 HEIGHT)))))
      (draw-text w
                 (draw-pieces (world-pieces w)
                              (draw-cursors w
                                            (draw-board 0
                                                        (empty-scene WIDTH (+ 100 HEIGHT))))))))


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


;;;; Helper functions ;;;;

; get-piece: List<Piece> Coord Coord -> Option<Piece>
; Returns the piece at a given location or #false if there are none.
(define (get-piece pl x y)
  (cond [(empty? pl) #false]
        [(and (= x (piece-x (first pl))) (= y (piece-y (first pl)))) (first pl)]
        [else (get-piece (rest pl) x y)]))

; Tests 
(check-expect (get-piece PIECE-LIST 7 1) (make-piece 'black 'Pawn 7 1))
(check-expect (get-piece PIECE-LIST 0 0) (make-piece 'black 'Rook 0 0))
(check-expect (get-piece PIECE-LIST 4 5) #false)


; movement-valid: World Coord Coord -> Boolean
; Returns #true if the movement done by a piece is valid, #false otherwise.
(define (movement-valid w)
  ; Check if space is occupied
  (if (= 1 (world-turn w))
      (if (piece? (get-piece (world-pieces w) (world-pos1x w) (world-pos1y w)))
          #false
          #true)
      (if (piece? (get-piece (world-pieces w) (world-pos2x w) (world-pos2y w)))
          #false
          #true)))

; Tests
(check-expect (movement-valid INITIAL-WORLD) #false)
(check-expect (movement-valid (make-world PIECE-LIST
                                          4
                                          5
                                          3
                                          0
                                          1
                                          #false
                                          #false
                                          0
                                          7))
              #true)
(check-expect (movement-valid (make-world PIECE-LIST
                                          4
                                          5
                                          3
                                          0
                                          2
                                          #false
                                          #false
                                          1
                                          0))
              #false)


; would-eat: World Coord Coord -> Boolean
(define (would-eat w)
  #false)

; Tests
; TODO


; eat-piece: World Coord Cord -> World
(define (eat-piece w)
  (if (= 1 (world-turn w))
      (struct-copy world w
                   [pieces (cons (make-piece 'white
                                             (piece-type (get-piece (world-pieces w)
                                                                    (world-movingx w)
                                                                    (world-movingy w)))
                                             (world-pos1x w)
                                             (world-pos1y w))
                                 (remove (get-piece
                                          (world-pieces w)
                                          (world-movingx w)
                                          (world-movingy w))
                                         (world-pieces w)))]
                   [turn 2]
                   [pick1 #false])
      (struct-copy world w
                   [pieces (cons (make-piece 'black
                                             (piece-type (get-piece (world-pieces w)
                                                                    (world-movingx w)
                                                                    (world-movingy w)))
                                             (world-pos2x w)
                                             (world-pos2y w))
                                 (remove (get-piece
                                          (world-pieces w)
                                          (world-movingx w)
                                          (world-movingy w))
                                         (world-pieces w)))]
                   [turn 1]
                   [pick2 #false])))

; Tests
; TODO


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

    ; Pick and drop piece (spacebar)
    ; TODO:
    ; Movement type allowed? -> movement-valid func
    ; Eat a piece func
    [(key=? key " ")
     (cond
       ; Player 1 part
       [(= 1 (world-turn w))
            (if (world-pick1 w)
                ; If player has picked a piece, check move
                (if (movement-valid w)
                    ; Check if move would eat a piece
                    (if (would-eat w)
                        ; Eat piece and end turn
                        (eat-piece w)
                        ; Empty space, drop the piece (remove the old and append the new) and end turn
                        (struct-copy world w
                                     [pieces (cons (make-piece 'white
                                                               (piece-type (get-piece (world-pieces w)
                                                                                      (world-movingx w)
                                                                                      (world-movingy w)))
                                                               (world-pos1x w)
                                                               (world-pos1y w))
                                                   (remove (get-piece
                                                            (world-pieces w)
                                                            (world-movingx w)
                                                            (world-movingy w))
                                                           (world-pieces w)))]
                                     [turn 2]
                                     [pick1 #false]))
                    ; Invalid movement
                    w)
                (if (piece? (get-piece (world-pieces w) (world-pos1x w) (world-pos1y w)))
                    ; Remember the piece the player picked up
                    (struct-copy world w
                                 [pick1 #true]
                                 [movingx (world-pos1x w)]
                                 [movingy (world-pos1y w)])
                    w))]
           
           ; Player 2 part
           [else (if (world-pick2 w)
                ; If player has picked a piece, check move
                (if (movement-valid w)
                    ; Check if move would eat a piece
                    (if (would-eat w)
                        ; Eat piece and end turn
                        (eat-piece w)
                        ; Empty space, drop the piece (remove the old and append the new) and end turn
                        (struct-copy world w
                                     [pieces (cons (make-piece 'black
                                                               (piece-type (get-piece (world-pieces w)
                                                                                      (world-movingx w)
                                                                                      (world-movingy w)))
                                                               (world-pos2x w)
                                                               (world-pos2y w))
                                                   (remove (get-piece
                                                            (world-pieces w)
                                                            (world-movingx w)
                                                            (world-movingy w))
                                                           (world-pieces w)))]
                                     [turn 1]
                                     [pick2 #false]))
                    ; Invalid movement
                    w)
                (if (piece? (get-piece (world-pieces w) (world-pos2x w) (world-pos2y w)))
                    ; Remember the piece the player picked up
                    (struct-copy world w
                                 [pick2 #true]
                                 [movingx (world-pos2x w)]
                                 [movingy (world-pos2y w)])
                    w))])]
    ; Nothing
    [else w]))
  

; main: Nothing -> Nothing
; Launches big-bang.
(define (main _)
  (big-bang INITIAL-WORLD-MENU
    [state #true]
    [name "Chess-expect!"]
    [to-draw draw-world]
    [on-key handle-key]))


; Launch big-bang
(main 0)