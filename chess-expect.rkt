;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname chess-expect) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; PF1 final project: chess-expect!
; Main file
; Author: Stefano Taillefert, 16.12.2018

;;;;;;;;;;;;;;;;;;;;;;;;;
; Coordinates system
;;;;;;;;;;;;;;;;;;;;;;;;;

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

; A Position (Pos) is a (make-pos x y) where:
; - x, y are NonNegativeInteger between 0 and 7 (included).
; Interpretation: a coordinate on the chessboard.
(define-struct pos [x y] #:transparent)

; A World is a (make-world pieces pos1 pos2 turn pick1 pick2 moving) where:
; - pieces is a List<Piece>,
; - pos1, pos2 are Position,
; - turn is either 0 (menu), 1 (player 1), 2 (player 2), 3 (p1 wins) or 4 (p2 wins),
; - pick1, pick2 are Boolean,
; - moving, is a Position.
; Interpretation: the world status with the pieces list, the position of the two
; players' (white is 1 and black is 2) pointers and movement status.
(define-struct world [pieces pos1 pos2 turn pick1 pick2 moving] #:transparent)

; A Piece is a (make-piece color type pos) where:
; - color is either 'white or 'black,
; - type is one of 'Pawn, 'Rook, 'Knight, 'Bishop, 'King, 'Queen,
; - pos is a Position.
; Interpretation: a piece with its data and coordinates.
(define-struct piece [color type pos] #:transparent)


;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;

; Screen width
(define WIDTH 800)

; Screen height
(define HEIGHT 800)

; The default piece list
(define PIECE-LIST (list (make-piece 'white 'Pawn (make-pos 0 6))
                         (make-piece 'white 'Pawn (make-pos 1 6))
                         (make-piece 'white 'Pawn (make-pos 2 6))
                         (make-piece 'white 'Pawn (make-pos 3 6))
                         (make-piece 'white 'Pawn (make-pos 4 6))
                         (make-piece 'white 'Pawn (make-pos 5 6))
                         (make-piece 'white 'Pawn (make-pos 6 6))
                         (make-piece 'white 'Pawn (make-pos 7 6))
                         (make-piece 'white 'Rook (make-pos 0 7))
                         (make-piece 'white 'Knight (make-pos 1 7))
                         (make-piece 'white 'Bishop (make-pos 2 7))
                         (make-piece 'white 'King (make-pos 3 7))
                         (make-piece 'white 'Queen (make-pos 4 7))
                         (make-piece 'white 'Bishop (make-pos 5 7))
                         (make-piece 'white 'Knight (make-pos 6 7))
                         (make-piece 'white 'Rook (make-pos 7 7))
                         (make-piece 'black 'Pawn (make-pos 0 1))
                         (make-piece 'black 'Pawn (make-pos 1 1))
                         (make-piece 'black 'Pawn (make-pos 2 1))
                         (make-piece 'black 'Pawn (make-pos 3 1))
                         (make-piece 'black 'Pawn (make-pos 4 1))
                         (make-piece 'black 'Pawn (make-pos 5 1))
                         (make-piece 'black 'Pawn (make-pos 6 1))
                         (make-piece 'black 'Pawn (make-pos 7 1))
                         (make-piece 'black 'Rook (make-pos 0 0))
                         (make-piece 'black 'Knight (make-pos 1 0))
                         (make-piece 'black 'Bishop (make-pos 2 0))
                         (make-piece 'black 'King (make-pos 3 0))
                         (make-piece 'black 'Queen (make-pos 4 0))
                         (make-piece 'black 'Bishop (make-pos 5 0))
                         (make-piece 'black 'Knight (make-pos 6 0))
                         (make-piece 'black 'Rook (make-pos 7 0))))

; Initial world.
; Starting positions are the king's coordinates, player1 (white) moves first.
(define INITIAL-WORLD (make-world PIECE-LIST
                                  (make-pos 3 7)
                                  (make-pos 3 0)
                                  1
                                  #false
                                  #false
                                  (make-pos 0 0)))

; Initial menu
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
(define MENU-TEXT (bitmap "img/menuText.png"))

; The main menu logo
(define MENU-LOGO (bitmap "img/logo.png"))

; Copyright text at the bottom
(define MENU-COPYRIGHT (text "Stefano Taillefert - PF1 Final Project" 16 'black))

; Victory image for player 1
(define P1-WIN (above (overlay (text "Player 1 wins!" 32 'black)
                               (rectangle 500 125 'solid 'white))
                      (overlay (text "Yay! Press escape to restart." 24 'black)
                               (rectangle 500 100 'solid 'white))))

; Victory image for player 2
(define P2-WIN (above (overlay (text "Player 2 wins!" 32 'black)
                               (rectangle 500 125 'solid 'white))
                      (overlay (text "Yay! Press escape to restart." 24 'black)
                               (rectangle 500 100 'solid 'white))))

; "Thank you" text at the bottom
(define THANKS-TEXT (text "Thank you for your time, have a nice day!" 16 'black))


;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Drawing functions ;;;;

; draw-board-line: Position Image -> Image
; Draws one chessboard line.
(define (draw-board-line pos img)
  (cond [(< 7 (pos-x pos)) img]
        [else
         (place-image B-TILE
                      (tile-x pos)
                      (tile-y pos)
                      (draw-board-line (make-pos (+ 2 (pos-x pos)) (pos-y pos)) img))]))


; draw-board: NonNegativeInteger Image -> Image
; Draws the whole chessboard.
(define (draw-board y img)
  (cond [(< 7 y) img]
        [(member y '(1 3 5 7))
         (draw-board (add1 y) (draw-board-line (make-pos 0 y) img))]
        [else (draw-board (add1 y) (draw-board-line (make-pos 1 y) img))]))


; draw-cursors: World Image -> Image
; Draws the players cursors on a given image.
(define (draw-cursors w img)
  (if (= 1 (world-turn w))
      (if (world-pick1 w)
          (place-image W-CURSOR-ACTIVE
                       (tile-x (world-pos1 w))
                       (tile-y (world-pos1 w))
                       (place-image W-CURSOR-ACTIVE
                                    (tile-x (world-moving w))
                                    (tile-y (world-moving w))
                                    img))
          (place-image W-CURSOR
                       (tile-x (world-pos1 w))
                       (tile-y (world-pos1 w))
                       img))
      (if (world-pick2 w)
          (place-image B-CURSOR-ACTIVE
                       (tile-x (world-pos2 w))
                       (tile-y (world-pos2 w))
                       (place-image B-CURSOR-ACTIVE
                                    (tile-x (world-moving w))
                                    (tile-y (world-moving w))
                                    img))
          (place-image B-CURSOR
                       (tile-x (world-pos2 w))
                       (tile-y (world-pos2 w))
                       img))))


; draw-piece: Piece Image -> Image
; Draws a single piece on a given image.
(define (draw-piece p img)
  (local [(define source (string-append "img/"
                                        (symbol->string (piece-color p))
                                        (symbol->string (piece-type p))
                                        ".png"))]
    (place-image (bitmap/file source)
                 (tile-x (piece-pos p))
                 (tile-y (piece-pos p))
                 img)))


; draw-pieces: List<Piece> Image -> Image
; Draws a list of pieces on a given image.
(define (draw-pieces pl img)
  (cond [(empty? pl) img]
        [else (draw-pieces (rest pl) (draw-piece (first pl) img))]))


; draw-text: World Image -> Image
; Prints the text according to the given world.
(define (draw-text w img)
  (cond [(= 3 (world-turn w))
         (place-image P1-WIN
                      400
                      400
                      (place-image THANKS-TEXT
                                   400
                                   850
                                   img))]
        [(= 4 (world-turn w))
         (place-image P2-WIN
                      400
                      400
                      (place-image THANKS-TEXT
                                   400
                                   850
                                   img))]
        [else
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
                                                      (text "Escape to restart match" 24 'black))
                                               ; Bottom right
                                               775
                                               850
                                               "right"
                                               "middle"
                                               img))]))


; draw-world: World -> Image
; Draws the chessboard, cursors, pieces and text.
(define (draw-world w)
  (if (= 0 (world-turn w))
      (place-image MENU-COPYRIGHT
                   400
                   850
                   (place-image MENU-LOGO
                                400
                                350
                                (place-image MENU-TEXT
                                             400
                                             450
                                             (draw-board 0
                                                         (empty-scene WIDTH (+ 100 HEIGHT))))))
      (draw-text w
                 (draw-pieces (world-pieces w)
                              (draw-cursors w
                                            (draw-board 0
                                                        (empty-scene WIDTH (+ 100 HEIGHT))))))))


;;;; Calculation functions ;;;;

; tile-x: Position -> NonNegNumber
; Returns the x coordinate corresponding to the given column number.
(define (tile-x pos)
  (+ (/ WIDTH 16) (* (pos-x pos) (/ WIDTH 8))))

; Tests
(check-expect (tile-x (make-pos 0 5)) 50)
(check-expect (tile-x (make-pos 1 0)) 150)
(check-expect (tile-x (make-pos 6 6)) 650)

  
; tile-y: Position -> NonNegNumber
; Returns the y coordinate corresponding to the given row number.
(define (tile-y pos)
  (+ (/ HEIGHT 16) (* (pos-y pos) (/ HEIGHT 8))))

; Tests
(check-expect (tile-y (make-pos 1 0)) 50)
(check-expect (tile-y (make-pos 9 1)) 150)
(check-expect (tile-y (make-pos 4 6)) 650)


; get-x-distance: World -> NonNegativeInteger
; Returns the x distance (in squares) between the cursor
; and the selected piece.
(define (get-x-distance w)
  (if (world-pick1 w)
      (abs (- (pos-x (world-pos1 w)) (pos-x (world-moving w))))
      (abs (- (pos-x (world-pos2 w)) (pos-x (world-moving w))))))

; Tests
(check-expect (get-x-distance (make-world PIECE-LIST
                                          (make-pos 3 5)
                                          (make-pos 0 6)
                                          1
                                          #true
                                          #false
                                          (make-pos 3 6)))
              0)
(check-expect (get-x-distance (make-world PIECE-LIST
                                          (make-pos 5 5)
                                          (make-pos 0 6)
                                          2
                                          #false
                                          #true
                                          (make-pos 5 6)))
              5)


; get-y-distance: World -> NonNegativeInteger
; Returns the y distance (in squares) between the cursor
; and the selected piece.
(define (get-y-distance w)
  (if (world-pick1 w)
      (abs (- (pos-y (world-pos1 w)) (pos-y (world-moving w))))
      (abs (- (pos-y (world-pos2 w)) (pos-y (world-moving w))))))

; Tests
(check-expect (get-y-distance (make-world PIECE-LIST
                                          (make-pos 3 5)
                                          (make-pos 0 6)
                                          1
                                          #true
                                          #false
                                          (make-pos 3 6)))
              1)
(check-expect (get-y-distance (make-world PIECE-LIST
                                          (make-pos 3 5)
                                          (make-pos 0 3)
                                          2
                                          #false
                                          #true
                                          (make-pos 5 6)))
              3)


;;;; Helper functions ;;;;

; get-piece: List<Piece> Position -> Option<Piece>
; Returns the piece at a given location or #false if there are none.
(define (get-piece pl pos)
  (cond [(empty? pl) #false]
        [(and (= (pos-x pos) (pos-x (piece-pos (first pl))))
              (= (pos-y pos) (pos-y (piece-pos (first pl)))))
         (first pl)]
        [else (get-piece (rest pl) pos)]))

; Tests 
(check-expect (get-piece PIECE-LIST (make-pos 7 1)) (make-piece 'black 'Pawn (make-pos 7 1)))
(check-expect (get-piece PIECE-LIST (make-pos 0 0)) (make-piece 'black 'Rook (make-pos 0 0)))
(check-expect (get-piece PIECE-LIST (make-pos 4 5)) #false)


; can-drop-here: World -> Boolean
; Returns #true if the space is free or occupied by an enemy, #false otherwise.
(define (can-drop-here w)
  (cond [(= 1 (world-turn w))
         (if (piece? (get-piece (world-pieces w) (world-pos1 w)))
             (if (symbol=? 'white (piece-color (get-piece (world-pieces w)
                                                          (world-pos1 w))))
                 (if (eq? (get-piece (world-pieces w) (world-pos1 w))
                          (get-piece (world-pieces w) (world-moving w)))
                     #true
                     #false)
                 #true)
             #true)]
        [else  (if (piece? (get-piece (world-pieces w) (world-pos2 w)))
                   (if (symbol=? 'black (piece-color (get-piece (world-pieces w)
                                                                (world-pos2 w))))
                       (if (eq? (get-piece (world-pieces w) (world-pos2 w))
                                (get-piece (world-pieces w) (world-moving w)))
                           #true
                           #false)
                       #true)
                   #true)]))

; Tests
(check-expect (can-drop-here INITIAL-WORLD) #false)
(check-expect (can-drop-here (make-world PIECE-LIST
                                         (make-pos 4 5)
                                         (make-pos 3 0)
                                         1
                                         #false
                                         #false
                                         (make-pos 0 7)))
              #true)
(check-expect (can-drop-here (make-world PIECE-LIST
                                         (make-pos 4 5)
                                         (make-pos 3 0)
                                         2
                                         #false
                                         #false
                                         (make-pos 1 0)))
              #false)


; would-eat: World -> Boolean
; Returns #true if the current move would eat a piece, #false otherwise.
(define (would-eat w)
  (cond [(= 1 (world-turn w))
         (if (piece? (get-piece (world-pieces w) (world-pos1 w)))
             (if (symbol=? 'white (piece-color (get-piece (world-pieces w)
                                                          (world-pos1 w))))
                 #false
                 #true)
             #false)]
        [else  (if (piece? (get-piece (world-pieces w) (world-pos2 w)))
                   (if (symbol=? 'black (piece-color (get-piece (world-pieces w)
                                                                (world-pos2 w))))
                       #false
                       #true)
                   #false)]))

; Tests
(check-expect (would-eat INITIAL-WORLD) #false)
(check-expect (would-eat (make-world PIECE-LIST
                                     (make-pos 4 5)
                                     (make-pos 0 6)
                                     2
                                     #false
                                     #false
                                     (make-pos 0 1)))
              #true)


; eat-piece: World -> World
; Returns a World without the piece that got eaten.
(define (eat-piece w)
  (if (= 1 (world-turn w))
      (if (symbol=? 'King
                    (piece-type (get-piece (world-pieces w) (world-pos1 w))))
          (struct-copy world w
                       [pieces (cons (make-piece 'white
                                                 (piece-type (get-piece (world-pieces w)
                                                                        (world-moving w)))
                                                 (world-pos1 w))
                                     (remove (get-piece (world-pieces w)
                                                        (world-pos1 w))
                                             (remove (get-piece (world-pieces w)
                                                                (world-moving w))
                                                     (world-pieces w))))]
                       [turn 3]
                       [pick1 #false])
          (struct-copy world w
                       [pieces (cons (make-piece 'white
                                                 (piece-type (get-piece (world-pieces w)
                                                                        (world-moving w)))
                                                 (world-pos1 w))
                                     (remove (get-piece (world-pieces w)
                                                        (world-pos1 w))
                                             (remove (get-piece (world-pieces w)
                                                                (world-moving w))
                                                     (world-pieces w))))]
                       [turn 2]
                       [pick1 #false]))
      (if (symbol=? 'King
                    (piece-type (get-piece (world-pieces w) (world-pos2 w))))
          (struct-copy world w
                       [pieces (cons (make-piece 'black
                                                 (piece-type (get-piece (world-pieces w)
                                                                        (world-moving w)))
                                                 (world-pos2 w))
                                     (remove (get-piece (world-pieces w)
                                                        (world-pos2 w))
                                             (remove (get-piece
                                                      (world-pieces w)
                                                      (world-moving w))
                                                     (world-pieces w))))]
                       [turn 4]
                       [pick2 #false])
          (struct-copy world w
                       [pieces (cons (make-piece 'black
                                                 (piece-type (get-piece (world-pieces w)
                                                                        (world-moving w)))
                                                 (world-pos2 w))
                                     (remove (get-piece (world-pieces w)
                                                        (world-pos2 w))
                                             (remove (get-piece
                                                      (world-pieces w)
                                                      (world-moving w))
                                                     (world-pieces w))))]
                       [turn 1]
                       [pick2 #false]))))


;;;; Big-bang and handlers ;;;;

; handle-key: World Key -> World
; Handles keypresses.
(define (handle-key w key)
  (cond
    ; Reset
    [(key=? key "escape")
     INITIAL-WORLD]

    ; Player 1 movement
    [(= 1 (world-turn w))
     (if (world-pick1 w)
         ; Movement with piece selected
         (cond [(key=? key " ")
                ; Try to drop the piece
                (if (can-drop-here w)
                    (if (would-eat w)
                        (eat-piece w)
                        (if (and (= 0 (get-x-distance w)) (= 0 (get-y-distance w)))
                            (struct-copy world w [pick1 #false])
                            (struct-copy world w
                                         [pieces (cons (make-piece 'white
                                                                   (piece-type (get-piece (world-pieces w)
                                                                                          (world-moving w)))
                                                                   (world-pos1 w))
                                                       (remove (get-piece
                                                                (world-pieces w)
                                                                (world-moving w))
                                                               (world-pieces w)))]
                                         [turn 2]
                                         [pick1 #false])))
                    w)]
               ; Move the piece around
               [(symbol=? 'Pawn (piece-type (get-piece (world-pieces w) (world-moving w))))
                (cond [(key=? key "w")
                       (if (= 6 (pos-y (world-moving w)))
                           (if (and (> 2 (get-y-distance w)) (< 0 (pos-y (world-pos1 w))))
                               (struct-copy world w [pos1 (make-pos (pos-x (world-pos1 w))
                                                                    (sub1 (pos-y (world-pos1 w))))])
                               w)
                           (if (and (> 1 (get-y-distance w)) (< 0 (pos-y (world-pos1 w))))
                               (struct-copy world w [pos1 (make-pos (pos-x (world-pos1 w))
                                                                    (sub1 (pos-y (world-pos1 w))))])
                               w))]
                      [(key=? key "s")
                       (if (and (< (pos-y (world-pos1 w)) (pos-y (world-moving w)))
                                (> 7 (pos-y (world-pos1 w))))
                           (struct-copy world w [pos1 (make-pos (pos-x (world-pos1 w))
                                                                (add1 (pos-y (world-pos1 w))))])
                           w)]
                      [else w])]
               [(symbol=? 'King (piece-type (get-piece (world-pieces w) (world-moving w))))
                (cond [(key=? key "w")
                       (if (and (or (> 1 (get-y-distance w))
                                    (> (pos-y (world-pos1 w)) (pos-y (world-moving w))))
                                (< 0 (pos-y (world-pos1 w))))
                           (struct-copy world w [pos1 (make-pos (pos-x (world-pos1 w))
                                                                (sub1 (pos-y (world-pos1 w))))])
                           w)]
                      [(key=? key "a")
                       (if (and (or (> 1 (get-x-distance w))
                                    (> (pos-x (world-pos1 w)) (pos-x (world-moving w))))
                                (< 0 (pos-x (world-pos1 w))))
                           (struct-copy world w [pos1 (make-pos (sub1 (pos-x (world-pos1 w)))
                                                                (pos-y (world-pos1 w)))])
                           w)]
                      [(key=? key "s")
                       (if (and (or (> 1 (get-y-distance w))
                                    (< (pos-y (world-pos1 w)) (pos-y (world-moving w))))
                                (> 7 (pos-y (world-pos1 w))))
                           (struct-copy world w [pos1 (make-pos (pos-x (world-pos1 w))
                                                                (add1 (pos-y (world-pos1 w))))])
                           w)]
                      [(key=? key "d")
                       (if (and (or (> 1 (get-x-distance w))
                                    (< (pos-x (world-pos1 w)) (pos-x (world-moving w))))
                                (> 7 (pos-x (world-pos1 w))))
                           (struct-copy world w [pos1 (make-pos (add1 (pos-x (world-pos1 w)))
                                                                (pos-y (world-pos1 w)))])
                           w)]
                      [else w])]
               [(symbol=? 'Rook (piece-type (get-piece (world-pieces w) (world-moving w))))
                (cond [(key=? key "w")
                       (if (and (= 0 (get-x-distance w)) (< 0 (pos-y (world-pos1 w))))
                           (struct-copy world w [pos1 (make-pos (pos-x (world-pos1 w))
                                                                (sub1 (pos-y (world-pos1 w))))])
                           w)]
                      [(key=? key "a")
                       (if (and (= 0 (get-y-distance w)) (< 0 (pos-x (world-pos1 w))))
                           (struct-copy world w [pos1 (make-pos (sub1 (pos-x (world-pos1 w)))
                                                                (pos-y (world-pos1 w)))])
                           w)]
                      [(key=? key "s")
                       (if (and (= 0 (get-x-distance w)) (> 7 (pos-y (world-pos1 w))))
                           (struct-copy world w [pos1 (make-pos (pos-x (world-pos1 w))
                                                                (add1 (pos-y (world-pos1 w))))])
                           w)]
                      [(key=? key "d")
                       (if (and (= 0 (get-y-distance w)) (> 7 (pos-x (world-pos1 w))))
                           (struct-copy world w [pos1 (make-pos (add1 (pos-x (world-pos1 w)))
                                                                (pos-y (world-pos1 w)))])
                           w)]
                      [else w])]
               [else w])
         ; Free movement
         (cond 
           [(and (key=? key "w") (< 0 (pos-y (world-pos1 w))))
            (struct-copy world w [pos1 (make-pos (pos-x (world-pos1 w))
                                                 (sub1 (pos-y (world-pos1 w))))])]

           [(and (key=? key "s") (> 7 (pos-y (world-pos1 w))))
            (struct-copy world w [pos1 (make-pos (pos-x (world-pos1 w))
                                                 (add1 (pos-y (world-pos1 w))))])]

           [(and (key=? key "a") (< 0 (pos-x (world-pos1 w))))
            (struct-copy world w [pos1 (make-pos (sub1 (pos-x (world-pos1 w)))
                                                 (pos-y (world-pos1 w)))])]

           [(and (key=? key "d") (> 7 (pos-x (world-pos1 w))))
            (struct-copy world w [pos1 (make-pos (add1 (pos-x (world-pos1 w)))
                                                 (pos-y (world-pos1 w)))])]
           ; Pick up a piece
           [(key=? key " ")
            (if (piece? (get-piece (world-pieces w) (world-pos1 w)))
                (if (symbol=? 'white (piece-color (get-piece (world-pieces w)
                                                             (world-pos1 w))))
                    ; If color is right, remember the piece the player picked up
                    (struct-copy world w
                                 [pick1 #true]
                                 [moving (world-pos1 w)])
                    w)
                w)]
            
           [else w]))]

    ; Player 2 movement
    [(= 2 (world-turn w))
     (struct-copy world w [turn 1])]
   
    ; Nothing
    [else w]))
  

; main: Nothing -> Nothing
; Launches big-bang.
(define (main _)
  (big-bang INITIAL-WORLD-MENU
    [name "Chess-expect!"]
    [to-draw draw-world]
    [on-key handle-key]))


; Launch big-bang
(main 0)