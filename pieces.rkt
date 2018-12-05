;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pieces) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; PF1 final project: chess-expect!
; Pieces definition file
; Author: Stefano Taillefert

(require racket/base)
(require 2htdp/image)
(require "config.rkt")

(provide W-KING W-QUEEN W-ROOK W-BISHOP W-KNIGHT W-PAWN
         B-KING B-QUEEN B-ROOK B-BISHOP B-KNIGHT B-PAWN)

; White pieces

(define W-KING-IMG (bitmap "img/whiteKing.png"))
(define W-KING (scale (/ (/ WIDTH 8) (image-width W-KING-IMG)) W-KING-IMG))

(define W-QUEEN-IMG (bitmap "img/whiteQueen.png"))
(define W-QUEEN (scale (/ (/ WIDTH 8) (image-width W-QUEEN-IMG)) W-QUEEN-IMG))

(define W-ROOK-IMG (bitmap "img/whiteRook.png"))
(define W-ROOK (scale (/ (/ WIDTH 8) (image-width W-ROOK-IMG)) W-ROOK-IMG))

(define W-BISHOP-IMG (bitmap "img/whiteBishop.png"))
(define W-BISHOP (scale (/ (/ WIDTH 8) (image-width W-BISHOP-IMG)) W-BISHOP-IMG))

(define W-KNIGHT-IMG (bitmap "img/whiteKnight.png"))
(define W-KNIGHT (scale (/ (/ WIDTH 8) (image-width W-KNIGHT-IMG)) W-KNIGHT-IMG))

(define W-PAWN-IMG (bitmap "img/whitePawn.png"))
(define W-PAWN (scale (/ (/ WIDTH 8) (image-width W-PAWN-IMG)) W-PAWN-IMG))

; Black pieces

(define B-KING-IMG (bitmap "img/blackKing.png"))
(define B-KING (scale (/ (/ WIDTH 8) (image-width B-KING-IMG)) B-KING-IMG))

(define B-QUEEN-IMG (bitmap "img/blackQueen.png"))
(define B-QUEEN (scale (/ (/ WIDTH 8) (image-width B-QUEEN-IMG)) B-QUEEN-IMG))

(define B-ROOK-IMG (bitmap "img/blackRook.png"))
(define B-ROOK (scale (/ (/ WIDTH 8) (image-width B-ROOK-IMG)) B-ROOK-IMG))

(define B-BISHOP-IMG (bitmap "img/blackBishop.png"))
(define B-BISHOP (scale (/ (/ WIDTH 8) (image-width B-BISHOP-IMG)) B-BISHOP-IMG))

(define B-KNIGHT-IMG (bitmap "img/blackKnight.png"))
(define B-KNIGHT (scale (/ (/ WIDTH 8) (image-width B-KNIGHT-IMG)) B-KNIGHT-IMG))

(define B-PAWN-IMG (bitmap "img/blackPawn.png"))
(define B-PAWN (scale (/ (/ WIDTH 8) (image-width B-PAWN-IMG)) B-PAWN-IMG))