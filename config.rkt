;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname config) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; PF1 final project: chess-expect!
; Config file
; Author: Stefano Taillefert

(require racket/base)

(provide WIDTH HEIGHT)

; Screen width
(define WIDTH 800)

; Screen height
(define HEIGHT 800)