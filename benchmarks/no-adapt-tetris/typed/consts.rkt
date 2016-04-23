#lang typed/racket
(require benchmark-util)

(: block-size Integer)
(define block-size 20)

(: board-height Integer)
(define board-height 20)

(: board-width Integer)
(define board-width 10)

(safe-and-unsafe-provide block-size
         board-width
         board-height)


