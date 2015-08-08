#lang racket

(define block-size 20)
(define board-height 20)
(define board-width 10)
(provide/contract [block-size integer?]
                  [board-width integer?]
                  [board-height integer?])


