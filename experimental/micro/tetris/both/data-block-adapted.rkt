#lang typed/racket

(provide (struct-out block)
         Block
         BSet
         Color)

(define-type Color Symbol)
(require benchmark-util)
(require/typed/check "data-block.rkt"
  [#:struct block ([x : Real]
                   [y : Real]
                   [color : Color])])

(define-type Block block)
(define-type BSet (Listof Block))
