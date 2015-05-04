#lang typed/racket

(provide (struct-out tetra)
         Tetra)

(require "data-posn-adapted.rkt"
         "data-block-adapted.rkt"
         benchmark-util)

(require/typed/check "data-tetra.rkt"
  [#:struct tetra ([center : posn]
                   [blocks : (Listof Block)])])

(define-type Tetra tetra)
