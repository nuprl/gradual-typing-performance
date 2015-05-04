#lang typed/racket

(provide (struct-out world)
         World)

(require benchmark-util
         "data-tetra-adapted.rkt"
         "data-block-adapted.rkt")
(require/typed/check "data-world.rkt"
  [#:struct world ([tetra : tetra]
                   [blocks : (Listof Block)])])

(define-type World world)
