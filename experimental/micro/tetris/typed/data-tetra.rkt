#lang typed/racket/base

(provide (struct-out tetra))

(require benchmark-util
         "data-posn-adapted.rkt"
         "data-block-adapted.rkt")

(struct tetra ([center : posn]
        [blocks : (Listof Block)]))
