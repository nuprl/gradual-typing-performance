#lang racket/base

(provide (struct-out tetra))

(require benchmark-util
         "data-posn.rkt"
         "data-block.rkt")

(struct tetra (center
        blocks))
