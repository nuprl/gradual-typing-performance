#lang typed/racket/base

(provide tetras)

(require benchmark-util
         "data-block-adapted.rkt"
         "data-tetra-adapted.rkt")
(require/typed/check "tetras-build-tetra-blocks.rkt"
  [build-tetra-blocks (-> Color Real Real Real Real Real Real Real Real Real Real Tetra)]
  )

;; =============================================================================

(define tetras
  (list
   (build-tetra-blocks 'green   1/2 -3/2    0 -1 0 -2 1 -1 1 -2)
   (build-tetra-blocks 'blue    1   -1      0 -1 1 -1 2 -1 3 -1)
   (build-tetra-blocks 'purple  1   -1      0 -1 1 -1 2 -1 2 -2)
   (build-tetra-blocks 'cyan    1   -1      0 -1 1 -1 2 -1 0 -2)
   (build-tetra-blocks 'orange  1   -1      0 -1 1 -1 2 -1 1 -2)
   (build-tetra-blocks 'red     1   -1      0 -1 1 -1 1 -2 2 -2)
   (build-tetra-blocks 'pink    1   -1      0 -2 1 -2 1 -1 2 -1)))
