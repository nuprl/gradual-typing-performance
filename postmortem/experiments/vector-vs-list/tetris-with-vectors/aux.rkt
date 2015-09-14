#lang racket

(require "data.rkt"
         "tetras.rkt")  

(provide
 vector-pick-random
 neg-1
 tetras)
#;
(provide/contract
 [vector-pick-random ((vectorof TETRA/C) . -> . TETRA/C)]
 [neg-1 integer?] ;; ha!
 [tetras (vectorof TETRA/C)])

(define r (make-pseudo-random-generator))
(parameterize ((current-pseudo-random-generator r))
  (random-seed 43453))


(define (vector-pick-random ls)
  (vector-ref ls (random (vector-length ls) r)))

(define neg-1 -1)

(define tetras
  (vector 
   (build-tetra-blocks 'green   1/2 -3/2    0 -1 0 -2 1 -1 1 -2)
   (build-tetra-blocks 'blue    1   -1      0 -1 1 -1 2 -1 3 -1)
   (build-tetra-blocks 'purple  1   -1      0 -1 1 -1 2 -1 2 -2)
   (build-tetra-blocks 'cyan    1   -1      0 -1 1 -1 2 -1 0 -2)
   (build-tetra-blocks 'orange  1   -1      0 -1 1 -1 2 -1 1 -2)
   (build-tetra-blocks 'red     1   -1      0 -1 1 -1 1 -2 2 -2)
   (build-tetra-blocks 'pink    1   -1      0 -2 1 -2 1 -1 2 -1)))

