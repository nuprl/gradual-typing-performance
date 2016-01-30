#lang racket

(define-struct posn (x y) #:prefab)
(define-struct block (x y color) #:prefab)
(define-struct tetra (center blocks) #:prefab)
(define-struct world (tetra blocks) #:prefab)

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(provide
 (struct-out block)
 (struct-out posn)
 (struct-out tetra)
 (struct-out world)
 posn=?)
#;
(provide
 (contract-out
  (struct block ([x real?] [y real?] [color COLOR/C]))
  (struct posn ([x real?] [y real?]))
  (struct tetra ([center POSN/C] [blocks BSET/C]))
  (struct world ([tetra TETRA/C] [blocks BSET/C]))       
  [posn=? (POSN/C POSN/C . -> . boolean?)])
 COLOR/C
 POSN/C
 BLOCK/C
 TETRA/C
 WORLD/C
 BSET/C)
