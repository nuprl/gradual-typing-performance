#lang racket

(struct posn (x y))
(struct block (x y color))
(struct tetra (center blocks))
(struct world (tetra blocks))

(define COLOR/C symbol?)
(define POSN/C (struct/c posn real? real?))
(define BLOCK/C (struct/c block real? real? COLOR/C))
(define BSET/C (listof BLOCK/C))  
(define TETRA/C (struct/c tetra POSN/C BSET/C))
(define WORLD/C (struct/c world TETRA/C BSET/C))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

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
