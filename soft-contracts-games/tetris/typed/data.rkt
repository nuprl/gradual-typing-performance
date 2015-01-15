#lang typed/racket

(struct: posn ([x : Real]
               [y : Real]))
(struct: block ([x : Real]
                [y : Real]
                [color : Color]))
(struct: tetra ([center : posn]
                [blocks : BSet]))
(struct: world ([tetra : tetra]
                [blocks : BSet]))

(define-type Posn posn)
(define-type Block block)
(define-type Tetra tetra)
(define-type World world)
(define-type Color Symbol)
(define-type BSet  (Listof Block))

(: posn=? (-> posn posn Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(provide
 (struct-out posn)
 (struct-out block)
 (struct-out tetra)
 (struct-out world)
 Posn
 Block
 Tetra
 World
 Color
 BSet
 Color
 BSet
 posn=?)
