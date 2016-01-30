#lang typed/racket

(define-struct posn ([x : Real]
               [y : Real]) #:prefab)
(define-struct block ([x : Real]
                [y : Real]
                [color : Symbol]) #:prefab)
(define-struct tetra ([center : posn]
                [blocks : (Listof block)]) #:prefab)
(define-struct world ([tetra : tetra]
                [blocks : (Listof block)]) #:prefab)

(: posn=? (-> posn posn Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))
(define-type Color Symbol)

(require/typed 2htdp/image 
  [#:opaque Image image?])

(define-type Posn posn)
(define-type Block block)
(define-type Tetra tetra)
(define-type World world)
(define-type BSet  (Listof Block))

(provide
 (struct-out posn)
 (struct-out block)
 (struct-out tetra)
 (struct-out world)
 posn=?
 Posn
 Block
 Tetra
 World
 Color
 BSet
 Color
 BSet
 Image)
