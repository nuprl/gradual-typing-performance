#lang typed/racket

(define-type (NEListof A) (Pairof A (Listof A)))
(define-type Dir (U "up" "down" "left" "right"))
(define-type Snake snake)
(define-type World world)
(define-type Posn  posn)

(provide
 (struct-out posn)
 (struct-out snake)
 (struct-out world)
 posn=?
 Dir
 Snake
 World
 Posn
 NEListof)

(: posn=? (-> posn posn Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))  

(define-struct snake ([dir  : Dir]
                [segs : (NEListof Posn)]) #:prefab)
(define-struct world ([snake : Snake]
                [food  : Posn]) #:prefab)

(define-struct posn ([x : Real]
               [y : Real]) #:prefab)
