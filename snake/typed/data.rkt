#lang typed/racket

(struct: snake ([dir  : Dir]
                [segs : (NEListof Posn)]))
(struct: world ([snake : Snake]
                [food  : Posn]))

(struct: posn ([x : Real]
               [y : Real]))

(define-type Dir (U "up" "down" "left" "right"))
(define-type Snake snake)
(define-type World world)
(define-type Posn  posn)
(define-type (NEListof A) (Pairof A (Listof A)))

(: posn=? (-> Posn Posn Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))  

(provide
 posn=?
 [struct-out posn]
 [struct-out snake]
 [struct-out world]
 Dir
 Snake
 World
 Posn
 NEListof
 )
