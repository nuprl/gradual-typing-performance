#lang racket

(provide
 ;; Image 
 fsm-a-b-or-c*-d)

(require slideshow "ysupport.rkt")

; ExpectsToSee is one of: 
; – AA
; – BC
; – DD 
; – ER 

(def/dots AA BC DD ER)

;(define AA "start, expect to see an 'a' next")
;(define BC "expect to see: 'b', 'c', or 'd'")
;(define DD "encountered a 'd', finished")
;(define ER "error, user pressed illegal key")

(define width  600)
(define height 450)
(define Yhigh  110)
(define Ylow   (- height (pict-height AA) 10))
(define X0      10)
(define Xdelta 260)
(define arr-wgt 10)

(define fsm-a-b-or-c*-d
  (build-scene 
   scene (rectangle width height)
   (pin-over scene X0                  50    AA)
   (pin-over scene (+ X0 (* 1 Xdelta)) Yhigh BC)
   (pin-over scene (+ X0 (* 2 Xdelta)) 50    DD)
   (pin-over scene (+ X0 (* 1 Xdelta)) Ylow  ER)
   (pin-arrow-line arr-wgt scene AA rc-find BC lc-find)
   (pin-over scene (+ X0 (* .5 Xdelta)) (/ (+ 50 Yhigh) 2) (t "\"a\""))
   (pin-arrow-line arr-wgt scene BC rc-find DD lc-find)
   (pin-over scene (+ X0 (* 1.5 Xdelta)) (/ (+ 50 Yhigh) 2) (t "\"d\""))
   (pin-arrow-line arr-wgt scene BC rc-find BC ct-find
                   #:start-angle (/ pi 6)
                   #:start-pull 2
                   #:end-angle (* 6/4 pi)
                   #:end-pull 2)
   (pin-over scene (+ X0 (* 1.15 Xdelta)) (/ (+ 50 Yhigh) 3) (t "\"b\""))
   (pin-arrow-line arr-wgt scene BC lc-find BC cb-find
                   #:start-angle pi
                   #:start-pull 2
                   #:end-angle (* -7/4 pi)
                   #:end-pull 2)
   (pin-over scene (+ X0 (* 0.85 Xdelta)) (* 1.0 (+ 50 Yhigh)) (t "\"c\""))
   (pin-arrow-line arr-wgt scene BC cb-find ER ct-find)
   (pin-over scene (+ X0 (* 0.3 Xdelta)) (* 1.3 (+ 50 Yhigh)) (t "not \"a\""))
   (pin-arrow-line arr-wgt scene AA cb-find ER ct-find)
   (pin-over scene (+ X0 (* 0.9 Xdelta)) (* 1.8 (+ 50 Yhigh)) (t "not \"b\", \"c\", or \"d\""))))
