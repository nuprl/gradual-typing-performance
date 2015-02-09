#lang racket

(provide
 ;; -> Image
 relative-scene
 absolute-scene)
 
;; ---------------------------------------------------------------------- 


(require 2htdp/image "xsupport.rkt")

(define dot (circle 4 'solid 'black))

;; Nat Image -> Image 
;; add dot at x to s0, labeled as "x" 
(define (absolute-dot x s0)
  (define t (text (number->string x) 12 'black))
  (build-scene 
   s (place-image dot x 20 s0)
   (place-image t x 40 s)))

(define (absolute-scene)
  (build-scene 
   s (rectangle 300 50 'solid 'white)
   (place-image/align (line 300 0 'black) 0 20 'left 'top s)
   (absolute-dot 50 s)
   (absolute-dot 90 s)
   (absolute-dot 160 s)
   (absolute-dot 190 s)
   (absolute-dot 220 s)))

(define (relative-scene)
  (define x 0)
  ;; Nat Image -> Image 
  ;; add dot at (+ x x0) to s0, labeled with "x0"
  ;; effect: bump x by x0
  (define (relative-dot x0 s0)
    (set! x (+ x x0))
    (define t (text (number->string x0) 12 'black))
    (build-scene 
     s (place-image dot x 20 s0)
     (place-image t x 40 s)))
  ;; -- IN --
  (build-scene 
   s (rectangle 300 50 'solid 'white)
   (place-image/align (line 300 0 'black) 0 20 'left 'top s)
   (relative-dot 50 s)
   (relative-dot 40 s)
   (relative-dot 70 s)
   (relative-dot 30 s)
   (relative-dot 30 s)))

; (relative-scene)
; (absolute-scene)
