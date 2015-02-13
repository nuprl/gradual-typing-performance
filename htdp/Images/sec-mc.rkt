#lang racket

(provide 
 ;; -> Image
 mc-state
 mc-state0 
 mc-state*
 mc-transition)

;; -----------------------------------------------------------------------------
(require "ysupport.rkt" slideshow pict/flash)

(define radius 16)
(define M (colorize (filled-ellipse radius radius) "black"))
(define C (ellipse radius radius))
(define side (rectangle (+ (* 2 radius) 12) (+ (* 3 radius) 18)))
(define (river boat-location)
  [define river (rectangle (+ (* 4 radius) 12) (+ (* 3 radius) 18))]
  [define wave (t " ~~ ")]
  (define y (- (/ (pict-height side) 2) 18))
  (cond
    [(eq? boat-location 'right)
     (define wboat (hc-append wave boat))
     (build-scene
      r river
      (pin-over r (- (pict-width river) (pict-width wboat) 3) y wboat)
      (pin-over r  9 -3 wave)
      (pin-over r 11 33 wave))]
    [else 
     (define wboat (hc-append boat wave))
     (build-scene
      r river
      (pin-over r 0 y wboat)
      (pin-over r  9 -3 wave)
      (pin-over r 11 33 wave))]))

(define boat 
  (vc-append 
   (filled-flash radius radius 2)
   (hc-append (blank 3) (filled-rectangle radius (/ radius 2)) (blank 3))))

;; MC = [List N N]
;; (list m c) denotes pairing of m missionaries and c cannibals  
(define mc-m first)
(define mc-c second)

;; MC (Union 'left 'right) MC -> Pict 
(define/contract (mc l b r)
  (->i ((l (listof natural-number/c)) (b symbol?) (r (listof natural-number/c)))
       #:pre (l r) (equal? '(3 3) (map + l r))
       (result pict?))
  (define l-pict (one-place l))
  (define the-river (build-scene r (river b)))
  (define r-pict (one-place r))
  (scale (ht-append l-pict the-river r-pict) .66))

;; N -> Pict 
(define (one-place l)
  (define f (ht-append (stack-em (mc-m l) M) (blank 3) (stack-em (mc-c l) C)))
  (cc-superimpose side f))

;; N Pict -> Pict 
(define (stack-em n P)
  (splice vc-append (build-list n (lambda (_) P)) (blank 3)))

;; [Pict ... -> Pict] [List-of Pict] Pict -> Image 
(define (splice vc-append l x) 
  (if (empty? l) 
      (blank radius)
      (for/fold ((img (first l))) [(y (rest l))]
        (vc-append img x y))))

;; -----------------------------------------------------------------------------

(define (bad x)
  (define str (scale (t "★") .5))
  (define w (pict-width x))
  (define h (pict-height x))
  (pin-under x (- w (pict-width str)) -5 #;(pict-height str) str))

(define (mc-state)
  (mc '(1 1) 'left '(2 2)))

(define mc0
  (mc '(3 3) 'left '(0 0)))

(define (mc-state0)
  mc0)

(define (mc-state*)
  (mc '(0 0) 'right '(3 3)))

(define configuration2
  (list 
    (mc '(3 2) 'right '(0 1))
    (mc '(3 1) 'right '(0 2))
    (mc '(2 2) 'right '(1 1))
    (bad (mc '(1 3) 'right '(2 0)))
    (bad (mc '(2 3) 'right '(1 0)))))

(define configuration3
  (list 
    (bad (mc '(2 3) 'left '(1 0)))
    (mc '(3 3) 'left '(0 0))
    (mc '(3 2) 'left '(0 1))))

(define mc1 (splice vc-append configuration2 (blank 20)))
(define mc2 (splice vc-append configuration3 (blank 20)))

(define (mc-transition)
  (define x (third configuration2))
  (define y (second configuration3))
  (build-scene 
   s (hc-append (blank 50) mc0 (blank 50) mc1 (blank 50) mc2 (blank 50))
   (vc-append (blank 40) s (blank 10))
   (pin-arrow-line 10 s mc0 rc-find (first configuration2) lc-find)
   (pin-arrow-line 10 s mc0 rc-find (second configuration2) lc-find)
   (pin-arrow-line 10 s mc0 rc-find x lc-find)
   (pin-arrow-line 10 s mc0 rc-find (fourth configuration2) lc-find)
   (pin-arrow-line 10 s mc0 rc-find (fifth configuration2) lc-find)
   (pin-arrow-line 10 s x rc-find (first configuration3) lc-find)
   (pin-arrow-line 10 s x rc-find y lc-find)
   (pin-arrow-line 10 s x rc-find (third configuration3) lc-find)
   (pin-arrow-line 10 s y rc-find mc0 lc-find
                   #:start-angle (/ pi 3)
                   #:start-pull .65
                   #:end-angle (/ pi -3)
                   #:end-pull .65
                   #:style 'short-dash)))

; (mc-transition)