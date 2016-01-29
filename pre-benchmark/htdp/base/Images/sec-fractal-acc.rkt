#lang racket

(provide
 ;; Boolean Boolean -> Image 
 ;; the first Boolean says "show me the midpoint" finding, 
 ;; the second one says "show me the recursive problem" generation
 bezier-tri

 ;; Boolean -> Image 
 ;; the Boolean determines whether you see the 'perspective'
 bezier-curve 
 ;; -> Image 
 savannah-step 
 savannah)

;; -----------------------------------------------------------------------------
(require 2htdp/image lang/posn)

(module+ test
  (require test-engine/racket-tests))

(define LEFT-LINE   0.66)
(define RIGHT-LINE  0.77)
(define LEFT        -.20)
(define RIGHT       +.20)
(define SMALLEST   10.0)

;; Image Number Number Number Number -> Image 
;; add a Savannah tree to scene
(define (add-savannah scene x0 y0 l a)
  (cond
    [(too-short? l) scene]
    [else 
     (local ((define x1 (+ x0 (* (cos a) l)))
             (define y1 (- y0 (* (sin a) l)))
             (define scene+ (scene+line scene x0 y0 x1 y1 'red))
             (define x-1/3 (+ x0 (* (cos a) l 1/3)))
             (define y-1/3 (- y0 (* (sin a) l 1/3)))
             (define scene1 (add-savannah scene+ x-1/3 y-1/3 (* LEFT-LINE l) (- a LEFT)))
             (define x-2/3 (+ x0 (* (cos a) l 2/3)))
             (define y-2/3 (- y0 (* (sin a) l 2/3)))
             (define scene2 (add-savannah scene1 x-2/3 y-2/3 (* RIGHT-LINE l) (- a RIGHT))))
       scene2)]))

(define (too-short? l)
  (< l SMALLEST))

(define (savannah)
  (set! SMALLEST 10)
  (scale .66 (add-savannah (empty-scene 200 200) 100 200 100 (/ pi 2))))

(define (savannah-step)
  (set! SMALLEST 60)
  (scale .66 (add-savannah (empty-scene 200 200) 100 200 100 (/ pi 2))))

;; (savannah-step)

;; -----------------------------------------------------------------------------

(define SMALL-ENOUGH 1.5) ;; do not go below 1.5

;; Image Posn Posn Posn -> Image 
;; draw a smooth curve from a to c from b's perspective 
(define (add-bezier scene0 a b c)
  ; (displayln `(,(distance a b) ,(distance b c)))
  (cond
    [(small-enough? a b c) (add-triangle scene0 a b c)]
    [else 
     (local ((define mid-a-b (mid-point a b))
             (define mid-c-b (mid-point c b))
             (define mid-mid (mid-point mid-a-b mid-c-b))
             (define scene1 (add-bezier scene0 a mid-a-b mid-mid))
             (define scene2 (add-bezier scene1 mid-mid mid-c-b c)))
       scene2)]))

;; Image Posn Posn Posn -> Image 
;; add the black triangle (a,b,c) to scene

(module+ test
  (check-expect 
   (add-triangle (empty-scene 9 9) (make-posn 0 0) (make-posn 5 5) (make-posn 5 0))
   (add-line 
    (add-line 
     (add-line (empty-scene 9 9)
               5 5
               0 0
               'red)
     5 5
     5 0
     'red)
    0 0
    5 0
    'red)))

(define (add-triangle scene a b c)
  (local ((define a.x (posn-x a))
          (define a.y (posn-y a))
          (define b.x (posn-x b))
          (define b.y (posn-y b))
          (define c.x (posn-x c)) 
          (define c.y (posn-y c))
          (define sc/ab (add-line scene a.x a.y b.x b.y 'red))
          (define sc/bc (add-line sc/ab b.x b.y c.x c.y 'red))
          (define sc/ca (add-line sc/bc c.x c.y a.x a.y 'red)))
    sc/ca))


;; Posn Posn Posn -> Boolean 
;; is the triangle to small tp be divided any further 

(module+ test
  (check-expect (small-enough? (make-posn 0 0) (make-posn 1 1) (make-posn 1.2 1.2)) true)
  (check-expect (small-enough? (make-posn 0 0) (make-posn 100 100) (make-posn 2 2)) false))

(define (small-enough? a b c)
  (or (<= (distance a b) SMALL-ENOUGH)
      (<= (distance c b) SMALL-ENOUGH)))

;; Posn Posn -> Number 
;; the Cartesian distance between the two points 

(module+ test
  (check-within (distance (make-posn 0 0) (make-posn 3 4)) 5 .1))

(define (distance A B)
  (sqrt (+ (sqr (- (posn-x A) (posn-x B))) (sqr (- (posn-y A) (posn-y B))))))

;; Posn Posn -> Posn 
;; determine the midpoint between two points 

(module+ test
  (check-expect (mid-point (make-posn 10 10) (make-posn 0 0)) (make-posn 5 5)))

(define (mid-point A B)
  (make-posn (/ (+ (posn-x A) (posn-x B)) 2)
             (/ (+ (posn-y A) (posn-y B)) 2)))

(module+ test (test))

;; ---------------------------------------------------------------------------------------------------
(define MT (empty-scene 200 200))
(define A (make-posn  30  10))
(define B (make-posn  60 190))
(define C (make-posn 150 102))

(define (add-point scene A S color)
  (define A.x (posn-x A))
  (define A.y (posn-y A))
  (define T (text S 16 color))
  (define width (image-width T))
  (define delta (+ (quotient width 2) 10))
  (define scene1 (place-image/align (circle 3 'solid color) A.x A.y  'center 'center scene))
  (place-image T (+ A.x delta) A.y scene1))

(define (bezier-curve with-perspective)
  (scale 
   .66
   (let ()
     (define curve (add-bezier MT A B C))
     (define +A (add-point curve A "A" 'red))
     (define +C (add-point +A C "C" 'red))
     (cond
       [(not with-perspective) +C]
       [else 
        (define +B (add-point +C B "B" 'blue))
        (define +lineA (scene+line +B (posn-x B) (posn-y B) (posn-x A) (posn-y A) 'blue))
        (scene+line +lineA (posn-x B) (posn-y B) (posn-x C) (posn-y C) 'blue)]))))

;; (bezier-curve #f)
;; (bezier-curve #t)

(define (bezier-tri with-midpoints with-new-perspective)
  (scale
   .66
   (let ()
     (define curve MT)
     (define +A (add-point curve A "A" 'red))
     (define +C (add-point +A C "C" 'red))
     (define +B (add-point +C B "B" 'blue))
     (define +lineA (scene+line +B (posn-x B) (posn-y B) (posn-x A) (posn-y A) 'blue))
     (define +lineC (scene+line +lineA (posn-x B) (posn-y B) (posn-x C) (posn-y C) 'blue))
     (define IT 
       (if with-new-perspective
           +lineC
           (scene+line +lineC (posn-x A) (posn-y A) (posn-x C) (posn-y C) 'red)))
     (cond
       [(not with-midpoints) IT]
       [else
        (define AB (mid-point A B))
        (define +AB (add-point IT AB "A-B" 'blue))
        (define CB (mid-point B C))
        (define +CB (add-point +AB CB "B-C" 'blue))
        (define A-B-C (mid-point AB CB))
        (define ABC (add-point +CB A-B-C "A-B-C" 'red))
        (define IT2 (scene+line ABC (posn-x AB) (posn-y AB) (posn-x CB) (posn-y CB) 'blue))
        (cond
          [(not with-new-perspective) IT2]
          [else 
           (define +A-A-B-C (scene+line IT2 (posn-x A) (posn-y A) (posn-x A-B-C) (posn-y A-B-C) 'red))
           (scene+line +A-A-B-C (posn-x C) (posn-y C) (posn-x A-B-C) (posn-y A-B-C) 'red)])]))))


;; (bezier-tri #f #f)
;; (bezier-tri #t #f)
;; (bezier-tri #t #t)
