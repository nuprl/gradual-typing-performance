;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-sierpinski) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

;; Posn Posn Posn -> Image
;; add a Sierpinski triangle at a, b, and c into MT
(define (sierpinski a0 b0 c0)
  (local (;; Posn Posn Posn Image -> Image
          ;; accumulator: s is MT with triangles between (a0,b0,c0) and  (a,b,c)
          ;; generative: 
          (define (sierpinski a b c s)
            (cond
              [(small? a b c) s]
              [else 
               (local ((define a-b (mid-point a b))
                       (define b-c (mid-point b c))
                       (define c-a (mid-point a c))
                       (define s0 (add-triangle a b c s))
                       (define s1 (sierpinski a a-b c-a s0))
                       (define s2 (sierpinski b a-b b-c s1))
                       (define s3 (sierpinski c c-a b-c s2)))
                 s3)])))
    (sierpinski a0 b0 c0 MT)))

;; Posn Posn -> Posn
;; compute the mid-point between a and b
(define (mid-point a b)
  (make-posn
    (mid (posn-x a) (posn-x b))
    (mid (posn-y a) (posn-y b))))

;; Number Number -> Number
;; compute the average of x and y
(define (mid x y)
  (quotient (+ x y) 2))

;; Posn Posn Posn Image -> Image
;; add triangle (a,b,c) to scene s0
(define (add-triangle a b c s0)
  (local ((define a.x (posn-x a))
          (define b.x (posn-x b))
          (define c.x (posn-x c))
          (define a.y (posn-y a))
          (define b.y (posn-y b))
          (define c.y (posn-y c))
          (define s1 (add-line s0 a.x a.y b.x b.y 'blue))
          (define s2 (add-line s1 b.x b.y c.x c.y 'blue))
          (define s3 (add-line s2 c.x c.y a.x a.y 'blue)))
    s3))

;; Posn Posn Posn -> Boolean 
;; is this Sierpinski triangle small enough to draw?
(define (small? a b c)
  (<= (distance a b) 4))

;; Posn Posn -> Number 
;; compute distance between two points 
(define (distance a b)
  (sqrt (+ (sqr (- (posn-x a) (posn-x b))) (sqr (- (posn-y a) (posn-y b))))))

;; -----------------------------------------------------------------------------
(define a (make-posn 200  50))
(define b (make-posn 27  350))
(define c (make-posn 373 350))

(check-within (distance a b) (distance b c) .5)
(check-within (distance b c) (distance c a) .5)
(check-within (distance c a) (distance a b) .5)

(define MT (empty-scene 400 400))
(sierpinski a b c)