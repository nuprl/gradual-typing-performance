;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-slope-root-newton) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

(define EPSILON .1)

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; Number -> Number
(define (bad-for-newton1 x)
  (- (* x x) x 1.8))

;; [@tech{Number} -> @tech{Number}] @tech{Number} -> @tech{Number}
;; find a number @racket[r] such that @racket[(f r)] is small
;; @bold{generative} repeatedly generate improved guesses using @racket[f] and @racket[r]

(check-within (newton poly 1) 2 EPSILON)
(check-within (newton poly 3.5) 4 EPSILON)

(define (newton f r1)
  (cond
    [(<= (abs (f r1)) EPSILON) r1]
    [else (newton f (tee (root-of-tangent f r1)))]))

(define (tee x) x)

#;
(define (tee x)
  (begin 
    (display `(newton poly ,x))
    (newline)
    x))

;; [Number -> Number] Number -> Number 
;; determine the slope of f at x 

(check-within (slope (lambda (x) (+ (* 5 x) 10)) 0) 5 EPSILON)
(check-within (slope (lambda (x) 10) 0) 0 EPSILON)
(check-within (slope (lambda (x) (* x x)) 1) 2.0 EPSILON)

(define (slope f x)
  (/ (- (f (+ x EPSILON)) (f (- x EPSILON)))
     2 EPSILON))

;; [Number -> Number] Number -> Number 
;; determine the root of the tangent to f at x

(check-within (root-of-tangent (lambda (x) (+ x 5)) 0) -5 EPSILON)
(check-within (root-of-tangent (lambda (x) (+ (* 2 x) 10)) 0) -5 EPSILON)
(check-error (root-of-tangent (lambda (x) 10) 0) "/: division by zero")

(define (root-of-tangent f x)
  (- x (/ (f x) (slope f x))))

