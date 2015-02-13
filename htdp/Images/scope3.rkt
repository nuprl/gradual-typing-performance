;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname scope3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define (f x) x)

... (f ...) ... 

(local ((define (e z) (f (* z z)))
        (define (f x) (+ (* x x) 25))
        (define (g y) (+ (f (+ y 1)) (f (- y 1)))))
  (e (g (f ...))))


(f ...)

