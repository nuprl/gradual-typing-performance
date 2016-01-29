;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname scope2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define (h x)
  (f (* 2 x)))

(define (f x)
  (+ (* x x) 25))

(define (g x)
  (+ (f (+ x 1)) (f (- x 1))))


