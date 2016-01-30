;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-double-amount) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#| A SILLY WORLD BASED SOLUTION
-------------------------------

(require 2htdp/universe)
(require 2htdp/image)

(define-struct world (initial current interest-rate months))

(define (show world)
  (place-image (text (convert world) 12 "blue") 250 50 (empty-scene 500 100)))

(define (convert world)
  (string-append
   "$" (number->string (world-initial world)) " "
   "$" (number->string (exact->inexact (world-current world))) " "
   (number->string (world-interest-rate world)) "% "
   (number->string (world-months world)) " months"))

(define (one-step world)
  (make-world (world-initial world)
              (* (world-current world) (world-interest-rate world))
              (world-interest-rate world)
              (add1 (world-months world))))

(define (balance-doubled? world)
  (>= (world-current world) (* 2 (world-initial world))))

(check-expect (calculate 200 (/ 0.06 12)) 139)
(define (calculate initial interest-rate)
  (world-months
   (big-bang (make-world initial initial (+ 1.0 interest-rate) 0)
             (on-tick one-step)
             (to-draw show)
             (stop-when balance-doubled?))))
|#

;; due to Adrian German <dgerman@soic.indiana.edu>

;; Number Number -> Nat
;; the number of months to double the given amount of money at the specified interest rate
;; GENERATIVE add the interest paid for the next period 

(check-expect (calculate 200 (/ 0.06 12)) 139)

(define (calculate initial interest-rate)
  (local ((define interest+principal (+ 1.0 interest-rate))
          ;; Number -> Nat
          ;; the number of months to double the given amount of money at the specified interest rate
          ;; GENERATIVE add the interest paid for the next period 
          (define (calculate current)
            (cond
              [(>= current (* 2 initial)) 0]
              [else (add1 (calculate (* current interest+principal)))])))
    (calculate initial)))

(define (calculate.v3 initial interest-rate)
  (local ((define interest+principal (+ 1.0 interest-rate))
          ;; Number Nat -> Nat
          ;; the number of months to double the given amount of money at the specified interest rate
          ;; GENERATIVE add the interest paid for the next period 
          ;; ACCUMULATOR months counts the number of steps taken 
          (define (calculate current months)
            (cond
              [(>= current (* 2 initial)) months]
              [else (calculate (* current interest+principal) (add1 months))])))
    (calculate initial 0)))
