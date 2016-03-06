#lang racket

;; a representation of the visible stacks

(require "card.rkt" "../base/utility.rkt")

(types Card)

(provide
 (type Stack [Listof Card]) ;; between 1 and FIVE cards
 
 ;; Card -> Stack
 create-stack
 
 ;; Stack -> Card
 top
 
 ;; Card Stack -> Stack
 push 
 
 ;; Stack -> N
 length
 
 ;; Stack -> N
 ;; sum up the bulls shown on the cards of the stack
 bulls)

;; ---------------------------------------------------------------------------------------------------
(require (prefix-in list: (only-in racket length)))
(module+ test (require rackunit))

(define (create-stack c) (list c))
(define top first)
(define push cons)
(define length list:length)
(define (bulls s) (foldr + 0 (map card-bulls s)))

(module+ test
  (require "card.rkt")
  (define stack
    (push (card 5 2) (push (card 4 2) (push (card 3 2) (push (card 2 2) (create-stack (card 1 2)))))))
  (check-equal? (bulls stack) 10))
