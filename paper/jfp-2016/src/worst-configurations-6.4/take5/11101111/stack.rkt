#lang typed/racket/base

;; a representation of the visible stacks

(provide

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

(require "card-adapted.rkt" "stack-types.rkt")
(require (prefix-in list: (only-in racket/base length)))
(require (only-in racket/list first))

;; ---------------------------------------------------------------------------------------------------

(: create-stack (-> Card Stack))
(define (create-stack c) (list c))
(: top (-> Stack Card))
(define top first)
(: push (-> Card Stack Stack))
(define (push c s)
  (cast ((inst cons Card Card) c s) Stack))
(: length (-> Stack Natural))
(define length list:length)
(: bulls (-> Stack Natural))
(define (bulls s) (foldr + 0 (map card-bulls s)))

