#lang typed/racket/base

;; TODO make stacks be objects ?

;; Forth stacks,
;;  data definition & operations

(provide
  ;; type Stack = List
  ;;
  ;; Notation:
  ;;  []    = empty stack
  ;;  x::xs = item 'x' on top of the stack 'xs'
  ;;
  ;; Operations will raise an exception with message "empty stack"
  ;;  if called on a stack with too few elements.

  stack-drop
  ;; (-> x::xs xs)
  ;; Drop the item on top of the stack

  stack-dup
  ;; (-> x::xs x::x::xs)
  ;; Duplicate the item on top of the stack

  stack-init
  ;; (-> Stack)
  ;; Initialize an empty stack

  stack-over
  ;; (-> x::y::xs x::y::x::xs)
  ;; Duplicate the item on top of the stack, but place the duplicate
  ;;  after the 2nd item on the stack.

  stack-pop
  ;; (-> x::xs (Values x xs))
  ;; Pop the top item from the stack, return both the item and the new stack.

  stack-push
  ;; (-> xs x x::xs)
  ;; Push an item on to the stack

  stack-swap
  ;; (-> x::y::xs y::x::xs)
  ;; Swap the positions of the first 2 items on the stack.
)

;; =============================================================================

(define-type Stack (Listof Integer))

(: list->stack (-> (Listof Integer) Stack))
(define (list->stack xs)
  (for/fold ([S : Stack (stack-init)])
            ([x : Integer (in-list (reverse xs))])
    (stack-push S x)))

(: stack-drop (-> Stack Stack))
(define (stack-drop S)
  (let-values ([(_v S+) (stack-pop S)])
    S+))

(: stack-dup (-> Stack Stack))
(define (stack-dup S)
  (let-values ([(v S+) (stack-pop S)])
    (stack-push (stack-push S+ v) v)))

(: stack-init (-> Stack))
(define (stack-init)
  '())

(: stack-over (-> Stack Stack))
(define (stack-over S)
  (let*-values ([(v1 S1) (stack-pop S)]
                [(v2 S2) (stack-pop S1)])
    (stack-push (stack-push (stack-push S2 v1) v2) v1)))

(: stack-pop (-> Stack (Values Integer Stack)))
(define (stack-pop S)
  (if (null? S)
      (raise-user-error "empty stack")
      (values (car S) (cdr S))))

(: stack-push (-> Stack Integer Stack))
(define (stack-push S v)
  (cons v S))

(: stack-swap (-> Stack Stack))
(define (stack-swap S)
  (let*-values ([(v1 S1) (stack-pop S)]
                [(v2 S2) (stack-pop S1)])
    (stack-push (stack-push S2 v1) v2)))

