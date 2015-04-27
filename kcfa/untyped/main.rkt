#lang racket/base

;; Create a few examples and run abstract interpretation

(require
  "structs.rkt"
  "ui.rkt"
)

;; =============================================================================

(define new-label gensym)

;(: make-ref (-> Var Exp))
(define (make-ref var)
  (Ref (new-label) var))

;(: make-lambda (-> (Listof Var) Exp Exp))
(define (make-lambda formals call)
  (Lam (new-label) formals call))

;(: make-call (-> Exp Exp * Exp))
(define (make-call fun . args)
  (Call (new-label) fun args))

;(: make-let (-> Var Exp Exp Exp))
(define (make-let var exp call)
  (make-call (make-lambda (list var) call) exp))

;; -- example

;; (let* ((id (lambda (x) x))
;;        (a  (id (lambda (z) (halt z))))
;;        (b  (id (lambda (y) (halt y)))))
;;   (halt b))
;(: standard-example Exp)
(define standard-example
  (make-let
    'id
    (make-lambda '(x k) (make-call (make-ref 'k) (make-ref 'x)))
    (make-call (make-ref 'id)
                (make-lambda '(z) (make-ref 'z))
                (make-lambda '(a) 
                              (make-call (make-ref 'id)
                                          (make-lambda '(y) (make-ref 'y))
                                          (make-lambda '(b)
                                                        (make-ref 'b)))))))

;; (let ((f1 (lambda (f k) (f k)))
;;       (f2 (lambda (f k) (f k)))
;;       (f3 (lambda (f k) (f k))))
;;   (f1 f2))
;(: loop3 Exp)
(define loop3
  (make-let
    'f1 (make-lambda '(f k) (make-call (make-ref 'f) (make-ref 'k)))
    (make-let
      'f2 (make-lambda '(f k) (make-call (make-ref 'f) (make-ref 'k)))
      (make-let
        'f3 (make-lambda '(f k) (make-call (make-ref 'f) (make-ref 'k)))
        (make-call (make-ref 'f1) (make-ref 'f2))))))

;; (let ((zero (lambda (f x k) (k x)))
;;       (succ (lambda (n k) (lambda (f x) (k (n f x)))))
;;       (add  (lambda (n1 n2 k) (k (n1 suc n2)))))
;;   (add (succ succ zero) (succ succ zero)))
;(: church Exp)
(define church
  (make-let
    'zero (make-lambda '(f x k) (make-call (make-ref 'k) (make-ref 'x)))
    (make-call (make-ref 'zero) (make-ref 'zero) (make-ref 'zero))))

;; -- main

(define (main)
  (for ([a-k (in-range 6)])
    (analyze standard-example)
    (analyze loop3)
    (analyze church)
  ))
    ;(printf "K = ~a\n~a\n"
    ;    a-k (format-mono-store (analyze standard-example)))))

(time (main))
