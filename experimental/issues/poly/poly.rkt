#lang racket/base

;; Choice of contracts: does it matter?

(module u racket/base
  (define N 500)
  (define (map f xs)
    (if (null? xs)
      null
      (cons (f (car xs)) (map f (cdr xs)))))
  (provide map N))

(module t1 typed/racket/base
  (require/typed (submod ".." u)
    (N Natural)
    (map (All (A B) (-> (-> A B) (Listof A) (Listof B)))))
  (define (main)
    (for/fold : (Listof Natural)
              ([x* : (Listof Natural) (build-list N (lambda (_) 0))])
              ([i (in-range N)])
      (map add1 x*))
    (void))
  (provide main))

(module t2 typed/racket/base
  (require/typed (submod ".." u)
    (N Natural)
    (map (-> (-> Integer Integer) (Listof Integer) (Listof Integer))))
  (define (main)
    (for/fold : (Listof Integer)
              ([x* : (Listof Integer) (build-list N (lambda (_) 0))])
              ([i (in-range N)])
      (map add1 x*))
    (void))
  (provide main))

(require
  (prefix-in t1: 't1)
  (prefix-in t2: 't2))

(time (t1:main))
(time (t2:main))
