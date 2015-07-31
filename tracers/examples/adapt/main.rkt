#lang typed/racket/base

;; WORKS GREAT if you remember to require/typed

(require/typed/provide "data.rkt"
  [#:struct foo ([x : (-> Natural Natural)] [y : String] [z : (Listof Natural)])]
  [val Index]
  [foo1 foo])

(define (main)
  ((foo-x foo1) 1)
  (eq? '() (foo-z foo1))
  (+ val val)
  (foo (lambda (x) (+ x 1)) "yes" '())
  (void))

(main)
