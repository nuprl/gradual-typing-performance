#lang racket/base

;; Cannot usefully require/typed/unsafe a typed struct.
;; - Trying to import as #:opaque makes an incompatible type
;; - Trying to import as #:struct defines a new, incompatible struct type

(module a typed/racket/base
  (struct foo ([x : Natural] [y : Natural]))
  (provide (struct-out foo)))

(module b typed/racket/base
  (require typed/racket/unsafe)
  (require/typed/unsafe (submod ".." a)
    ;; NO, opaque makes a new type
    ;[#:opaque Foo foo?]
    ;[foo (-> Natural Natural Foo)]
    ;[foo-x (-> Foo Natural)]
    ;[foo-y (-> Foo Natural)]
    ;; NO, incompatible structs
    [#:struct foo ([x : Natural] [y : Natural])]
  )
  (define-type Foo foo)

  (: foo-succ (-> Foo Foo))
  (define (foo-succ f)
    (foo (add1 (foo-x f)) (foo-y f)))
)
(require 'b)
