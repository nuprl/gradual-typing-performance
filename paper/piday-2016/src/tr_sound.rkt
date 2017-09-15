#lang racket/base

(module u racket/base
  (define (unsafe x)
    (set-box! x "hi"))
  (provide unsafe))

(module t typed/racket/base
  (require/typed (submod ".." u)
    (unsafe (-> (Boxof Integer) Void)))
  (: typed (-> (Boxof Integer) Integer))
  (define (typed x)
    (unsafe x)
    (unbox x))
  (provide typed))

(require 't)
(typed (box 1))
