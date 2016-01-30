#lang typed/racket/base

(provide (struct-out foo) val foo1 lam)

(require/typed "data.rkt"
  [#:struct foo ([x : (-> Natural Natural)] [y : String] [z : (Listof Natural)])]
  [val Index]
  [foo1 foo]
  [lam (-> Index Index)])
