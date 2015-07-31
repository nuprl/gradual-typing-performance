#lang typed/racket/base

(require/typed/provide "data.rkt"
  [#:struct foo ([x : (-> Natural Natural)] [y : String] [z : (Listof Natural)])]
  [val Index]
  [foo1 foo])
