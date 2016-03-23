#lang typed/racket/base

(require
 benchmark-util
 "modulegraph-adapted.rkt"
 )

(require/typed/check "summary.rkt"
  [#:struct summary (
    [source : Path-String]
    [dataset : (Vectorof (Listof Index))]
    [modulegraph : ModuleGraph])]
  [from-rktd (->* [String] [#:graph (U Path #f)] Summary)]
  [all-variations (-> Summary (Sequenceof String))]
  [get-num-variations (-> Summary Index)]
  [get-project-name (-> Summary String)]
  [predicate->variations (-> Summary (-> String Boolean) (Sequenceof String))]
  [untyped-mean (-> Summary Real)]
  [variation->mean-runtime (-> Summary String Real)]
)

(define-type Summary summary)

(safe-and-unsafe-provide
 Summary
 from-rktd
 all-variations
 get-num-variations
 get-project-name
 predicate->variations
 untyped-mean
 variation->mean-runtime
)
