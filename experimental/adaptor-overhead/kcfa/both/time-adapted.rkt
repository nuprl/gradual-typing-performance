#lang typed/racket/base

(require
  benchmark-util
  "structs-adapted.rkt"
  "benv-adapted.rkt"
)

(require/typed/check "time.rkt"
  [time-zero Time]
  [k (Parameterof Natural)]
  [tick (-> Stx Time Time)]
  [alloc (-> Time (-> Var Addr))]
)

;; ---

(provide
  time-zero
  k
  tick
  alloc
  Value
)

;; =============================================================================

;; -- time.rkt
(define-type Value Closure)

