#lang typed/racket/base

;; Adapter for gregor's `structs.rkt` file.

(provide
  (struct-out YMD)
  (struct-out HMSN)
  (struct-out comparison)
  build-comparison
  order?
  Month)

(require
  benchmark-util
  "../base/types.rkt")

(require/typed/check "structs.rkt"
  [#:struct YMD ([y : Natural]
                 [m : Month]
                 [d : Natural])]
  [#:struct HMSN ([h : Integer]
                 [m : Integer]
                 [s : Integer]
                 [b : Integer])])

(require/typed/check "compare.rkt"
  [#:opaque Order order?]
  [#:struct comparison ([=? : (-> Any Any Boolean)]
                        [<? : (-> Any Any Boolean)]
                        [<=? : (-> Any Any Boolean)]
                        [>? : (-> Any Any Boolean)]
                        [>=? : (-> Any Any Boolean)]
                        [comparator : (-> Any Any Symbol)]
                        [order : Order])]
  [build-comparison (-> Symbol (-> Any Boolean) (-> Any Exact-Rational) comparison)]
)
