#lang racket/base

(provide
  ;; -- all `defparam2`
)

;; -----------------------------------------------------------------------------
;; -- Parameters
(define-syntax-rule (defparam2 a b c)
  (begin
    (define a (make-parameter c))
    (provide a)))

(defparam2 *CACHE?* Boolean #t)

(defparam2 *BENCHMARK-TABLE-CACHE* Path-String "cache-benchmark-table.rktd")
(defparam2 *BENCHMARK-SAVINGS-CACHE* Path-String "cache-benchmark-savings.rktd")
(defparam2 *DRAFT?* Boolean #t)
(defparam2 *LNM-TABLE-CACHE* Path-String "cache-lnm-table.rktd") ;; Place to store cached lnm table
(defparam2 *LNM-TABLE-DATA-CACHE* Path-String "cache-lnm-table-data.rktd")
(defparam2 *LNM-OVERHEAD* (Listof Exact-Rational) '(1/5 3 10))
(defparam2 *RKT-VERSIONS* (Listof String) '("6.2" "6.3" "6.4"))
(defparam2 *EXACT-TABLE-CACHE* Path-String "cache-exact-table.rktd")

;; -----------------------------------------------------------------------------

(defparam2 *NUM-BENCHMARKS* Natural 0)
(defparam2 *NUM-OO-BENCHMARKS* Natural 0)
(defparam2 *TOTAL-NUM-CONFIGURATIONS* Natural 0)

