#lang racket/base

(provide
  log2
  natural->bitstring
  bitstring->natural
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/math exact-ceiling)
  (only-in racket/format ~r)
)

;; =============================================================================

;; log, base 2
;; (: log2 (-> Integer Flonum))
(define (log2 n)
  (exact-ceiling (/ (log n) (log 2))))

;; Convert a natural number to a binary string, padded to the supplied width
;; (: natural->bitstring (-> Index #:pad Index String))
(define (natural->bitstring n #:pad pad-width)
  (~r n #:base 2 #:min-width pad-width #:pad-string "0"))

;; Convert a binary string to a natural number
(define (bitstring->natural str)
  (define N (string-length str))
  (for/sum ([i (in-range N)])
    (define c (string-ref str (- N (add1 i))))
    (if (equal? #\1 c)
        (expt 2 i)
        0)))

;; =============================================================================

(module+ test
  (require rackunit)

  ;; -- log2
  (check-equal? (log2 2) 1)
  (check-equal? (log2 32) 5)
  (check-equal? (log2 1024) 10)

  ;; -- natural->bitstring
  ;(check-equal? (natural->bitstring 0 #:pad 2) "10")
  (check-equal? (natural->bitstring 2 #:pad 2) "10")
  (check-equal? (natural->bitstring 2 #:pad 10) "0000000010")

  ;; -- bitstring->natural
  (check-equal? (bitstring->natural "10") 2)
  (check-equal? (bitstring->natural "00010") 2)
)
