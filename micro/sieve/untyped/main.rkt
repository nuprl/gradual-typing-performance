#lang racket/base

;; Use the partner file "streams.rkt" to implement the Sieve of Eratosthenes.
;; Then compute and print the 10,000th prime number.

(require benchmark-util
         "streams-struct.rkt"
"main-sieve.rkt"
"main-count-from.rkt"
"streams-stream-get.rkt")

;;------------------------------------------------------------------------------

;; stream of prime numbers
;(: primes stream)
(define primes (sieve (count-from 2)))

;; Compute the 10,000th prime number
;(: N-1 Natural)
(define N-1 9999)

;(: main (-> Void))
(define (main)
  (printf "The ~a-th prime number is: ~a\n" (add1 N-1) (stream-get primes N-1)))

(time (main))
