#lang racket/base

(require (only-in "eval.rkt"
  forth-eval*
))

;; =============================================================================

(define (main)
  (call-with-input-file* (vector-ref (current-command-line-arguments) 0)
    (lambda (p)
      (forth-eval* p)))
  (void))

;(require contract-profile)
(time (main))
