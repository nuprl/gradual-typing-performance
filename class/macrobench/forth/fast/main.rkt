#lang racket/base

(require (only-in "eval.rkt"
  forth-eval*
))

;; =============================================================================

(define (main)
  (call-with-input-file* "history.txt"
    (lambda (p)
      (forth-eval* p)))
  (void))

(time (main))
