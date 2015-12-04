#lang racket/base

(require (only-in "eval.rkt"
  forth-eval*
))

;; =============================================================================

(define (main)
  (call-with-input-file* (vector-ref (current-command-line-arguments) 0)
    (lambda (p)
      (let-values ([(_e _s) (forth-eval* p)]) (void))))
  (void))

(require contract-profile)
(contract-profile-thunk main)
