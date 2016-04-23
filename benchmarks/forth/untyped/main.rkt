#lang racket/base

(require benchmark-util)
(require/check (only-in "eval.rkt"
  forth-eval*
))

;; =============================================================================

(define (main)
  (call-with-input-file* "../base/history.txt"
    (lambda (p)
      (let-values ([(_e _s) (forth-eval* p)]) (void))))
  (void))

(time (main))
