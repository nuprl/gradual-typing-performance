#lang typed/racket/base

(require
  benchmark-util
  "../base/command-types.rkt")
(require/typed/check "eval.rkt"
  (forth-eval* (-> Input-Port Any))
)

;; =============================================================================

(define (main)
  (call-with-input-file* (ann "../base/history.txt" Path-String)
    (lambda ([p : Input-Port])
      (forth-eval* p)))
  (void))

(time (main))
