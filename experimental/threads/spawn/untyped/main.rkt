#lang racket/base

(require "../base/num-threads.rkt")

(define (main)
  (define c (make-custodian))
  (parameterize ([current-custodian c])
    (for ([i (in-range NUM-THREADS)])
      (thread (lambda () (void)))))
  (custodian-shutdown-all c))

(time (main))

