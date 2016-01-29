#lang typed/racket/base

(require/typed "../base/num-threads.rkt" [NUM-THREADS Natural])

(: main (-> Void))
(define (main)
  (define c (make-custodian))
  (parameterize ([current-custodian c])
    (for ([i (in-range NUM-THREADS)])
      (thread (lambda () (void)))))
  (custodian-shutdown-all c))

(time (main))

