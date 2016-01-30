#lang racket/base

(provide label->string/removing-sentinel)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
(only-in "label-label-length.rkt" label-length)
(only-in "label-sentinel.rkt" sentinel?)
(only-in "label-label-ref.rkt" label-ref))

;; =============================================================================

(define (label->string/removing-sentinel label)
  (let* ([ln (label-length label)]
         [N (if (and (> ln 0) (sentinel? (label-ref label (sub1 ln))))
                (sub1 ln)
                ln)])
    (build-string N (lambda (i)
                      (unless (integer? i) (error "label->string 1"))
                      (let ([val (label-ref label i)])
                        (unless (char? val) (error "label->string 2"))
                        val)))))
