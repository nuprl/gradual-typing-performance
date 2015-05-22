#lang typed/racket/base

(provide label->string/removing-sentinel)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])
(require/typed/check "label-sentinel.rkt"
  [sentinel? (-> Any Boolean)])
(require/typed/check "label-label-ref.rkt"
  [label-ref (-> label Integer (U Symbol Char))])

;; =============================================================================

(: label->string/removing-sentinel (-> label String))
(define (label->string/removing-sentinel label)
  (let* ([ln (label-length label)]
         [N (if (and (> ln 0) (sentinel? (label-ref label (sub1 ln))))
                (sub1 ln)
                ln)])
    (build-string N (lambda ([i : Integer])
                      (unless (index? i) (error "label->string 1"))
                      (let ([val (label-ref label i)])
                        (unless (char? val) (error "label->string 2"))
                        val)))))
