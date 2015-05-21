#lang typed/racket/base

(provide sublabel)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])

;; =============================================================================

;; sublabel: label number number -> label
;; Gets a slice of the label on the half-open interval [i, j)
(: sublabel (case-> (-> label Index label)
                    (-> label Index Index label)))
(define sublabel
  (case-lambda
    ((lbl i)
     (sublabel lbl i (label-length lbl)))
    ((lbl i j)
     (unless (<= i j)
       (error 'sublabel "illegal sublabel [~a, ~a]" i j))
     (label (label-datum lbl)
                 (+ i (label-i lbl))
                 (+ j (label-i lbl))))))
