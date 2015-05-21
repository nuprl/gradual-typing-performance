#lang racket/base

(provide sublabel)

;; -----------------------------------------------------------------------------

(require "data-label.rkt"
(only-in "label-label-length.rkt" label-length))

;; =============================================================================

;; sublabel: label number number -> label
;; Gets a slice of the label on the half-open interval [i, j)
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
