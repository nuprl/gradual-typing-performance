#lang racket/base

(provide sublabel!)

;; -----------------------------------------------------------------------------

(require "data-label.rkt"
(only-in "label-label-length.rkt" label-length))

;; =============================================================================

;; sublabel!: label number number -> void
;; destructively sets the input label to sublabel.
(define sublabel!
  (case-lambda
    ((label i)
     (sublabel! label i (label-length label)))
    ((label i j)
     (begin
       ;; order dependent code ahead!
       (set-label-j! label (+ j (label-i label)))
       (set-label-i! label (+ i (label-i label)))
       (void)))))
