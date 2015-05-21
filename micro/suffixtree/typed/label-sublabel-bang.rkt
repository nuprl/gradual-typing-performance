#lang typed/racket/base

(provide sublabel!)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])

;; =============================================================================

;; sublabel!: label number number -> void
;; destructively sets the input label to sublabel.
(: sublabel! (case-> (-> label Index Void)
                     (-> label Index Index Void)))
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
