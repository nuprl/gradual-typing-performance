#lang typed/racket/base

(provide note-freq)

;; =============================================================================

;; details at http://www.phy.mtu.edu/~suits/notefreqs.html
(: note-freq (-> Natural Float))
(define (note-freq note)
  ;; A4 (440Hz) is 57 semitones above C0, which is our base.
  (: res Nonnegative-Real)
  (define res (* 440 (expt (expt 2 1/12) (- note 57))))
  (if (flonum? res) res (error "not real")))
