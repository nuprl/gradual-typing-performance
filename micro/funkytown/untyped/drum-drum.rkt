#lang racket/base

(provide drum)

;; -----------------------------------------------------------------------------

(require "data-array.rkt"
(only-in "array-transform.rkt" array-append*)
(only-in "synth-constants.rkt" fs)
(only-in "drum-snare.rkt" snare)
(only-in "drum-bass-drum.rkt" bass-drum)
(only-in "array-struct-make-array.rkt" make-array))

;; =============================================================================

;; limited drum machine
(define (drum n pattern tempo)
  (define samples-per-beat (quotient (* fs 60) tempo))
  (define (make-drum drum-sample samples-per-beat)
    (array-append*
     (list drum-sample
           (make-array (vector (- samples-per-beat
                                  (Array-size drum-sample)))
                       0.0))))
  (define O     (make-drum bass-drum samples-per-beat))
  (define X     (make-drum snare     samples-per-beat))
  (define pause (make-array (vector samples-per-beat) 0.0))
  (array-append*
   (for*/list ([i (in-range n)]
                                        [beat (in-list pattern)])
     (case beat
       [(X)  X]
       [(O)  O]
       [(#f) pause]))))
