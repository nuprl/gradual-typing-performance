#lang racket/base

(provide synthesize-note)

;; -----------------------------------------------------------------------------

(require
         "data-array.rkt"
(only-in "sequencer-note.rkt" note)
(only-in "sequencer-note-freq.rkt"
  note-freq)
(only-in "array-struct-build-array.rkt"
  build-array))

;; =============================================================================

;; Accepts notes or pauses, but not chords.
(define (synthesize-note note n-samples function)
  (build-array (vector n-samples)
               (if note
                   (function (note-freq note))
                   (lambda (x) 0.0)))) ; pause
