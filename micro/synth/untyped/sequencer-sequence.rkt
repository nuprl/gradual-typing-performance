#lang racket/base

(provide sequence)

;; -----------------------------------------------------------------------------

(require
         "data-array.rkt"
(only-in "array-transform.rkt"
  array-append*)
(only-in "synth-constants.rkt"
  fs)
(only-in "sequencer-synthesize-note.rkt"
  synthesize-note))

;; =============================================================================

;; repeats n times the sequence encoded by the pattern, at tempo bpm
;; pattern is a list of either single notes (note . duration) or
;; chords ((note ...) . duration) or pauses (#f . duration)
(define (sequence n pattern tempo function)
  (define samples-per-beat (quotient (* fs 60) tempo))
  (array-append*
   (for*/list ([i   (in-range n)] ; repeat the whole pattern
                                        [note (in-list  pattern)])
     (define nsamples (* samples-per-beat (cdr note)))
     (synthesize-note (car note) nsamples function))))
