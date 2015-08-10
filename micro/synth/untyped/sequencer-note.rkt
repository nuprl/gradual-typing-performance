#lang racket/base

(provide note)

;; -----------------------------------------------------------------------------

(require (only-in "sequencer-name-octave-note.rkt" name+octave->note))

;; =============================================================================

;; Single note.
(define (note name octave duration)
  (cons (name+octave->note name octave) duration))
