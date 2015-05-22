#lang typed/racket/base

(provide note)

;; -----------------------------------------------------------------------------

(require benchmark-util)
(require/typed/check "sequencer-name-octave-note.rkt"
  [name+octave->note (-> Symbol Natural Natural)])

;; =============================================================================

;; Single note.
(: note (-> Symbol Natural Natural (Pairof Natural Natural)))
(define (note name octave duration)
  (cons (name+octave->note name octave) duration))
