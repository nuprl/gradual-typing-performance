#lang typed/racket/base

(provide synthesize-note)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "type-aliases.rkt"
         "data-array-adapted.rkt")
(require/typed/check "sequencer-note.rkt"
  [note (-> Symbol Natural Natural (Pairof Natural Natural))])
(require/typed/check "sequencer-note-freq.rkt"
  [note-freq (-> Natural Float)])
(require/typed/check "array-struct-build-array.rkt"
  [build-array ((Vectorof Integer) ((Vectorof Integer) -> Float) -> Array)])

;; =============================================================================

;; Accepts notes or pauses, but not chords.
(: synthesize-note (-> (U #f Natural)
                       Natural
                       (-> Float (-> Indexes Float))
                       Array))
(define (synthesize-note note n-samples function)
  (build-array (vector n-samples)
               (if note
                   (function (note-freq note))
                   (lambda (x) 0.0)))) ; pause
