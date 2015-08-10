#lang typed/racket/base

(provide sequence)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "type-aliases.rkt"
         "data-array-adapted.rkt")
(require/typed/check "array-transform.rkt"
  [array-append* (case-> ((Listof Array) -> Array)
                         ((Listof Array) Integer -> Array))])
(require/typed/check "synth-constants.rkt"
  [fs Natural])
(require/typed/check "sequencer-synthesize-note.rkt"
  [synthesize-note (-> (U #f Natural) Natural (-> Float (-> Indexes Float)) Array)])

;; =============================================================================

;; repeats n times the sequence encoded by the pattern, at tempo bpm
;; pattern is a list of either single notes (note . duration) or
;; chords ((note ...) . duration) or pauses (#f . duration)
(: sequence (-> Natural
                (Listof (Pairof (U Natural #f) Natural))
                Natural
                (-> Float (-> Indexes Float)) Array))
(define (sequence n pattern tempo function)
  (: samples-per-beat Natural)
  (define samples-per-beat (quotient (* fs 60) tempo))
  (array-append*
   (for*/list : (Listof Array) ([i   (in-range n)] ; repeat the whole pattern
                                        [note : (Pairof (U Natural #f) Natural) (in-list  pattern)])
     (: nsamples Natural)
     (define nsamples (* samples-per-beat (cdr note)))
     (synthesize-note (car note) nsamples function))))
