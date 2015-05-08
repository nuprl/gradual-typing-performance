#lang typed/racket/base

(provide drum)

;; -----------------------------------------------------------------------------

(require "data-array-adapted.rkt"
         "type-aliases.rkt"
         benchmark-util)
(require/typed/check "array-transform.rkt"
  [array-append* (case-> ((Listof Array) -> Array)
                         ((Listof Array) Integer -> Array))])
(require/typed/check "synth-constants.rkt"
  [fs Natural])
(require/typed/check "drum-snare.rkt"
  [snare Array])
(require/typed/check "drum-bass-drum.rkt"
  [bass-drum Array])
(require/typed/check "array-struct-make-array.rkt"
  [make-array ((Vectorof Integer) Float -> Array)])

;; =============================================================================

;; limited drum machine
(: drum (-> Natural Pattern Natural Array))
(define (drum n pattern tempo)
  (: samples-per-beat Natural)
  (define samples-per-beat (quotient (* fs 60) tempo))
  (: make-drum (-> Array Natural Array))
  (define (make-drum drum-sample samples-per-beat)
    (array-append*
     (list drum-sample
           (make-array (vector (- samples-per-beat
                                  (Array-size drum-sample)))
                       0.0))))
  (: O Array)
  (define O     (make-drum bass-drum samples-per-beat))
  (: X Array)
  (define X     (make-drum snare     samples-per-beat))
  (: pause Array)
  (define pause (make-array (vector samples-per-beat) 0.0))
  (array-append*
   (for*/list : (Listof Array) ([i : Integer (in-range n)]
                                        [beat : Drum-Symbol (in-list pattern)])
     (case beat
       [(X)  X]
       [(O)  O]
       [(#f) pause]))))
