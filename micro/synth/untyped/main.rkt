#lang racket/base

(require
         (for-syntax racket/base syntax/parse)
         racket/stxparam
         "data-array.rkt"
(only-in "sequencer-note.rkt" note)
(only-in "sequencer-sequence.rkt" sequence)
(only-in "drum-drum.rkt" drum)
(only-in "mixer-mix.rkt" mix)
(only-in "synth-emit.rkt" emit)
(only-in "synth-sawtooth-wave.rkt" sawtooth-wave))

;; =============================================================================

(begin-for-syntax
 (define-syntax-class mixand
   #:attributes (signal weight)
   (pattern [signal:expr (~datum #:weight) weight:expr])
   (pattern signal:expr #:with weight #'1)))

(define-syntax (mix/sugar stx)
  (syntax-parse stx
    [(_ sig:mixand ...)
     #'(mix (list sig.signal sig.weight) ...)]))

;; Test from Vincent's repo.
(define (large-test)
 (emit
  (mix/sugar
   (sequence 2 (list
     (note 'C 5 1)
     (cons #f 1)
     (note 'C 5 1)
     (cons #f 1)
     (note 'A# 4 1)
     (cons #f 1)
     (note 'C 5 1)
     (cons #f 3)
     (note 'G 4 1)
     (cons #f 3)
     (note 'G 4 1)
     (cons #f 1)
     (note 'C 5 1)
     (cons #f 1)
     (note 'F 5 1)
     (cons #f 1)
     (note 'E 5 1)
     (cons #f 1)
     (note 'C 5 1)
     (cons #f 9))
     380 sawtooth-wave)
   (drum 8 '(O #f #f #f X #f #f #f) 380))))

;; Small test, for development
(define (small-test)
  (emit
   (mix/sugar
    (sequence 1 (list
      (note 'C 5 1)
      (cons #f 1)
      (note 'C 5 1))
      1200 sawtooth-wave)
    (drum 1 '(O #f #f #f X) 1200))))

(define (main)
  ;; (large-test)
  (small-test)
  (void))

(time (main))
