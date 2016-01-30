#lang typed/racket/base

(require benchmark-util
         (for-syntax racket/base syntax/parse)
         racket/stxparam
         "data-array-adapted.rkt"
         "type-aliases.rkt")

(require/typed/check "sequencer-note.rkt"
  [note (-> Symbol Natural Natural (Pairof Natural Natural))])
(require/typed/check "sequencer-sequence.rkt"
  [sequence (-> Natural (Listof (Pairof (U Natural #f) Natural)) Natural (-> Float (-> Indexes Float)) Array)])

(require/typed/check "drum-drum.rkt"
  [drum (-> Natural Pattern Natural Array)])

(require/typed/check "mixer-mix.rkt"
  [mix (-> Weighted-Signal * Array)])

(require/typed/check "synth-emit.rkt"
  [emit (-> Array (Vectorof Integer))])
(require/typed/check "synth-sawtooth-wave.rkt"
  [sawtooth-wave (-> Float (-> Indexes Float))])

;; =============================================================================

;; Just a shorthand
(define-type Weighted-Signal (List Array Real))

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
(: large-test (-> (Vectorof Integer)))
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
(: small-test (-> (Vectorof Integer)))
(define (small-test)
  (emit
   (mix/sugar
    (sequence 1 (list
      (note 'C 5 1)
      (cons #f 1)
      (note 'C 5 1))
      1200 sawtooth-wave)
    (drum 1 '(O #f #f #f X) 1200))))

(: main (-> Void))
(define (main)
  ;; (large-test)
  (small-test)
  (void))

(time (main))
