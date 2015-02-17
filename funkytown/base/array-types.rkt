#lang typed/racket/base

(provide Indexes
         In-Indexes
         (struct-out Array)
         (struct-out Settable-Array)
         (struct-out Mutable-Array)
         Weighted-Signal
         Drum-Symbol
         Pattern)

(define-type Indexes (Vectorof Integer))
(define-type In-Indexes Indexes)
;(define-type Indexes (Vectorof Index))
;(define-type In-Indexes (U (Vectorof Integer) Indexes))

(struct: (A) Array ([shape : Indexes]
                    [size : Integer]
                    [strict? : (Boxof Boolean)]
                    [strict! : (-> Void)]
                    [unsafe-proc : (Indexes -> A)]))

(struct: (A) Settable-Array Array ([set-proc : (Indexes A -> Void)]))

(struct: (A) Mutable-Array Settable-Array ([data : (Vectorof A)]))

;; From mix: A Weighted-Signal is a (List (Array Float) Real)
(define-type Weighted-Signal (List (Array Float) Real))

;; drum patterns are simply lists with either O (bass drum), X (snare) or
;; #f (pause)
(define-type Drum-Symbol (U 'O 'X #f))
(define-type Pattern (Listof Drum-Symbol))
