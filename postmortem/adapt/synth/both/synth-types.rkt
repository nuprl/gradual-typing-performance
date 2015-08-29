#lang typed/racket/base

(provide (all-defined-out))

(require "array-struct-adapted.rkt")


;; From mix: A Weighted-Signal is a (List (Array Float) Real)
(define-type Weighted-Signal (List Array Real))

;; drum patterns are simply lists with either O (bass drum), X (snare) or
;; #f (pause)
(define-type Drum-Symbol (U 'O 'X #f))
(define-type Pattern (Listof Drum-Symbol))
