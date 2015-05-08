#lang racket/base

(provide name+octave->note)

;; =============================================================================

;; A note is represented using the number of semitones from C0.
(define (name+octave->note name octave)
  (+ (* 12 octave)
     (case name
       [(C) 0]
       [(C# Db) 1]
       [(D) 2]
       [(D# Eb) 3]
       [(E) 4]
       [(F) 5]
       [(F# Gb) 6]
       [(G) 7]
       [(G# Ab) 8]
       [(A) 9]
       [(A# Bb) 10]
       [(B) 11]
       [else 0])))
