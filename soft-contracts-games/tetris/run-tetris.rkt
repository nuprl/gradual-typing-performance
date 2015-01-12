#lang racket  

(require (prefix-in t: "tetris.rkt"))

(define t:hist (reverse (with-input-from-file "tetris-hist-3.txt" read)))

(define (contract-main)
  (t:replay t:w0 t:hist))

(time (begin (contract-main) (void)))
