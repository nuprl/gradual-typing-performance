#lang racket/base

(require benchmark-util
         "data-world.rkt")
(require "aux-tetras.rkt")
(require "aux-list-pick-random.rkt")
(require "main-replay.rkt")

;; =============================================================================

(define (main)
 (define w0 (world (list-pick-random tetras) '()))
 (define raw (with-input-from-file "../base/tetris-hist-3.txt" read))
 (unless (list? raw) (error "input from file is not a list"))
 (replay w0 (reverse raw)))

(time (main))
