#lang racket
(require "state.rkt")

(provide build-player0)

(define (build-player0 name tiles name+eplayer)
  (apply player0 name (append tiles (list (second name+eplayer)))))