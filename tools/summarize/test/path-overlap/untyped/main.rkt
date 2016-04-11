#lang racket/base

(require "lib.rkt" "../src/lib.rkt")

(define x (+ CONST (lib-fun)))
(begin lib-val (void))
