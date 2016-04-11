#lang racket/base

(require "struct-def.rkt" racket/match)

(match-define (foo x y) SAMPLE-FOO)
(foo 3 3)
