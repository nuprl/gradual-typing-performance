#lang racket/base

;; AKA quick-test.rkt

(require
  "quad-main.rkt"
  "world-typed.rkt"
  "quick-sample-typed.rkt"
  "render-typed.rkt"
  typed/racket/class)

(parameterize ([world:quality-default world:draft-quality])
  (time
    (begin
      (define to (typeset (quick-sample)))
      (send (new pdf-renderer%) render-to-file to "quick-test-typed.pdf")
      (void))))
