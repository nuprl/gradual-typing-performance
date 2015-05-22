#lang racket/base

(require
  "main-typed.rkt"
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
