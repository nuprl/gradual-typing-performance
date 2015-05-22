#lang racket/base

(require
  "quad-main.rkt"
  "world.rkt"
  "quick-sample.rkt"
  "render.rkt"
  (only-in racket/class new send))

(parameterize ([world:quality-default world:draft-quality])
  (time
    (begin
      (define to (typeset (quick-sample)))
      (send (new pdf-renderer%) render-to-file to "quick-test-untyped.pdf")
      (void))))
