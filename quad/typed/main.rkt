#lang racket/base

;; AKA quick-test.rkt

(require
  "quad-main.rkt"
  "world.rkt"
  "quick-sample.rkt"
  "render.rkt"
  typed/racket/class)

(parameterize ([world:quality-default world:draft-quality])
  (time
    (begin
      (define to (typeset (quick-sample)))
      (send (new pdf-renderer%) render-to-file to "../base/output.pdf")
      (void))))
