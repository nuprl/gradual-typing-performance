#lang typed/racket/base

;; AKA quick-test.rkt

;; -----------------------------------------------------------------------------

(require
 benchmark-util
 "../base/core-types.rkt"
 "../base/quad-types.rkt"
 (only-in typed/racket/class new send)
)
(require/typed/check "world.rkt"
  [world:allow-hyphenated-last-word-in-paragraph Boolean]
  [world:quality-default (Parameterof Index)]
  [world:draft-quality Index])
(require/typed/check "quad-main.rkt"
  [typeset (-> Quad DocQuad)])
(require/typed/check "quick-sample.rkt"
  [quick-sample (-> Quad)])
(require/typed/check "render.rkt"
  ;; TODO worried about this signature
  [pdf-renderer% (Class [render-to-file (Quad Path-String -> Void)])])

;; =============================================================================

(parameterize ([world:quality-default world:draft-quality])
  (time
    (begin
      (define to (typeset (quick-sample)))
      (send (new pdf-renderer%) render-to-file to "../base/output.pdf")
      (void))))
