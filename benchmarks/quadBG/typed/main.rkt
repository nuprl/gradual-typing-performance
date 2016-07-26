#lang typed/racket/base

;; AKA quick-test.rkt

;; -----------------------------------------------------------------------------

(require
 require-typed-check
 "../base/quad-types.rkt"
 (only-in typed/racket/class new send))

(require/typed/check "world.rkt"
  (world:allow-hyphenated-last-word-in-paragraph Boolean)
  (world:quality-default (Parameterof Integer))
  (world:draft-quality Index)
)
(require/typed/check "quad-main.rkt"
  (typeset (-> Quad Quad))
)
(require/typed/check "quick-sample.rkt"
  (quick-sample (-> Quad))
)
(require/typed/check "render.rkt"
  (pdf-renderer%
    (Class
      [render-to-file (Quad Path-String -> Void)]
      [render-element (Quad -> Any)]
      [render-page ((Listof Quad) -> Void)]
      [render-word (Quad -> Any)]
      [render (-> Quad Any)]
      [finalize (-> Any Any)]
      [setup (-> Quad Quad)]))
)

;; =============================================================================

;; 137ms
(parameterize ([world:quality-default world:draft-quality])
  (time
    (begin
      (define to (typeset (quick-sample)))
      (send (new pdf-renderer%) render-to-file to "./output.pdf")
      (delete-file "./output.pdf")
      (void))))
;; 630ms for heart-of-darkness
