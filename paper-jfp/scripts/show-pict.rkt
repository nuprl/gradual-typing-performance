#lang racket/base

(provide
  pict->png
  ;; (-> Pict Path-String Boolean)
  ;; Draw a pict to a .png file
)

(require
  (only-in racket/class send)
  (only-in pict pict->bitmap))

;; =============================================================================

(define (pict->png p out-file)
  (define bm (pict->bitmap p))
  (send bm save-file out-file 'png 100))

