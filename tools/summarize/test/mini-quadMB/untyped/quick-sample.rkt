#lang racket/base

(provide quick-sample)

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  racket/file
  (only-in racket/include include)
(only-in "quads.rkt"
  page-break
  column-break
  word
  box
  block
 block-break))

;; =============================================================================

(define (quick-sample)
  (block '(measure 240.0 font "Times New Roman" leading 16.0 vmeasure 300.0 size 13.5 x-align justify x-align-last-line left)
         (box '(width 15.0))
         "helloworldthisisaveryveryverylongwordthatisgoingtorequiresomehyphenationquicklynowbeforeitgoesoffthepage"))
