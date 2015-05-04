#lang typed/racket/base

(provide (struct-out posn)
         Posn)

(require benchmark-util)
(require/typed/check "data-posn.rkt"
  [#:struct posn ([x : Real]
                  [y : Real])])

(define-type Posn posn)
