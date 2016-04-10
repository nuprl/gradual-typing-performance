#lang typed/racket/base

(require "card-adapted.rkt")
(provide Stack)
(define-type Stack
  (U (List Card)
     (List Card Card)
     (List Card Card Card)
     (List Card Card Card Card)
     (List Card Card Card Card Card)))
