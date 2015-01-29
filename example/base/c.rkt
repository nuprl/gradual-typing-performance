#lang racket

(provide mac)

;; example silly macro that doesn't get ported
(define-syntax-rule (mac x)
  (add1 x))
