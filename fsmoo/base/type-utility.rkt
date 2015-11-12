#lang typed/racket

(provide
 ;; syntax: (provide/type (f type) ...)
 provide/type)

(define-syntax-rule
  (provide/type (f type) ...)
  (begin (: f type) ... (provide f ...)))