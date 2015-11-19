#lang racket/base

(provide
  quad
  quad?
  quad-attrs?)

;; =============================================================================

(define (quad name attrs items)
  (list* name attrs items))

(define (quad? x)
  (and (list? x)
       (not (eq? '() x))
       (symbol? (car x))
       (list? (cadr x))))

(define (quad-attrs? xs)
  (and (list? xs)
       (for/and ([x xs])
         (and (pair? x) (symbol? (car x))))))

