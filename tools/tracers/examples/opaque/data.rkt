#lang racket/base

(provide adt? v1 get-tag)

(struct adt (tag val rest))

(define v0 (adt 'Nil #f #f))
(define v1 (adt 'Cons 1 v0))

(define get-tag adt-tag)
