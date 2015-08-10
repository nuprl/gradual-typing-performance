#lang racket/base

(provide
  (struct-out Array)
  (struct-out Settable-Array)
  (struct-out Mutable-Array))

(define-struct Array (shape
               size
               strict?
               strict!
               unsafe-proc) #:prefab)

(define-struct (Settable-Array Array) (set-proc) #:prefab)

(define-struct (Mutable-Array Settable-Array) (data) #:prefab)
