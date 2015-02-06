#lang racket/base

(provide
 (struct-out Array)
 (struct-out Mutable-Array)
 (struct-out Settable-Array))

(struct Array (shape
               size
               strict?
               strict!
               unsafe-proc))

(struct Settable-Array Array (set-proc))

(struct Mutable-Array Settable-Array (data))

