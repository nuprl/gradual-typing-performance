#lang racket/base

(require
 (only-in "private/array/array-broadcast.rkt"     array-broadcasting)
 (only-in "private/array/array-comprehension.rkt" for/array)
 (only-in "private/array/array-constructors.rkt" make-array)
 (only-in "private/array/array-pointwise.rkt"     array-map)
 (only-in "private/array/array-sequence.rkt"      in-array)
 (only-in "private/array/array-struct.rkt"        Array array-size build-array array-strictness)
 (only-in "private/array/array-transform.rkt"     array-append*))

(provide Array array-size build-array array-strictness
         in-array
         for/array
         make-array
         array-map
         array-broadcasting
         array-append*)
