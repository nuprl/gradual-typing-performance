#lang racket/base

(require (only-in typed/untyped-utils require/untyped-contract))
(require/untyped-contract
 (begin (require   (only-in "array-struct.rkt" Array)
                   (only-in typed/racket/base Index Vectorof)))
 "typed-array-constructors.rkt"
 [make-array        (All (A) ((Vectorof Integer) A -> (Array A)))])

(provide make-array)
