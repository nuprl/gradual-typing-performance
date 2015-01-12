#lang racket/base

(require (only-in "typed-mutable-array.rkt" unsafe-vector->array Mutable-Array?))

(provide unsafe-vector->array
         Mutable-Array?)
