#lang typed/racket/base

(require benchmark-util
         "../base/array-types.rkt")

(require/typed/check "array-struct.rkt"
 (#:struct Array ([shape : (Vectorof Integer)]
                   [size : Integer]
                   [strict? : (Boxof Boolean)]
                   [strict! : (-> Void)]
                   [unsafe-proc : (-> (Vectorof Integer) Float)]))
 [array-shape (-> Array (Vectorof Integer))]
 [array-size (-> Array Integer)]
 [unsafe-array-proc (-> Array (-> (Vectorof Integer) Float))]
 [array-default-strict! (-> Array Void)]
 [array-strict? (-> Array Boolean)]
 [array-strictness (Parameterof Boolean)]
 [build-array ((Vectorof Integer) ((Vectorof Integer) -> Float) -> Array)]
 [make-array ((Vectorof Integer) Float -> Array)]
 [unsafe-build-array ((Vectorof Integer) ((Vectorof Integer) -> Float) -> Array)]
 [unsafe-build-simple-array (Indexes (Indexes -> Float) -> Array)]
 ;; -- only for mutable-array.rkt
 [#:struct (Settable-Array Array) ([set-proc : (Indexes Float -> Void)])]
 [#:struct (Mutable-Array Settable-Array) ([data : (Vectorof Float)])]
 [unsafe-vector->array (Indexes (Vectorof Float) -> Mutable-Array)]
)

(provide
 Array
 (rename-out (Array? array?))
 (rename-out (Array-shape array-shape))
 (rename-out (Array-size  array-size))
 (rename-out (Array-unsafe-proc unsafe-array-proc))
 array-default-strict!
 array-strict?
 array-strictness
 build-array
 make-array
 unsafe-build-array
 unsafe-build-simple-array
  Mutable-Array
  unsafe-vector->array
)
