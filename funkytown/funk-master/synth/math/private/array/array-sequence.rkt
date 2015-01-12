#lang racket/base

(require (for-syntax racket/base)
         (only-in typed/untyped-utils require/untyped-contract)
         typed-racket/base-env/prims
         (only-in racket/unsafe/ops unsafe-fx+ unsafe-fx<)
         (only-in "array-struct.rkt"
                  array?
                  array-shape
                  array-size
                  unsafe-array-proc)
         (only-in "utils.rkt" Indexes next-indexes!))

(provide (rename-out [in-array-clause  in-array]))

(define-sequence-syntax in-array-clause
  (λ () #'in-array)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ arr-expr)]
       (syntax/loc stx
         [(x)
          (:do-in
           ([(ds size dims js proc)
             (plet: (A) ([arr : (Array A)  arr-expr])
               (cond [(array? arr)
                      (define ds (array-shape arr))
                      (define dims (vector-length ds))
                      (define size (array-size arr))
                      (define proc (unsafe-array-proc arr))
                      (define: js : Indexes (make-vector dims 0))
                      (values ds size dims js proc)]
                     [else
                      (raise-argument-error 'in-array "Array" arr)]))])
           (void)
           ([j 0])
           (unsafe-fx< j size)
           ([(x)  (proc js)])
           #true
           #true
           [(begin (next-indexes! ds dims js)
                   (unsafe-fx+ j 1))])])]
      [[_ clause] (raise-syntax-error 'in-array "expected (in-array <Array>)" #'clause #'clause)])))
