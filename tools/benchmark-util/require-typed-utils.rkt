#lang racket

;; BRITTLE macros for conditional requires,
;;  depending on whether the current or requiree module is typed or not

(provide
 require/check
 ;; for untyped modules
 ;; require when the requiree is untyped or the boundary is enabled
 ;; require unsafe submodule when the requiree is typed and boundary is disabled

 require/typed/check
 ;; for typed modules
 ;; require when the requiree is typed
 ;; require/typed when the requiree is untyped and boundary enabled

 require/typed/check-boundary
 ;; for typed modules
 ;; require when the requiree is typed
 ;; require/typed when the requiree is untyped

 require/typed/check-no-boundary
 ;; for typed modules
 ;; require when the requiree is typed
 ;; unsafe-require/typed when the requiree is untyped

 require/adapted
 ;; for typed modules
 ;; Same as typed require, but can be overriden by a benchmark-config.rktd file

  require/typed/if
  ;; (require/typed/if T E)
  ;; Imports `T` if the current module is typed, and `E` otherwise.

  safe-and-unsafe-provide
  ;; for typed modules
  ;; Both provides as usual and creates a submodule
  ;; `unsafe` that `unsafe-provide`s the same values
)

(require
  (for-syntax
    typed/untyped-utils
    syntax/parse
    (only-in racket/base prefix-in)
    "override-boundary.rkt")
  (only-in typed/racket require/typed)
  (prefix-in typed: (only-in typed/racket
                             require
                             provide))
  (prefix-in typed: (only-in typed/racket/unsafe
                             unsafe-require/typed
                             unsafe-provide)))

(begin-for-syntax
  (define (they-are-typed m)
    (module->language-info (syntax->datum m) #t))
  (define (i-am-typed)
    (syntax-local-typed-context?)))

;; =============================================================================
;; TODO: check for overrides in this and require/adapted
;;

(define-syntax (require/check stx)
  (syntax-parse stx
    [(_ m*:str ...)
     #:with (m+* ...)
       (for/list ([m (in-list (syntax-e #'(m* ...)))])
         (if (or (not (they-are-typed m))
                 (keep-boundary? m))
           m
           #`(submod #,m unsafe)))
     #'(require m+* ...)]
    [_
     (raise-user-error
       'require/check
       "Bad syntax in '~a'.\n  (If you need support for 'prefix-in' etc. we can add it.)"
       (syntax->datum stx))]))

(define-for-syntax (param-require/typed/check stx my-require/typed)
  (syntax-parse stx
    #:literals (prefix-in)
    [(_ (prefix-in p m:str) rt-clause ...)
     (cond
       [((they-are-typed #'m))
        #'(typed:require (prefix-in p m))]
       [else
        #'(my-require/typed (prefix-in p m) rt-clause ...)])]
    [(_ m:str rt-clause ...)
     (cond
       [(they-are-typed #'m) #'(typed:require m)]
       [else
        #`(#,my-require/typed m rt-clause ...)])]))

(define-syntax (require/typed/check-boundary stx)
  (param-require/typed/check stx #'require/typed)
  )

(define-syntax (require/typed/check-no-boundary stx)
  (param-require/typed/check stx #'typed:unsafe-require/typed))

(define-for-syntax (decide-boundary m)
  (cond [(keep-boundary? m) #'require/typed/check-boundary]
        [else               #'require/typed/check-no-boundary]))

(define-syntax (require/typed/check stx)
  (syntax-parse stx
    #:literals (prefix-in)
    [(_ (prefix-in p m:str) rt-clause ...)
     #`(#,(decide-boundary #'m) (prefix-in p m) rt-clause ...)]
    [(_ m:str rt-clause ...)
     #`(#,(decide-boundary #'m) m rt-clause ...)]))

(define-syntax (require/adapted stx)
  (syntax-parse stx
    #:literals (prefix-in)
    [(_ m-adaptee:str m-adaptor:str rt-clause ...)
     ;; here I check if the boundary with the adaptee is present, the
     ;; adaptor is always typed
     (cond
       [(keep-boundary? #'m-adaptee)
        #'(typed:require m-adaptor rt-clause ...)]
       [#'(typed:require (submod m-adaptor unsafe) rt-clause ...)])]))

(define-syntax (require/typed/if stx)
  (syntax-parse stx
    [(_ t e) (if (syntax-local-typed-context?)
           #'(require t)
           #'(require e))]))

(define-syntax (safe-and-unsafe-provide stx)
  (syntax-parse stx
    [(_ p-clause ...)
     #'(begin
         (typed:provide p-clause ...)
         (module* unsafe #f
           (typed:unsafe-provide p-clause ...)))]))
