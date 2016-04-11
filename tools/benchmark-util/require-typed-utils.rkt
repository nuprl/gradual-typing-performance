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
 ;; same as typed require, but includes the name of the adaptee module

 require/external/typed
 require/external/untyped
 require/external/adapted

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
  racket/function
    typed/untyped-utils
    syntax/parse
    (only-in racket/base prefix-in)
    "override-boundary.rkt")
  (for-syntax racket/syntax)
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

;; Syntax for parsing require specifications
(begin-for-syntax
  (define-syntax-class require-spec
    #:attributes (
      module-name
      ;; (Syntaxof String)
      ;; Syntax object containing the (string) name of the required module
    )
    #:literals (except-in only-in prefix-in)
    (pattern m:str
     #:attr module-name #'m)
    (pattern (prefix-in m:str n*:id ...)
     #:attr module-name #'m)
    (pattern (only-in m:str n*:id ...)
     #:attr module-name #'m)
    (pattern (except-in m:str n*:id ...)
     #:attr module-name #'m)
  )

  ;; Replace the module name required in `old-stx` with `new-stx`
  (define (subst/require-spec old-stx new-stx)
    (syntax-parse old-stx
     #:literals (except-in only-in prefix-in)
     [m:str
      new-stx]
     [(only-in m:str n*:id ...)
      #`(only-in #,new-stx n* ...)]
     [(except-in pre:id m:str)
      #`(prefix-in pre #,new-stx)]
     [(prefix-in pre:id m:str)
      #`(prefix-in pre #,new-stx)]))

  (define (insert-unsafe-if p? mod-name m)
    (cond [(p? mod-name) m]
          [else
           (define submod-ref (datum->syntax mod-name (list 'submod mod-name 'unsafe)))
           (subst/require-spec m submod-ref)])))

(define-syntax (require/check stx)
  (syntax-parse stx
    [(_ m*:require-spec ...)
     (define (p? mod-name)
       (or (not (they-are-typed mod-name))
           (keep-boundary? mod-name)))
     (define m+ (map (curry insert-unsafe-if p?)
                     (syntax-e    #'(m*.module-name ...))
                     (syntax->list #'(m* ...))))
     #`(require #,@m+)]
    [_
     (raise-user-error
      'require/check "Bad/unrecognized syntax in '~a'" (syntax->datum stx))]))

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
    [(_ m-adaptee:str m-adaptor:require-spec)
     (define new-m
       (insert-unsafe-if (λ (_e) (keep-boundary? #'m-adaptee))
                         (attribute m-adaptor.module-name)
                         #'m-adaptor))
     #`(typed:require #,new-m)]))


(define-syntax (require/external/typed stx)
  (syntax-parse stx
    [(_ m rt-clause ...)
     (cond [(act-like-untyped?)
            #`(require/typed m rt-clause ...)]
           [else
            #`(typed:require m)])]))

(define-syntax (require/external/untyped stx)
  (syntax-parse stx
    [(_ m rt-clause ...)
     (cond [(act-like-untyped?)
            #`(typed:unsafe-require/typed m rt-clause ...)]
           [else
            #`(require/typed m rt-clause ...)])]))

(define-syntax (require/external/adapted stx)
  (syntax-parse stx
    [(_ m*:require-spec ...)
     (define new-ms (map (insert-unsafe-if (λ (_e) (act-like-untyped?)))
                         (syntax-e     #'(m*.module-name ...))
                         (syntax->list #'(m* ...))))
     #`(typed:require #,@new-ms)]))

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
