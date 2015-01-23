#lang racket
(provide require/typed/check
         require/typed/if)

(require (for-syntax 
          typed/untyped-utils))

(require (for-syntax syntax/parse
                     (only-in racket/base prefix-in)))

(require (only-in typed/racket require/typed))
(require (prefix-in typed: (only-in typed/racket require)))

(define-syntax (require/typed/check stx)
  (syntax-parse stx 
    #:literals (prefix-in)
    [(_ (prefix-in p m:str) rt-clause ...)
     (cond 
       [(not (syntax-local-typed-context?))
        #'(require (prefix-in p m))]
       [(and (syntax-local-typed-context?)
             (module->language-info (syntax->datum #'m) #t))
        #'(typed:require (prefix-in p m))]
       [else 
        #'(require/typed m rt-clause ...)])]
    [(_ m:str rt-clause ...)
     (cond 
       [(not (syntax-local-typed-context?))
        #'(require m)]
       [(and (syntax-local-typed-context?)
             (module->language-info (syntax->datum #'m) #t))
        #'(typed:require m)]
       [else 
        #'(require/typed m rt-clause ...)])]))

(define-syntax (require/typed/if stx)
  (syntax-parse stx 
    [(_ t e) (if (syntax-local-typed-context?)
           #'(require t)
           #'(require e))]))
     
