#lang racket

;; BRITTLE macros for conditional requires,
;;  depending on whether the current module is typed or not


(provide
  require/typed/check
  ;; Same syntax as require/typed, but does not install contracts
  ;;  if the current module AND providing module are typed.
  ;;  Additionally, can be overidden by a benchmark-config.rktd file

  require/adapted
  ;; Same as typed require, but can be overriden by a benchmark-config.rktd file

  require/typed/if
  ;; (require/typed/if T E)
  ;; Imports `T` if the current module is typed, and `E` otherwise.
)

(require
  (for-syntax
    typed/untyped-utils
    syntax/parse
    (only-in racket/base prefix-in))
  (only-in typed/racket require/typed)
  (prefix-in typed: (only-in typed/racket require))
)

(begin-for-syntax
  (define-syntax-rule (they-are-typed m)
    (module->language-info (syntax->datum #'m) #t))
  (define (i-am-typed) (syntax-local-typed-context?)))

;; =============================================================================
;; TODO: check for overrides in this and require/adapted
(define-syntax (require/typed/check stx)
  (syntax-parse stx 
    #:literals (prefix-in)
    [(_ (prefix-in p m:str) rt-clause ...)
     (cond 
       [(not (i-am-typed))
        #'(require (prefix-in p m))]
       [(they-are-typed m)
        #'(typed:require (prefix-in p m))]
       [else 
        #'(require/typed m rt-clause ...)])]
    [(_ m:str rt-clause ...)
     (cond 
       [(not (i-am-typed)) #'(require m)]
       [(they-are-typed m) #'(typed:require m)]
       [else 
        #'(require/typed m rt-clause ...)])]))

(define-syntax (require/adapted stx)
  (syntax-parse stx
    #:literals (prefix-in)
    [(_ (prefix-in p m:str) rt-clause ...)
     #'(require/typed m rt-clause ...)]
    [(_ m:str rt-clause ...)]))

(define-syntax (require/typed/if stx)
  (syntax-parse stx 
    [(_ t e) (if (syntax-local-typed-context?)
           #'(require t)
           #'(require e))]))

