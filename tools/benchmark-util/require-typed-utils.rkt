 #lang racket

;; BRITTLE macros for conditional requires,
;;  depending on whether the current module is typed or not


(provide
; require/check
 ;; require when the requiree is untyped or the boundary is enabled
 ;; require unsafe submodule when the requiree is typed and boundary is disabled
 
 require/typed/check
 ;; require when the requiree is typed
 ;; require/typed when the requiree is untyped and boundary enabled

; require/typed/check-boundary
 ;; require when the requiree is typed
 ;; require/typed when the requiree is untyped

; require/typed/check-no-boundary
 ;; require when the requiree is typed
 ;; unsafe-require/typed when the requiree is untyped

 require/adapted
 ;; Same as typed require, but can be overriden by a benchmark-config.rktd file

  require/typed/if
  ;; (require/typed/if T E)
  ;; Imports `T` if the current module is typed, and `E` otherwise.

  safe-and-unsafe-provide
  ;; For typed modules, both provides as usual and creates a submodule
  ;; `unsafe` that `unsafe-provide`s the same values
)

(require
  (for-syntax
    typed/untyped-utils
    syntax/parse
    (only-in racket/base prefix-in))
  (only-in typed/racket require/typed)
  (prefix-in typed: (only-in typed/racket
                             require
                             provide))
  (prefix-in typed: (only-in typed/racket/unsafe
                             unsafe-require/typed
                             unsafe-provide))
)

(begin-for-syntax
  (define-syntax-rule (they-are-typed m)
    (module->language-info (syntax->datum #'m) #t))
  (define (i-am-typed) (syntax-local-typed-context?))
  ;; TODO: implement
  ;; m : module-path I think
  ;; Should check metadata file and return #f if the boundary is disabled
  (define (keep-boundary? m) #f))

;; =============================================================================
;; TODO: check for overrides in this and require/adapted
;;

(define-syntax (require/typed/check-boundary stx)
  (syntax-parse stx 
    #:literals (prefix-in)
    [(_ (prefix-in p m:str) rt-clause ...)
     (cond
       [(not (i-am-typed))
        (cond [(or (not (they-are-typed m))
                   (keep-boundary? m))
               #'(require (prefix-in p m))]
              [else
               #'(require (prefix-in p (submod m unsafe)))])]       
       [((they-are-typed m))
        #'(typed:require (prefix-in p m))]
       [(keep-boundary? m)
        #'(require/typed (prefix-in p m) rt-clause ...)]
       [else
        #'(typed:unsafe-require/typed (prefix-in p m) rt-clause ...)])]
    [(_ m:str rt-clause ...)
     (cond
       [(not (i-am-typed))
        (cond [(or (not (they-are-typed m))
                   (keep-boundary? m))
               #'(require m)]
              [else
               #'(require (submod m unsafe))])]
       [(they-are-typed m) #'(typed:require m)]
       [(keep-boundary? m)
        #'(require/typed m rt-clause ...)]
       [else 
        #'(typed:unsafe-require/typed m rt-clause ...)])]))
(define-syntax (require/typed/check stx)
  (syntax-parse stx 
    #:literals (prefix-in)
    [(_ (prefix-in p m:str) rt-clause ...)
     (cond 
       [(not (i-am-typed))
        (cond [(or (not (they-are-typed m))
                   (keep-boundary? m))
               #'(require (prefix-in p m))]
              [else
               #'(require (prefix-in p (submod m unsafe)))])]       
       [((they-are-typed m))
        #'(typed:require (prefix-in p m))]
       [(keep-boundary? m)
        #'(require/typed (prefix-in p m) rt-clause ...)]
       [else
        #'(typed:unsafe-require/typed (prefix-in p m) rt-clause ...)])]
    [(_ m:str rt-clause ...)
     (cond
       [(not (i-am-typed))
        (cond [(or (not (they-are-typed m))
                   (keep-boundary? m))
               #'(require m)]
              [else
               #'(require (submod m unsafe))])]
       [(they-are-typed m) #'(typed:require m)]
       [(keep-boundary? m)
        #'(require/typed m rt-clause ...)]
       [else 
        #'(typed:unsafe-require/typed m rt-clause ...)])]))

(define-syntax (require/adapted stx)
  (syntax-parse stx
    #:literals (prefix-in)
    [(_ m-adaptee:str m-adaptor:str rt-clause ...)
     ;; here I check if the boundary with the adaptee is present, the adaptor is always typed
     (cond
       [(keep-boundary? m-adaptee)
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
