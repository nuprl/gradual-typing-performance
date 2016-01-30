#lang typed/racket

(require (for-syntax syntax/parse))

(provide require/typed/check
         cond-untyped)

(define-for-syntax (process-rt-clause stx)
  stx ; need to fix this to remove the type information
  )

(define-for-syntax (process-rt-clauses stxs)
  (define lst (syntax->list stxs))
  (map process-rt-clause lst))

(define-syntax (require/typed/check stx)
  (syntax-case stx (define-type)
    [(_ m rt-clause ...)
     (let ([lang-info (module->language-info (syntax->datum #'m) #t)])
       (and lang-info 
            (equal? (vector-ref lang-info 0) 'typed-racket/language-info)))
     #`(require m)]
    [(_ m rt-clause ...) 
     #'(require/typed m rt-clause ...)]))

(define-syntax (cond-untyped stx)
  (syntax-parse stx
    [(_ m:str e1 e2)
     (define mod-in (open-input-file (syntax->datum #'m)))
     (define lang-line (read-line mod-in))
     ;; Depending on the file structure is awfully hacky, but unfortunately
     ;; using any form of reflection here tends to fail because it requires
     ;; loading the target module. This can cause cyclic loading problems
     ;; which are basically unavoidable.
     (define typed? (regexp-match? #rx"#lang typed/racket" lang-line))
     (close-input-port mod-in)
     (if typed?
         #'e2
         #'e1)]))
