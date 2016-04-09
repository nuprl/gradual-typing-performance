#lang racket/base

(require typed/racket)
(require (for-syntax syntax/parse)
         (for-syntax typed/racket/unsafe)
         (for-syntax "../require-typed-utils.rkt"))

;; A #lang for adaptors to enable prediction configurations
(define-syntax (adaptor-module-begin stx)
  (syntax-parse stx
    [(mb blah ...)
     (define r (datum->syntax stx 'typed/racket))
     (define hijack-no-boundary
       (datum->syntax
        stx
        '(define-syntax require/typed/check (make-rename-transformer #'require/typed/check-no-boundary))))
     (define  hijack-with-boundary
       (datum->syntax
        stx
        '(define-syntax require/typed/check (make-rename-transformer #'require/typed/check-boundary))))
     #`(#%module-begin
        #,hijack-with-boundary
        blah ...
        (module unsafe #,r
          #,hijack-no-boundary
          blah ...)
        )]))

(provide (rename-out [adaptor-module-begin #%module-begin]))
(provide (except-out (all-from-out typed/racket)
                     #%module-begin))
