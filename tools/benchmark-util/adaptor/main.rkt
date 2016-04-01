#lang racket/base

(require typed/racket)
(require (for-syntax syntax/parse)
         (for-syntax typed/racket/unsafe))

(define-syntax (adaptor-module-begin stx)
  (syntax-parse stx
    [(mb blah ...)
     #:with r (datum->syntax stx 'typed/racket)
     #:with u-req (datum->syntax stx
                                 '(require typed/racket/unsafe))
     #:with def-display->displayln
            (datum->syntax stx
              '(define-syntax require/typed (make-rename-transformer #'unsafe-require/typed)))
     #`(#%module-begin
        blah ...
        (module unsafe r
          u-req
          def-display->displayln
          blah ...)
        )]))

(provide (rename-out [adaptor-module-begin #%module-begin]))
(provide (except-out (all-from-out typed/racket)
                     #%module-begin))
