#lang racket/base

;; This module provides helper macros for `require/typed`

(require racket/contract/region racket/contract/base
         syntax/location
         (for-syntax racket/base
                     syntax/parse
                     (prefix-in tr: "../typecheck/renamer.rkt")))

(provide require/contract define-ignored)

(define-syntax (define-ignored stx)
  (syntax-case stx ()
    [(_ name expr)
     (syntax-case (local-expand/capture-lifts #'expr
                                              'expression
                                              null #;(list #'define-values))
       (begin define-values)
       [(begin (define-values (n) e) ... e*)
        #`(begin (define-values (n) e) ...
                 (define name #,(syntax-property #'e*
                                                 'inferred-name
                                                 (syntax-e #'name))))]
       [(begin e)
        #`(define name #,(syntax-property #'e
                                          'inferred-name
                                          (syntax-e #'name)))])]))

(define-syntax (get-alternate stx)
  (syntax-case stx ()
    [(_ id)
     (tr:get-alternate #'id)]))

;; Requires an identifier from an untyped module into a typed module
;; nm is the import
;; hidden is an id that will end up being the actual definition
;; nm will be bound to a rename transformer so that it is not provided
;; with all-defined-out
(define-syntax (require/contract stx)
  (define-syntax-class renameable
    (pattern nm:id
             #:with orig-nm #'nm
             #:with orig-nm-r ((make-syntax-introducer) #'nm))
    (pattern (orig-nm:id nm:id)
             #:with orig-nm-r ((make-syntax-introducer) #'nm)))

  (syntax-parse stx
    [(require/contract nm:renameable hidden:id cnt lib)
     ;;bg; create a log for this boundary
     (define bg-message (format "~a\t~a\t~a\t~a" (unbox bg:id-counter) (bg-source->string (syntax-source #'lib)) (syntax-e #'nm) (syntax-e #'lib)))
     (set-box! bg:id-counter (add1 (unbox bg:id-counter)))
     #`(begin (require (only-in lib [nm.orig-nm nm.orig-nm-r]))
              (define-syntax nm.nm
                (make-rename-transformer
                 (syntax-property (syntax-property (quote-syntax hidden)
                                                   'not-free-identifier=? #t)
                                  'not-provide-all-defined #t)))
              ;;bg; declare a new contract
              (printf "[BG:CREATE]\t~a\n" #,bg-message)
              (define-ignored hidden
                (contract cnt
                          (get-alternate nm.orig-nm-r)
                          '(interface for #,(syntax->datum #'nm.nm))
                          ;;bg; hide the log message in the negative-position blame object
                          (list 'bg-typed-contract #,bg-message (current-contract-region))
                          (quote nm.nm)
                          (quote-srcloc nm.nm))))]))

(define-for-syntax bg:id-counter (box 0))
(define-for-syntax (last xs)
  (for/fold ([prev #f]) ([x xs])
    x))
(define-for-syntax (bg-source->string src)
  (cond
   [(path-string? src)
    (path->string (last (explode-path src)))]
   [else
    (error 'require-contract "BG cannot parse filename from '~a'\n" src)]))
