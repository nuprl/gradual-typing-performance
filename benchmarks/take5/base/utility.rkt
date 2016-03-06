#lang racket

;; this module provides syntactically checked hints for typing modules 

(provide
 ;; (define-type t any-s-expression) :: syntax to call t a type 
 define-type types
 ;; (type t) :: provide syntax to export t, opaquely
 ;; (type t any-s-expression) :: provide syntax to export t, transparently 
 (rename-out [type-out type]))

;; -----------------------------------------------------------------------------
(require (for-syntax racket/provide-transform))

(define-syntax type-out
  (make-provide-pre-transformer
   (lambda (stx phase)
     (syntax-case stx ()
       [(_ id) #'id]
       [(_ id t)
        (begin
          (syntax-local-lift-module-end-declaration #'(define-type id t))
          #'id)]))))

(define-syntax-rule
  (define-type type-id t)
  (define type-id (type 't)))
(struct type (e) #:transparent)

(define-syntax-rule
  (types t ...)
  (begin (unless (type? t) (error 'types "~e not a type" t))
         ...))

