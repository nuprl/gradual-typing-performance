#lang racket/base
(require benchmark-util/require-typed-utils)
(require (for-syntax racket/base syntax/parse racket/syntax))
(define-syntax (struct-list stx)
  (syntax-parse stx #:datum-literals (:)
   [(_ name:id (f* ...))
    #:with ((name-f* cad*r) ...)
      (for/list ([f (in-list (syntax-e #'(f* ...)))]
                 [i (in-naturals 0)])
        (list (format-id stx "~a-~a" (syntax-e #'name) (syntax-e f))
              (format-id stx "ca~ar" (make-string i #\d))))
    #:with name? (format-id stx "~a?" (syntax-e #'name))
    #:with make-name (format-id stx "make-~a" (syntax-e #'name))
    (syntax/loc stx (begin
      (define (name? x)
        (and (pair? x) (eq? 'name (car x))))
      (safe-and-unsafe-provide name?)
      (define (name f* ...)
        (list 'name f* ...))
      (safe-and-unsafe-provide name)
      (define make-name name)
      (safe-and-unsafe-provide make-name)
      (define (name-f* p)
        (cad*r (cdr p)))
      ...
      (safe-and-unsafe-provide name-f* ...)
    ))]))
(define-syntax (struct-vector stx)
  (syntax-parse stx #:datum-literals (:)
   [(_ name:id (f* ...))
    #:with ((name-f* set-name-f!* i*) ...)
      (for/list ([f (in-list (syntax-e #'(f* ...)))]
                 [i (in-naturals 1)])
        (list (format-id stx "~a-~a" (syntax-e #'name) (syntax-e f))
              (format-id stx "set-~a-~a!" (syntax-e #'name) (syntax-e f))
              i))
    #:with name? (format-id stx "~a?" (syntax-e #'name))
    #:with make-name (format-id stx "make-~a" (syntax-e #'name))
    (syntax/loc stx (begin
      (define (name? x)
        (and (vector? x) (eq? 'name (vector-ref x 0))))
      (safe-and-unsafe-provide name?)
      (define (name f* ...)
        (vector 'name f* ...))
      (safe-and-unsafe-provide name)
      (define make-name name)
      (safe-and-unsafe-provide make-name)
      (begin
        (define (name-f* p)
          (vector-ref p 'i*))
        (define (set-name-f!* p x)
          (vector-set! p 'i* x)))
      ...
      (safe-and-unsafe-provide name-f* ... set-name-f!* ...)
    ))]))


(struct-vector label (datum i j))

;; A suffix tree consists of a root node.
(struct-list suffix-tree (root))

;; up-label: label
;; parent: (union #f node)
;; children: (listof node)
;; suffix-link: (union #f node)
(struct-vector node (up-label parent children suffix-link))
