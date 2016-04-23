#lang typed/racket/base
(require benchmark-util/require-typed-utils)
(require (for-syntax racket/base syntax/parse racket/syntax))
(define-syntax (struct-list stx)
  (syntax-parse stx #:datum-literals (:)
   [(_ name:id ([f*:id : t*] ...))
    #:with ((name-f* cad*r) ...)
      (for/list ([f (in-list (syntax-e #'(f* ...)))]
                 [i (in-naturals 0)])
        (list (format-id stx "~a-~a" (syntax-e #'name) (syntax-e f))
              (format-id stx "ca~ar" (make-string i #\d))))
    #:with Name (format-id stx "~a" (string-titlecase (symbol->string (syntax-e #'name))))
    #:with name? (format-id stx "~a?" (syntax-e #'name))
    (printf "(define-type ~a (Pairof '~a ~a))\n" (syntax->datum #'Name) (syntax->datum #'name) (cons 'List (syntax->datum #'(t* ...))))
    (syntax/loc stx (begin
      (define-type Name (Pairof 'name (List t* ...)))
      (safe-and-unsafe-provide Name)
      (define (name? (x : Any)) : Boolean
        (and (pair? x) (eq? 'name (car x))))
      (safe-and-unsafe-provide name?)
      (define (name (f* : t*) ...) : Name
        (list 'name f* ...))
      (safe-and-unsafe-provide name)
      (define (name-f* (p : Name)) : t*
        (ann (cad*r (cdr p)) t*))
      ...
      (safe-and-unsafe-provide name-f* ...)
    ))]))
(define-syntax (struct-vector stx)
  (syntax-parse stx #:datum-literals (:)
   [(_ name:id ([f*:id : t*] ...))
    #:with ((name-f* set-name-f!* i*) ...)
      (for/list ([f (in-list (syntax-e #'(f* ...)))]
                 [i (in-naturals 1)])
        (list (format-id stx "~a-~a" (syntax-e #'name) (syntax-e f))
              (format-id stx "set-~a-~a!" (syntax-e #'name) (syntax-e f))
              i))
    #:with Name (format-id stx "~a" (string-titlecase (symbol->string (syntax-e #'name))))
    #:with name? (format-id stx "~a?" (syntax-e #'name))
    (printf "(define-type ~a ~a)\n" (syntax->datum #'Name)
      (cons 'Vector (cons (syntax->datum #'name) (syntax->datum #'(t* ...)))))
    (syntax/loc stx (begin
      (define-type Name (Vector 'name t* ...))
      (safe-and-unsafe-provide Name)
      (define (name? (x : Any)) : Boolean
        (and (vector? x) (eq? 'name (vector-ref x 0))))
      (safe-and-unsafe-provide name?)
      (define (name (f* : t*) ...) : Name
        (vector 'name f* ...))
      (safe-and-unsafe-provide name)
      (begin
        (define (name-f* (p : Name)) : t*
          (ann (vector-ref p 'i*) t*))
        (define (set-name-f!* (p : Name) (x : t*)) : Void
          (vector-set! p 'i* x)))
      ...
      (safe-and-unsafe-provide name-f* ... set-name-f!* ...)
    ))]))


(struct-vector label ([datum : (Vectorof (U Char Symbol))] [i : Natural] [j : Natural]))

;; A suffix tree consists of a root node.
(struct-list suffix-tree ([root : Node]))

;; up-label: label
;; parent: (union #f node)
;; children: (listof node)
;; suffix-link: (union #f node)
(struct-vector node ([up-label : Label] [parent : (U #f Node)] [children : (Listof Node)] [suffix-link : (U #f Node)]))

(define-type Tree Suffix-Tree)
(define make-label label)
(define make-node node)
(define make-suffix-tree suffix-tree)

(safe-and-unsafe-provide Tree make-label make-suffix-tree make-node)

