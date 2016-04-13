#lang typed/racket
(require benchmark-util)
(require (for-syntax racket/base syntax/parse racket/syntax))
(define-syntax (struct2 stx)
  (syntax-parse stx #:datum-literals (:)
   [(_ name:id ([f*:id : t*] ...))
    #:with ((name-f* i*) ...)
      (for/list ([f (in-list (syntax-e #'(f* ...)))]
                 [i (in-naturals 1)])
        (list (format-id stx "~a-~a" (syntax-e #'name) (syntax-e f)) i))
    #:with Name (format-id stx "~a" (string-titlecase (symbol->string (syntax-e #'name))))
    (syntax/loc stx (begin
      (define-type Name (Pairof 'name (Listof Any)))
      (provide Name)
      (define (name (f* : t*) ...) : Name
        (list 'name f* ...))
      (provide name)
      (define (name-f* (p : Name)) : t*
        (cast (list-ref p 'i*) t*))
      ...
      (provide name-f* ...)
    ))]))

(struct2 posn ([x : Real]
               [y : Real]))
(struct2 block ([x : Real]
                [y : Real]
                [color : Symbol]))
(struct2 tetra ([center : Posn]
                [blocks : (Listof Block)]))
(struct2 world ([tetra : Tetra]
                [blocks : (Listof Block)]))

(: posn=? (-> Posn Posn Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(safe-and-unsafe-provide
 posn=?)
