#lang racket
(require (for-syntax racket/base syntax/parse racket/syntax))

(define-syntax (struct2 stx)
  (syntax-parse stx #:datum-literals (:)
   [(_ name:id (f*:id ...))
    #:with ((name-f* i*) ...)
      (for/list ([f (in-list (syntax-e #'(f* ...)))]
                 [i (in-naturals 1)])
        (list (format-id stx "~a-~a" (syntax-e #'name) (syntax-e f)) i))
    (syntax/loc stx (begin
      (define (name f* ...)
        (list 'name f* ...))
      (provide name)
      (define (name-f* p)
        (list-ref p 'i*))
      ...
      (provide name-f* ...)
    ))]))

(struct2 posn (x
               y))
(struct2 block (x
                y
                color))
(struct2 tetra (center
                blocks))
(struct2 world (tetra
                blocks))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(provide posn=?
  posn posn-x posn-y block
  block block-x block-y block-color
  tetra tetra-center tetra-blocks
  world world-tetra world-blocks)
