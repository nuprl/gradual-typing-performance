#lang racket/base
(require benchmark-util/require-typed-utils)
(require (for-syntax racket/base syntax/parse racket/syntax))

(define (caddddr xs)
  (cadddr (cdr xs)))

(define (cadddddr xs)
  (cadddr (cddr xs)))

(define (caddddddr xs)
  (cadddr (cdddr xs)))


(define-syntax (struct-list stx)
  (syntax-parse stx #:datum-literals (:)
   [(_ name:id ([f*:id : t*] ...))
    #:with ((name-f* cad*r) ...)
      (for/list ([f (in-list (syntax-e #'(f* ...)))]
                 [i (in-naturals 0)])
        (list (format-id stx "~a-~a" (syntax-e #'name) (syntax-e f))
              (format-id stx "ca~ar" (make-string i #\d))))
    #:with name? (format-id stx "~a?" (syntax-e #'name))
    (syntax/loc stx (begin
      (define (name f* ...)
        (list 'name f* ...))
      (safe-and-unsafe-provide name)
      (define (name-f* p)
        (cad*r (cdr p)))
      ...
      (safe-and-unsafe-provide name-f* ...)
    ))]))

;; TODO need to deal with struct inheritance
(struct-list array (
   [shape : (Vectorof Integer)]
   [size : Integer]
   [i:strict? : (Boxof Boolean)]
   [i:strict! : (-> Void)]
   [unsafe-proc : (-> (Vectorof Integer) Float)]
))
(struct-list settable-array (
   [shape : (Vectorof Integer)]
   [size : Integer]
   [i:strict? : (Boxof Boolean)]
   [i:strict! : (-> Void)]
   [unsafe-proc : (-> (Vectorof Integer) Float)]
  [set-proc : ((Vectorof Integer) Float -> Void)]
))
(struct-list mutable-array (
   [shape : (Vectorof Integer)]
   [size : Integer]
   [i:strict? : (Boxof Boolean)]
   [i:strict! : (-> Void)]
   [unsafe-proc : (-> (Vectorof Integer) Float)]
  [set-proc : ((Vectorof Integer) Float -> Void)]
  [data : (Vectorof Float)]
))

(define (Array? x)
  (and (pair? x) (memq (car x) '(array settable-array mutable-array)) #t))
(define array? Array?)

;(define (Settable-Array? (x : Any)) : Boolean
;  (and (pair? x) (memq (car x) '(array 'settable-array)) #t))
(define Settable-Array? array?)
(define settable-array? Settable-Array?)

;(define (Mutable-Array? (x : Any)) : Boolean
;  (and (pair? x) (eq? (car x) 'mutable-array)))
(define Mutable-Array? array?)
(define mutable-array? Mutable-Array?)

(safe-and-unsafe-provide
  Array? array?
  Settable-Array? settable-array?
  Mutable-Array? mutable-array?
)
