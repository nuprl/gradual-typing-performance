#lang typed/racket/base
(require benchmark-util/require-typed-utils)
(require (for-syntax racket/base syntax/parse racket/syntax))

(: caddddr (All (A B C D E F G)
  (case->
   (-> (List A B C D E) E)
   (-> (List A B C D E F) E)
   (-> (List A B C D E F G) E))))
(define (caddddr xs)
  (cadddr (cdr xs)))

(: cadddddr (All (A B C D E F G)
  (case->
   (-> (List A B C D E F) F)
   (-> (List A B C D E F G) F))))
(define (cadddddr xs)
  (cadddr (cddr xs)))

(: caddddddr (All (A B C D E F G) (-> (List A B C D E F G) G)))
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
    #:with Name (format-id stx "~a" (string-titlecase (symbol->string (syntax-e #'name))))
    #:with name? (format-id stx "~a?" (syntax-e #'name))
    #:with Name? (format-id stx "~a?" (syntax-e #'Name))
    (printf "(define-type ~a (Pairof '~a ~a))\n" (syntax->datum #'Name) (syntax->datum #'name) (cons 'List (syntax->datum #'(t* ...))))
    (syntax/loc stx (begin
      (define-type Name (Pairof 'name (List t* ...)))
      (safe-and-unsafe-provide Name)
      (define (name (f* : t*) ...) : Name
        (list 'name f* ...))
      (safe-and-unsafe-provide name)
      (define (name-f* (p : Name)) : t*
        (ann (cad*r (cdr p)) t*))
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

(define (Array? (x : Any)) : Boolean
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
