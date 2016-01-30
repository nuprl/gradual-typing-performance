#lang racket

(provide
 ;; (struct-open str s field ...)
 ;; creates 
 ;; (define-values (field ...) (values (str-field s) ...)
 ;; so that functions can immediately refer to all fields 
 struct-open)
 

(require (for-syntax racket/syntax racket/struct-info))

(define-syntax (struct-open stx)
  (syntax-case stx ()
    [(struct-open str s x ...)
     ;; syntax-parse 
     ;; -- str: id
     ;; -- s : expression that evaluates to instance of str
     ;; -- x ... : id and distinct plus at least one 
     ;; ---------------------------------------------------
     ;; semantics: 
     ;; -- parents and parent fields 
     ;; -- we may wish to allow renaming (field taget-id)
     (let ((x... (syntax->list #'(x ...))))
       ;; create names for the accessors:
       (define str-x...
         (for/list ((x x...))
           (format-id #'str "~a-~a" #'str (syntax-e x))))
       ;; get the accessors:
       (define-values (str:id _constructor predicate accessors* _mutators* _super-type)
         (apply values (extract-struct-info (syntax-local-value #'str))))
       ;; check that the accessors exist:
       (for/and ((str-x str-x...))
         (or (memf (lambda (x) (free-identifier=? str-x x)) accessors*)
             (raise-syntax-error #f (format "~a not a field of ~a" (syntax-e str-x) str:id))))
       (with-syntax ((str? predicate)
                     ((str-x ...) str-x...))
         #'(define-values (x ...) 
             (if (str? s)
                 (values (str-x s) ...)
                 (error 'struct-open "~a not an instance of ~a" s str)))))]))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  
  (struct foo (bar moo) #:transparent)
  
  (define f (foo 1 'a))
  
  (define (one f)
    (struct-open foo f bar)
    bar)
  
  (check-equal? (one f) (foo-bar f))
  
  (define (two f)
    (struct-open foo f bar moo)
    (cons bar moo))
  
  (check-equal? (two f) (cons (foo-bar f) (foo-moo f)))
  
  (check-exn exn:fail? (lambda () (struct-open foo (cons 1 2)) 1))
  
  (struct-copy foo f (bar 10))

  ;; how to test error:
  ;; (struct-open foo f ggg)
  )

