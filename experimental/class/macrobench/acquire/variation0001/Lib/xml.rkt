#lang racket

;; ---------------------------------------------------------------------------------------------------
;; provide auxiliaries for recognizing XML inputs and differentiating Xexprs 

(require (only-in xml xexpr?))

(provide
 (contract-out
  (xml-element-diff
   ;; determine differences between two Xexpr without consideration to attribute order 
   ;; and without consideration of msg fields in 'error' elements 
   (-> xexpr? xexpr? boolean?))
  
  (numbered-attributes
   ;; create a list of attributes numbered sequentially '((foo1 "1") (foo2 "2") (foo3 "3"))
   (-> string? (-> any/c xexpr?) (listof any/c) (listof (list/c symbol? string?)))))
 
 ;; --------------------------------------------------------------------------------------------------

 ;; parameter: Boolean -> Void U -> Boolean 
 ;; shows error messages in error elements 
 verbose 

 ;; syntax: 
 ;;  (predicate (tag ((attribute class?) ...) nested? ...) ...)
 ;; defines a predicate
 ;;   Xexpr -> Boolean 
 ;; nested? is one of: 
 ;;   -- a predicate
 ;;   -- ... to indicate a repetition of the preceding predicate 
 xml-predicate 
 
 
 ;; syntax: 
 ;;  (xml-parser (tag ((attribute class?) ...) nested ... #:action body:expr) ... optional-else)
 ;; defines a parser 
 ;;   Xexpr -> X
 ;; nested is one of: 
 ;;   -- a named predicate (id pred?)
 ;;   -- ... to indicate a repetition of the preceding predicate 
 ;; attribute ... nested.id ... are bound in body
 ;; optional-else is
 ;; [else expr]
 xml-parser)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(require racket/syntax)

(module+ test 
  (require rackunit))

(define verbose (make-parameter #f))

;; Xexpr Xexpr -> Boolean 
(define (xml-element-diff x y)
  (define x-tag (first x))
  (define y-tag (first y))
  (cond 
    [(eq? x-tag y-tag) (xml-attribute-diff x-tag (rest x) (rest y))]
    [else (printf "distinct elements:\n")
          (printf "  1: ~e\n" x-tag)
          (printf "  2: ~e\n" y-tag)
          #f]))

;; Xattr = empty | (cons Attributes [Listof Xexpr])
;; SyXattr Xattr -> Boolean 
(define (xml-attribute-diff tag attr+body1 attr+body2)
  (cond
    [(and (empty? attr+body1) (empty? attr+body2)) #t]
    [(empty? attr+body1) 
     (printf "distinct attributes:\n")
     (printf "  1: empty\n")
     (printf "  2: ~e\n" attr+body2)
     #f]
    [(empty? attr+body2)
     (printf "distinct attributes:\n")
     (printf "  1: ~e\n" attr+body1)
     (printf "  2: empty\n")
     #f]
    [else 
     (define-values (x-attr x-err) (normalize-attribute tag (first attr+body1)))
     (define-values (y-attr y-err) (normalize-attribute tag (first attr+body2)))
     (cond
       [(equal? x-attr y-attr) 
        (when (and x-err (verbose))
          (printf "error messages:\n")
          (printf "  1: ~a\n" x-err)
          (printf "  2: ~a\n" y-err))
        (define body1 (rest attr+body1))
        (define body2 (rest attr+body2))
        (cond
          [(= (length body1) (length body2))
           (andmap xml-element-diff body1 body2)]
          [else 
           (printf "distinct element body list:\n")
           (printf "  1: ~e\n" body1)
           (printf "  2: ~e\n" body2)
           #f])]
       [else (printf "distinct attributes:\n")
             (printf "  1: ~e\n" x-attr)
             (printf "  2: ~e\n" y-attr)
             #f])]))

;; Symbol [Listof [List Symbol String]] ->* [Listof [List Symbol (U String Number)]] (U String false)
(define (normalize-attribute tag a)
  (define attr (sort a symbol<=? #:key first))
  (define msg #f)
  (values (for/list ((a attr))
            (define val (second a))
            (cond
              [(eq? 'error tag) (set! msg val) (list (first a) "")]
              [else (define num (string->number val))
                    (list (first a) (or  num val)) #f])) 
          ;; relying on left-to-right order here:
          msg))

;; Symbol Symbol -> Boolean 
(define (symbol<=? s t)
  (string<=? (symbol->string s) (symbol->string t)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)
  
  (check-true (xml-element-diff '(p) '(p)))
  (check-true (xml-element-diff '(p ((a1 "hello"))) '(p ((a1 "hello")))))
  (check-true (xml-element-diff '(p ((a2 "world") (a1 "hello"))) '(p ((a1 "hello") (a2 "world")))))
  (check-true
   (xml-element-diff
    '(p ((a2 "world") (a1 "hello")) (a) (b))
    '(p ((a1 "hello") (a2 "world")) (a) (b))))
  (check-true
   (xml-element-diff
    '(p ((a2 "world") (a1 "hello")) (a ((a2 "world") (a1 "hello"))) (b))
    '(p ((a1 "hello") (a2 "world")) (a ((a2 "world") (a1 "hello"))) (b))))
  (check-true
   (xml-element-diff
    '(p ((a2 "world") (a1 ".5")) (a ((a2 "world") (a1 "hello"))) (b))
    '(p ((a1 "0.5") (a2 "world")) (a ((a2 "world") (a1 "hello"))) (b))))
  (check-true (xml-element-diff '(error [[msg "a"]]) '(error [[msg "b"]])))
  
  (check-false
   (xml-element-diff
    '(p ((a2 "world") (a1 ".5")) (a) (b))
    '(p ((a1 "hello") (a2 "world")) (a) (b) (c))))
  (check-false
   (xml-element-diff
    '(p ((a2 "world") (a1 "hello")) (a) (b))
    '(p ((a1 "hello") (a2 "world")) (a) (b) (c))))
  )

;; ---------------------------------------------------------------------------------------------------
(define-syntax (xml-predicate stx)
  (syntax-case stx ()
    [(_ (tag ((attribute attribute?) ...) nested? ...) ...)
     (with-syntax ([((nested ...) ...)
                    (map (λ (stx)
                           (map (λ (stx)
                                  (syntax-case stx (...)
                                    [... '...]
                                    [nested #'(? nested)]))
                                (syntax->list stx)))
                         (syntax->list #'((nested? ...) ...)))])
       #`(lambda (x)
           (match x 
             [(list 'tag (list-no-order `(attribute ,(? attribute? attribute)) ...)
                    nested ...)
              #t]
             ...
             (else #f))))]))

(define-syntax (xml-parser stx)
  (syntax-case stx (else)
    [(_ (tag ((attribute check-attribute) ...) nested? ... #:action action) ... [else e])
     (let* ([ns 
             (map (λ (stx)
                    (map (λ (stx)
                           (syntax-case stx (...)
                             [... '...]
                             [(n nested) #'(? nested n)]))
                         (syntax->list stx)))
                  (syntax->list #'((nested? ...) ...)))]
            [bind-single
             (lambda (stx)
               (syntax-case stx ()
                 [(? nested n) #'[n (nested n)]]
                 [_ stx]))]
            [bind-map
             (lambda (stx)
               (syntax-case stx ()
                 [(? nested n) #'[n (map nested n)]]
                 [_ stx]))]
            [parsed-ns 
             (for/list ((ns ns))
               (let loop ((ns ns))
                 (cond
                   [(null? ns) '()]
                   [(null? (cdr ns)) (cons (bind-single (car ns)) (loop (cdr ns)))]
                   [else 
                    (if (eq? (cadr ns) '...)
                        (cons (bind-map (car ns)) (loop (cddr ns)))
                        (cons (bind-single (car ns)) (loop (cdr ns))))])))])
       
       (with-syntax ([((n ...) ...) ns]
                     [((n-parsed ...) ...) parsed-ns])
         #`(lambda (x)
             ; (displayln `(parser ,x))
             (match x 
               [(list 'tag (list-no-order `(attribute ,(? check-attribute attribute)) ...) n ...)
                (let ((attribute (check-attribute attribute))
                      ...)
                  (let (n-parsed ...)
                    action))]
               ...
               [else e]))))]
    [(_ (tag ((attribute check-attribute) ...) nested? ... #:action action) ...)
     #'(xml-parser (tag ((attribute check-attribute) ...) nested? ... #:action action) ... [else #f])]))

(module+ test
  (define (tile r c) `(tile ((row ,r) (column ,c))))
  (define (hotel n . t) (list* 'hotel `((name ,n)) t))
  
  (define board? (xml-predicate (board () tile? ... hotel? ...)))
  (define hotel? (xml-predicate (hotel ((name label?)) tile? ...)))
  (define tile?  (xml-predicate (tile ((column column?) (row row?)))))
  
  (define label? string?)
  (define column? string?)
  (define row? string?)
  
  ;; -------------------------------------------------------------------------------------------------
  (check-true (tile? (tile "1" "a")))
  (check-true (hotel? (hotel "American" (tile "1" "a") (tile "2" "a"))))
  (check-true (label? "hello"))
  (check-true (board? `(board ())))
  (check-true
   (board? `(board ()
                   ,(tile "1" "A") ,(tile "1" "A"))))
  (check-true
   (board? `(board ()
                   ,(hotel "American" (tile "1" "B")))))
  (check-true
   (board? `(board ()
                   ,(tile "1" "A") ,(tile "1" "A")
                   ,(hotel "American" (tile "1" "B")))))
  
  ;; -------------------------------------------------------------------------------------------------
  
  (define board-p
    (xml-parser
     (board () (t tile-p) ... (h hotel-p) ... #:action (list 'board t h))))
  
  (define hotel-p 
    (xml-parser
     (hotel ((name label-p)) (t tile-p) ... #:action (list 'hotel name t))))
  
  (define tile-p 
    (xml-parser
     (tile ((column column-p) (row row-p)) #:action (list 'tile row column))))
  
  (define label-p string->number)
  (define column-p values)
  (define row-p string->number)
  
  ;; -------------------------------------------------------------------------------------------------
  
  (check-equal? (tile-p (tile "1" "a")) '(tile 1 "a"))
  (check-equal? (hotel-p (hotel "55" (tile "1" "a") (tile "2" "a")))
                '(hotel 55 ((tile 1 "a") (tile 2 "a"))))
  
  (check-equal? (label-p "1") 1)
  (check-equal? (board-p `(board ())) '(board () ()))
  (check-equal? (board-p `(board () ,(tile "1" "A") ,(tile "1" "A")))
                '(board ((tile 1 "A") (tile 1 "A")) ()))
  (check-equal? (board-p`(board () ,(hotel "42" (tile "1" "B"))))
                '(board () ((hotel 42 ((tile 1 "B"))))))
  (check-equal? (board-p `(board () ,(tile "1" "A") ,(tile "1" "A") ,(hotel "32" (tile "1" "B"))))
                '(board ((tile 1 "A") (tile 1 "A")) ((hotel 32 ((tile 1 "B")))))))

;; ---------------------------------------------------------------------------------------------------

(define (numbered-attributes format-string x->xexpr attribute-values)
  (for/list ((h attribute-values) (i (in-naturals))) 
    `(,(format-symbol format-string (+ i 1)) ,(x->xexpr h))))

(module+ test
  (check-equal? (numbered-attributes "foo~a" number->string '(1 2)) '((foo1 "1") (foo2 "2"))))
