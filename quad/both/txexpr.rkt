#lang racket/base

;; bg: 2015-05-18: Let's try just importing, with opaque types
;; I think the answer for this library is to define the type
;; and re-design, but no worries for now.

;; bg: abstract helpers
(provide
 txexpr-car
 txexpr-cdr
 txexpr-cons
 txexpr-map
 ;; -- bg
 (rename-out
  [id x->string]
  [id list->xexpr]))

(define txexpr-cons cons)
(define txexpr-car car)
(define txexpr-map map)
(define txexpr-cdr cdr)

;; =============================================================================

;; bg
(define (id x) x)

(require (for-syntax racket/base))
(require racket/match xml racket/string racket/list racket/bool)

(module+ safe (require racket/contract))

(define-syntax (define+provide+safe stx)
  (syntax-case stx ()
    [(_ (proc arg ... . rest-arg) contract body ...)
     #'(define+provide+safe proc contract
         (λ(arg ... . rest-arg) body ...))]
    [(_ name contract body ...)
     #'(begin
         (define name body ...)
         (provide name)
         (module+ safe
           (provide (contract-out [name contract]))))]))

(define+provide+safe (txexpr-tag? x)
  (any/c . -> . boolean?)
  (symbol? x))

(define+provide+safe (txexpr-tags? x)
  (any/c . -> . boolean?)
  (and (list? x) (andmap txexpr-tag? x)))


(define+provide+safe (txexpr-attr? x)
  (any/c . -> . boolean?)
  (match x
    [(list (? symbol?) (? string?)) #t]
    [else #f]))


(define (validate-txexpr-attrs x #:context [txexpr-context #f])
  ; ((any/c) (#:context (or/c #f txexpr?)) . ->* . txexpr-attrs?)
  (define (make-reason)
    (if (not (list? x))
        (format "because ~v is not a list" x)
        (let ([bad-attrs (filter (λ(i) (not (txexpr-attr? i))) x)])
          (format "because ~a ~a" (string-join (map (λ(ba) (format "~v" ba)) bad-attrs) " and ") (if (> (length bad-attrs) 1)
                                                                                                     "are not valid attributes"
                                                                                                     "is not in the form '(symbol \"string\")")))))
  (match x
    [(list (? txexpr-attr?) ...) x]
    [else [else (error (string-append "validate-txexpr-attrs: "
                                      (if txexpr-context (format "in ~v, " txexpr-context) "")
                                      (format "~v is not a valid list of attributes ~a" x (make-reason))))]]))

(define+provide+safe (txexpr-attrs? x)
  (any/c . -> . boolean?)
  (with-handlers ([exn:fail? (λ(exn) #f)])
    (and (validate-txexpr-attrs x) #t)))


(define+provide+safe (txexpr-elements? x)
  (any/c . -> . boolean?)
  (match x
    [(list elem ...) (andmap txexpr-element? elem)]
    [else #f]))


(define (validate-txexpr-element x #:context [txexpr-context #f])
  ; ((any/c) (#:context (or/c #f txexpr?)) . ->* . txexpr-element?)
  (cond
    [(or (string? x) (txexpr? x) (symbol? x)
         (valid-char? x) (cdata? x)) x]
    [else (error (string-append "validate-txexpr-element: "
                                (if txexpr-context (format "in ~v, " txexpr-context) "")
                                (format "~v is not a valid element (must be txexpr, string, symbol, XML char, or cdata)" x)))]))


(define+provide+safe (txexpr-element? x)
  (any/c . -> . boolean?)
  (with-handlers ([exn:fail? (λ(exn) #f)])
    (and (validate-txexpr-element x) #t)))


;; is it a named x-expression?
;; todo: rewrite this recurively so errors can be pinpointed (for debugging)
(define+provide+safe (validate-txexpr x)
  (any/c . -> . txexpr?)
  (define (validate-txexpr-element-with-context e) (validate-txexpr-element e #:context x))
  (define (validate-txexpr-attrs-with-context e) (validate-txexpr-attrs e #:context x))

  (when (match x
          [(list (? symbol?)) #t]
          [(list (? symbol? name) (and attr-list (list (list k v ...) ...)) rest ...)
           (and (validate-txexpr-attrs-with-context attr-list)
                (andmap validate-txexpr-element-with-context rest))]
          [(list (? symbol? name) rest ...)(andmap validate-txexpr-element-with-context rest)]
          [else (error (format "validate-txexpr: ~v is not a list starting with a symbol" x))])
    x))

(define+provide+safe (txexpr? x)
  (any/c . -> . boolean?)
  (with-handlers ([exn:fail? (λ(exn) #f)])
    (and (validate-txexpr x) #t)))



(define+provide+safe (make-txexpr tag [attrs null] [elements null])
  ;; todo?: use xexpr/c provides a nicer error message
  ((symbol?) (txexpr-attrs? txexpr-elements?)
             . ->* . txexpr?)
  (filter (compose1 not null?) `(,tag ,attrs ,@elements)))


(define+provide+safe (txexpr->values x)
  (txexpr? . -> . (values symbol? txexpr-attrs? txexpr-elements?))
  (match
      ; txexpr may or may not have attr
      ; if not, add null attr so that decomposition only handles one case
      (match x
        [(list _ (? txexpr-attrs?) _ ...) x]
        [else `(,(car x) ,null ,@(cdr x))])
    [(list tag attr content ...) (values tag attr content)]))


(define+provide+safe (txexpr->list x)
  (txexpr? . -> . list?)
  (define-values (tag attrs content) (txexpr->values x))
  (list tag attrs content))


;; convenience functions to retrieve only one part of txexpr
(define+provide+safe (get-tag x)
  (txexpr? . -> . txexpr-tag?)
  (car x))


(define+provide+safe (get-attrs x)
  (txexpr? . -> . txexpr-attrs?)
  (define-values (tag attrs content) (txexpr->values x))
  attrs)


(define+provide+safe (get-elements x)
  (txexpr? . -> . txexpr-elements?)
  (define-values (tag attrs elements) (txexpr->values x))
  elements)


;; helpers. we are getting a string or symbol
(define+provide+safe (->txexpr-attr-key x)
  (can-be-txexpr-attr-key? . -> . txexpr-attr-key?)
  (if (string? x) (string->symbol x) x))

(define+provide+safe (->txexpr-attr-value x)
  (can-be-txexpr-attr-value? . -> . txexpr-attr-value?)
  (->string x))

(define+provide+safe (txexpr-attr-key? x)
  (any/c . -> . boolean?)
  (symbol? x))

(define+provide+safe (can-be-txexpr-attr-key? x)
  (any/c . -> . boolean?)
  (or (symbol? x) (string? x)))

(define+provide+safe (txexpr-attr-value? x)
  (any/c . -> . boolean?)
  (string? x))

(define (txexpr-attr-values? xs) (and (list? xs) (andmap txexpr-attr-value? xs)))

(define+provide+safe (can-be-txexpr-attr-value? x)
  (any/c . -> . boolean?)
  (can-be-txexpr-attr-key? x))

(define (->string x)
  (if (symbol? x) (symbol->string x) x))

(define+provide+safe (can-be-txexpr-attrs? x)
  (any/c . -> . boolean?)
  (ormap (λ(test) (test x)) (list txexpr-attr? txexpr-attrs? can-be-txexpr-attr-key? can-be-txexpr-attr-value?)))

(define (list-of-can-be-txexpr-attrs? xs) (and (list? xs) (andmap can-be-txexpr-attrs? xs)))

(define+provide+safe (attrs->hash . items)
  (() #:rest list-of-can-be-txexpr-attrs? . ->* . hash?)
  ;; can be liberal with input because they're all just nested key/value pairs
  ;; but still need this function to make sure that 'foo and "foo" are treated as the same hash key
  (define (make-key-value-list items)
    (if (null? items)
        null
        (let ([key (->txexpr-attr-key (car items))]
              [value (->txexpr-attr-value (cadr items))]
              [rest (cddr items)])
          (cons (cons key value) (make-key-value-list rest)))))
  (make-immutable-hash (make-key-value-list (flatten items))))

(define+provide+safe (hash->attrs hash)
  (hash? . -> . txexpr-attrs?)
  (hash-map hash list))

(define+provide+safe (attrs-have-key? x key)
  ((or/c txexpr-attrs? txexpr?) can-be-txexpr-attr-key? . -> . boolean?)
  (define attrs (if (txexpr-attrs? x) x (get-attrs x)))
  (hash-has-key? (attrs->hash attrs) (->txexpr-attr-key key)))

(define+provide+safe (attrs-equal? x1 x2)
  ((or/c txexpr-attrs? txexpr?) (or/c txexpr-attrs? txexpr?) . -> . boolean?)
  (define attrs-tx1 (attrs->hash (if (txexpr-attrs? x1) x1 (get-attrs x1))))
  (define attrs-tx2 (attrs->hash (if (txexpr-attrs? x2) x2 (get-attrs x2))))
  (and
   (= (length (hash-keys attrs-tx1)) (length (hash-keys attrs-tx2)))
   (for/and ([(key value) (in-hash attrs-tx1)])
     (equal? (hash-ref attrs-tx2 key) value))))

(define+provide+safe (attr-set tx key value)
  (txexpr? can-be-txexpr-attr-key? can-be-txexpr-attr-value? . -> . txexpr?)
  (define new-attrs
    (hash->attrs (hash-set (attrs->hash (get-attrs tx)) (->txexpr-attr-key key) (->txexpr-attr-value value))))
  (make-txexpr (get-tag tx) new-attrs (get-elements tx)))


(define+provide+safe (attr-ref tx key)
  (txexpr? can-be-txexpr-attr-key? . -> . txexpr-attr-value?)
  (with-handlers ([exn:fail? (λ(e) (error (format "attr-ref: no value found for key ~v" key)))])
    (hash-ref (attrs->hash (get-attrs tx)) key)))

(define+provide+safe (attr-ref* tx key)
  (txexpr? can-be-txexpr-attr-key? . -> . txexpr-attr-values?)
  (filter-not false?
              (flatten
               (let loop ([tx tx])
                 (and (txexpr? tx)
                      (cons (and (attrs-have-key? tx key)(attr-ref tx key))
                            (map loop (get-elements tx))))))))

;; convert list of alternating keys & values to attr
(define+provide+safe (merge-attrs . items)
  (() #:rest list-of-can-be-txexpr-attrs? . ->* . txexpr-attrs?)
  (define attrs-hash (apply attrs->hash items))
  ;; sort needed for predictable results for unit tests
  (define sorted-hash-keys (sort (hash-keys attrs-hash) (λ(a b) (string<? (->string a) (->string b)))))
  `(,@(map (λ(key) (list key (hash-ref attrs-hash key))) sorted-hash-keys)))


(define+provide+safe (remove-attrs x)
  (txexpr? . -> . txexpr?)
  (cond
    [(txexpr? x) (let-values ([(tag attr elements) (txexpr->values x)])
                   (make-txexpr tag null (remove-attrs elements)))]
    [(txexpr-elements? x) (map remove-attrs x)]
    [else x]))

;; todo: exclude-proc will keep things out, but is there a way to keep things in?
(define+provide+safe (map-elements/exclude proc x exclude-test)
  (procedure? txexpr? procedure? . -> . txexpr?)
  (cond
    [(txexpr? x)
     (if (exclude-test x)
         x
         (let-values ([(tag attr elements) (txexpr->values x)])
           (make-txexpr tag attr
                        (map (λ(x)(map-elements/exclude proc x exclude-test)) elements))))]
    ;; externally the function only accepts txexpr,
    ;; but internally we don't care
    [else (proc x)]))

(define+provide+safe (map-elements proc x)
  (procedure? txexpr? . -> . txexpr?)
  (map-elements/exclude proc x (λ(x) #f)))

;; function to split tag out of txexpr
(define+provide+safe (splitf-txexpr tx pred [proc (λ(x) null)])
  ((txexpr? procedure?) (procedure?) . ->* . (values txexpr? txexpr-elements?))
  (define matches null)
  (define (do-extraction x)
    (cond
      [(pred x) (begin  ; store matched item and return processed value
                  (set! matches (cons x matches))
                  (proc x))]
      [(txexpr? x) (let-values([(tag attr body) (txexpr->values x)])
                     (make-txexpr tag attr (do-extraction body)))]
      [(txexpr-elements? x) (filter (compose1 not null?) (map do-extraction x))]
      [else x]))
  (define tx-extracted (do-extraction tx)) ;; do this first to fill matches
  (values tx-extracted (reverse matches)))

(define+provide+safe (xexpr->html x)
  (xexpr? . -> . string?)
  (define (->cdata x)
    (if (cdata? x) x (cdata #f #f x)))

  (xexpr->string (let loop ([x x])
                   (cond
                     [(txexpr? x) (if (member (get-tag x) '(script style))
                                      (make-txexpr (get-tag x) (get-attrs x) (map ->cdata (get-elements x)))
                                      (make-txexpr (get-tag x) (get-attrs x) (map loop (get-elements x))))]
                     [else x]))))
