#lang typed/racket/base

(provide
  randomly-pick
  ;; (-> cons? any)

  distinct
  ;; (-> list? boolean?)
  ;; for small lists O(n^2)

  (rename-out [ext:aux:partition aux:partition])
  ;; (->* [(Listof Any) (-> Any Real)] [#:info (-> Any Any)] (Listof (Listof Any)))
  ;; Precondition: input list is sorted vis <= or >=
  )

;; -----------------------------------------------------------------------------

(require
  (only-in racket/list
    first second
    rest range cons?)
)

;; ---------------------------------------------------------------------------------------------------

(: randomly-pick (All (A) (-> (Listof A) A)))
(define (randomly-pick l)
  (list-ref l (random (length l))))

(: ext:aux:partition (All (A B) (-> (Listof A) (-> A Real) (-> A B) (Listof (Listof B)))))
(define (ext:aux:partition lo-h-size selector info)
  (define s* (map selector lo-h-size))
  (define s1 (sort s* <=))
  (define s2 (sort s* <=))
  (unless (or (equal? s* s1) (equal? s* s2))
    (error 'aux:partition "Precondition: expected a sorted list"))
  ((inst aux:partition A B) lo-h-size selector info))

(: aux:partition (All (A B) (-> (Listof A) (-> A Real) (-> A B) (Listof (Listof B)))))
(define (aux:partition lo-h-size selector info)
  (define one (first lo-h-size))
  (let loop : (Listof (Listof B))
       [(pred : Real (selector one))
        (l    : (Listof A) (rest lo-h-size))
        [pt   : (Listof B) (list (info one))]]
    (cond
      [(null? l) (list (reverse pt))]
      [else 
       (define two (first l))
       (if (not (= (selector two) pred))
           (cons (reverse pt) (loop (selector two) (rest l) (list (info two))))
           (loop pred (rest l) (cons (info two) pt)))])))

(: distinct (-> (Listof Any) Boolean))
(define (distinct s)
  (cond
    [(null? s) #t]
    [else (and (not (member (first s) (rest s))) (distinct (rest s)))]))
