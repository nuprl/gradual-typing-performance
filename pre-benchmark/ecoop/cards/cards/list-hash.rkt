#lang typed/racket

(provide hash-ref
         hash-set!
         make-hash
         Hash)

(define-type (Hash a b) (Boxof (Listof (Pairof a b))))

(: make-hash (All (a b) (-> (Hash a b))))
(define (make-hash)
  (box empty))


(: hash-ref (All (a b c) 
                 (case-> ((Hash a b) a -> (Option b))
                         ((Hash a b) a (-> c) -> (U b c)))))
(define (hash-ref bhash key [failure-result (thunk #f)])
  (let* ([hash (unbox bhash)]
         [res (assoc key hash)])
    (if res
        (cdr res)
        (failure-result))))

(: hash-set! (All (a b) ((Hash a b) a b -> Void)))
(define (hash-set! bhash key v)
  (set-box! bhash (update-hash-list (unbox bhash) key v)))

(: update-hash-list (All (a b) ((Listof (Pairof a b)) a b -> (Listof (Pairof a b)))))
(define (update-hash-list hash key v)
  (cond
    [(empty? hash) (list (cons key v))]
    [(equal? (first hash) key) (cons (cons key v) (rest hash))]
    [else (cons (first hash) (update-hash-list (rest hash) key v))]))