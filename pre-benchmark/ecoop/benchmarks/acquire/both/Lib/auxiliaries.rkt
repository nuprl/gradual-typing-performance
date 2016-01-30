#lang racket

(require math)

(provide
 (contract-out
  
  (randomly-pick (-> cons? any))
  
  (distinct
   ;; for small lists O(n^2)
   (-> list? boolean?))
  
  (sorted 
   ;; is the given list sorted according to the given comparison wrt to the key 
   (->i ((cmp (-> any/c any/c any))) (#:key (key (-> any/c any))) (ok? (-> list? any))))
  
  (winners
   ;; find the prefix that has the same value according to the [optional] key selector
   (->i ([l (key) (and/c cons? (sorted >= #:key (if (unsupplied-arg? key) values key)))])
        ([key (-> any/c real?)]
         #:info [info (-> any/c any/c)])
        (list-of-winners cons?)))
  
  (partition 
   ;; create equal-valued partitions (sorted), assuming the list is sorted 
   (->i ([l (key) (and/c cons? 
                         (or/c
                          (sorted <= #:key (if (unsupplied-arg? key) values key))
                          (sorted >= #:key (if (unsupplied-arg? key) values key))))])
        ([key (-> any/c real?)] 
         #:info (info (-> any/c any/c)))
        (partitions (listof cons?))))
  
  (combinations/no-order
   ;; create combinations of size at most k for list l w/o regard to order 
   (->i ([k natural-number/c][l (k) (and/c list? (compose (>=/c k) length))])
        [result (k l) (and/c [listof list?] (compose (=/c (co-no-order# (length l) k)) length))]))))

;; (co-no-order# (-> natural-number/c natural-number/c natural-number/c))
;; number of combinations of size at most k for a collection of n items 
(define (co-no-order# n k)
  (for/sum ((j (range (+ k 1)))) (binomial n j)))

;; ---------------------------------------------------------------------------------------------------

(define (randomly-pick l)
  (list-ref l (random (length l))))

(define ((sorted cmp #:key (key values)) l)
  (or (empty? l)
      (let all ([l (rest l)][pred (key (first l))])
        (cond
          [(empty? l) #t]
          [else
           (define key2 (key (first l)))
           (and (cmp pred key2) (all (rest l) key2))]))))

(define (partition lo-h-size (selector values) #:info (info values))
  (define one (first lo-h-size))
  (let loop [(pred (selector one)) (l (rest lo-h-size)) [partition (list (info one))]]
    (cond
      [(empty? l) (list (reverse partition))]
      [else 
       (define two (first l))
       (if (not (= (selector two) pred))
           (cons (reverse partition) (loop (selector two) (rest l) (list (info two))))
           (loop pred (rest l) (cons (info two) partition)))])))

(define (winners lo-h-size (selector values) #:info (info values))
  (first (partition lo-h-size selector #:info info)))

(define (distinct s)
  (cond
    [(empty? s) #t]
    [else (and (not (member (first s) (rest s))) (distinct (rest s)))]))

(define (combinations/no-order n lox)
  (cond
    [(= n 0) '(())]
    [(empty? lox) '(())]
    [else 
     (define one (first lox))
     (define chs (combinations/no-order (- n 1) (rest lox)))
     (append (map (lambda (n-1) (cons one n-1)) chs) (combinations/no-order n (rest lox)))]))

(module+ test 
  (require rackunit (submod ".."))
  
  ;; testing choice 
  ; (check-equal? (choice 3 '()) '(()))
  (check-equal? (combinations/no-order 1 '(a b c)) '((a) (b) (c) ()))
  (check-equal? (combinations/no-order 2 '(a b c)) '((a b) (a c) (a) (b c) (b) (c) ()))
  (check-equal? (combinations/no-order 3 '(a b c)) '((a b c) (a b) (a c) (a) (b c) (b) (c) ()))
  
  ;; testing random pick 
  (define l (build-list 100 add1))
  (define x (randomly-pick l))
  (check-true (cons? (member x l)))
  
  ;; testing sorted 
  (check-false ((sorted <= #:key second) '((a 6) (b 4) (c 4))))
  (check-true ((sorted >= #:key second) '((a 6) (b 4) (c 4))))
  
  ;; testing distinct 
  (check-true (distinct (build-list 10 (λ (i) (list i i)))))
  (check-true (distinct (build-list 10 number->string)))
  (check-false (distinct (build-list 10 (λ (_) 1))))
  
  ;; testing winners 
  (check-equal? (winners '(3 3 2 1)) '(3 3))
  (check-equal? (winners '((a 3) (b 3) (c 2) (d 1)) second) '((a 3) (b 3)))
  (check-equal? (winners '((a 3) (b 3) (c 2) (d 1)) second #:info first) '(a b))
  
  ;; testing partition 
  (check-equal? (partition '(1 1 2)) '((1 1) (2)))
  (check-equal? (partition '(1 1 2 2 3)) '((1 1) (2 2) (3))))

