#lang racket

;; ---------------------------------------------------------------------------------------------------
;; basic game ontology and data structures for simple concepts (hotels, cash, shares)

;; ---------------------------------------------------------------------------------------------------
(require "basics-intf.rkt")

(basics&
 MIN-PLAYER#
 MAX-PLAYER#

 hotel? hotel<=? random-hotel 
 AMERICAN CONTINENTAL FESTIVAL IMPERIAL SACKSON TOWER WORLDWIDE ALL-HOTELS SAFE# FINAL# 
 string->hotel hotel->label hotel->color string->count xhotel? hotel->xexpr 
 
 shares? banker-shares0 player-shares0 shares-order/c SHARES-PER-TURN#
 shares++ shares-- shares-available shares-available? shares-minus shares-plus shares-compatible shares->list
 shares-combinable? *combine-shares *create-shares xshare? xshare<=? shares->xexpr xorder? shares->string
 order->xexpr
 
 cash? CASH0 string->cash cash->string
 price-per-share 
 bonus)
(provide shares-order?)
;; ---------------------------------------------------------------------------------------------------
(require "Lib/xml.rkt" "Lib/auxiliaries.rkt" (only-in srfi/1 list-index))

(module+ test (require rackunit))

;; ---------------------------------------------------------------------------------------------------

(define MIN-PLAYER# 3)
(define MAX-PLAYER# 6)

(define SAFE# 12)
(define FINAL# 40)

(define AMERICAN    "American")
(define CONTINENTAL "Continental")
(define FESTIVAL    "Festival")
(define IMPERIAL    "Imperial")
(define SACKSON     "Sackson")
(define TOWER       "Tower")
(define WORLDWIDE   "Worldwide")

(define HOTELS
  `(,AMERICAN ,CONTINENTAL ,FESTIVAL ,IMPERIAL ,SACKSON ,TOWER ,WORLDWIDE))
(define HOTEL:C
  '(red      blue        green    yellow   purple  brown   orange))

;; Hotel  :: HOTELS 

(define (hotel? x)
  (cons? (member x HOTELS)))

(define hotel<=? string<=?)

(define ALL-HOTELS HOTELS)

(define random-hotel (lambda () (randomly-pick HOTELS)))

;; Hotel -> Color 
(define (hotel->color h)
  (for/first ((i HOTELS) (c HOTEL:C) #:when (equal? h i)) c))

(define (string->hotel n)
  (and n (member n HOTELS) n))

(define hotel->label values)

(define xhotel? 
  (xml-predicate (hotel ((label string->hotel)))))

(define (hotel->xexpr h)
  `(hotel ((label ,(hotel->label h)))))

;; ---------------------------------------------------------------------------------------------------
;; SHARES = [Hashof Hotel Nat]

(define SHARES0 25)
(define SHARES-PER-TURN# 2)

;; should come from Lib/auxiliaries eventually
(define (all-equal? l) (or (empty? l) (andmap (curry equal? (first l)) (rest l))))
(define shares-order/c (and/c (listof hotel?) all-equal? (compose (<=/c SHARES-PER-TURN#) length)))
(define shares-order? (flat-contract-predicate shares-order/c))

(define player-shares0 (for/fold ((s (hash))) ((h ALL-HOTELS)) (hash-set s h 0)))

(define banker-shares0 (for/fold ((s (hash))) ((h ALL-HOTELS)) (hash-set s h SHARES0)))

(define (shares? s) 
  (and (hash? s) (andmap (lambda (x) (member x HOTELS)) (hash-keys s)) #t))

(define (shares-minus s t)
  (for/fold ((s s)) ([(hotel n) t]) (hash-update s hotel (λ (m) (max 0 (- m n))))))

(define (shares-plus s t)
  (for/fold ((s s)) ([(hotel n) t]) (hash-update s hotel (λ (m) (+ m n)))))

(define (shares++ s h)
  (hash-update s h add1))

(define (shares-- s h)
  (hash-update s h sub1))

(define (shares-available s h)
  (hash-ref s h))

(define (shares-available? available-s hotels)
  (hash? 
   (for/fold ((s available-s)) ((h hotels))
     (and s (if (> (shares-available s h) 0) (shares-- s h) #f)))))

;; Shares -> [Listof Hotel]
(define (shares->list s)
  (for/fold ((l '())) ([(hotel count) s])
    (append (make-list count hotel) l)))

;; [Listof Hotel] -> Shares 
(define (list->shares hotels)
  (for/fold ((s player-shares0)) ((h hotels)) (shares++ s h)))

(define ((shares-compatible s) t)
  (for/and ([(hotel count) t]) (>= (shares-available s hotel) count)))

(define (string->count x)
  (define n (string->number x))
  (and n (exact-integer? n) (<= 0 n) (<= n SHARES0) n))

(define (shares->string sh)
  (string-join (for/list ([(h c) sh]) (format "~a : ~a " h c))))

(define xshare?
  (xml-predicate (share ((name string->hotel) (count string->count)))))

(define (shares->xexpr s)
  (define x 
    (sort (for/list ([(n c) s] #:when (> c 0)) `(,(hotel->label n) ,c))  string<=? #:key first))
  (for/list ((share x))
    (define-values (name count) (apply values share))
    `(share ((name ,name) (count ,(number->string count))))))

(define (xshare<=? s t)
  (define hs ((xml-parser (share ((name values) (count values)) #:action name)) s))
  (define ht ((xml-parser (share ((name values) (count values)) #:action name)) t))
  (hotel<=? hs ht))

(define xorder?
  (xml-predicate (order () xhotel? ...)))

(define (order->xexpr o)
  `(order () ,@(map hotel->xexpr o)))

(define (*create-shares h n)
  (for/fold ((s player-shares0)) ((i n)) (shares++ s h)))

(define (shares-combinable? ls)
  (for/and ([(key count) (foldr shares-plus player-shares0 ls)]) (<= count SHARES0)))

(define (*combine-shares s)
  (foldr shares-plus player-shares0 s))

;; ---------------------------------------------------------------------------------------------------
;; CASH

(define CASH0 8000)

(define cash? natural-number/c)

(define (string->cash s)
  (define n (string->number s))
  (and n (exact-integer? n) (>= n 0) n))

(define (cash->string c)
  (number->string c))

;; ---------------------------------------------------------------------------------------------------
;; the cost table for hotels, for buying shares and merging hotels 

(define PRICES
  `((Price (,WORLDWIDE ,SACKSON) (,FESTIVAL ,IMPERIAL ,AMERICAN) (,CONTINENTAL ,TOWER))
    (200            2                     0                        0)
    (300            3                     2                        0)
    (400            4                     3                        2)
    (500            5                     4                        3)
    (600            6                     5                        4)
    (700           11                     6                        5)
    (800           21                    11                        6)
    (900           31                    21                       11)
    (1000          41                    31                       21)
    (1100        +inf.0                  41                       31)
    (1200        +inf.0                 +inf.0                    41)))

(unless (set=? (apply set (apply append (rest (first PRICES)))) (apply set HOTELS))
  (define hotels:set (apply set HOTELS))
  (define hotels-in-prices (apply set (apply append (rest (first PRICES)))))
  (error 'PRICES "~a" (set-symmetric-difference hotels:set hotels-in-prices)))

;; determine the majority and minority bonus for a hotel of size tile#

(define (bonus mode hotel tile#)
  (* (price-per-share hotel tile#) (if (eq? mode 'majority) 10 5)))

;; determine the price per share for a hotel of size tile#

(define (price-per-share hotel tile#)
  (define table (rest PRICES))
  (define limit-selector
    (ormap (λ (hotels selector) (and (member hotel hotels) selector)) 
           (rest (first PRICES))
           (list second third fourth)))
  (define price* (reverse (map first table)))
  (define limit* (reverse (map limit-selector table)))
  (ormap (λ (price limit) (and (>= tile# limit) price)) price* limit*))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  
  (check-true (xorder? (order->xexpr '())))
  (check-true (xorder? (order->xexpr (list AMERICAN))))
  (check-true (xorder? (order->xexpr (list AMERICAN TOWER))))
  (check-true (xorder? (order->xexpr (list AMERICAN TOWER FESTIVAL))))

  (check-true ((shares-compatible banker-shares0) player-shares0))
  (check-false ((shares-compatible player-shares0) banker-shares0))
  
  (check-equal? (length (shares->list banker-shares0)) (* SHARES0 (length ALL-HOTELS)))
  (check-equal? (length (shares->list player-shares0)) 0)
  
  (check-true
   (xshare<=? 
    (first (shares->xexpr (*create-shares AMERICAN 3)))
    (first (shares->xexpr (*create-shares TOWER 4)))))
  (check-false
   (xshare<=? 
    (first (shares->xexpr (*create-shares TOWER 4)))
    (first (shares->xexpr (*create-shares AMERICAN 3)))))
  
  (define h0 (random-hotel))
  (check-equal? (hash-ref (shares++ banker-shares0 h0) h0) (+ SHARES0 1))
  (check-equal? (hash-ref (shares-- banker-shares0 h0) h0) (- SHARES0 1))
  
  (check-equal? (shares-minus banker-shares0 player-shares0) banker-shares0)
  (check-equal? (shares-minus banker-shares0 banker-shares0) player-shares0)
  
  (check-true (andmap xshare? (shares->xexpr player-shares0)))
  (check-true (andmap xshare? (shares->xexpr banker-shares0)))
  
  (check-equal? (bonus 'majority AMERICAN 35) 10000)
  (check-equal? (bonus 'minority CONTINENTAL 12) 4500)
  (check-equal? (bonus 'majority SACKSON 4) 4000)

  (check-equal? (price-per-share AMERICAN 35) 1000)
  (check-equal? (price-per-share CONTINENTAL 12) 900)
  (check-equal? (price-per-share SACKSON 4) 400)
  
  )
