#lang typed/racket

;; ---------------------------------------------------------------------------------------------------
;; basic game ontology and data structures for simple concepts (hotels, cash, shares)

;; ---------------------------------------------------------------------------------------------------
(provide
 hotel?
 ;; (-> Any Boolean)
 hotel<=?
 ;; (-> Hotel Hotel Boolean)
 
 ALL-HOTELS
 ;; (Listof Hotel), also sorted
 
 SAFE# FINAL#
 ;; Natural

 hotel->label
 ;; (-> Hotel String)

 hotel->color
 ;; (-> Hotel Symbol)

 shares?
 ;; (-> Any Boolean)

 banker-shares0
 ;; Shares

 player-shares0
 ;; Shares

 shares-order?
 ;; 

 SHARES-PER-TURN#
 ;; Natural

 shares++
 ;; (-> Shares Hotel Shares)

 (rename-out [ext:shares-- shares--])
 ;; (-> Shares Hotel Shares)
 ;; Precondition: (shares-available s h) > 0

 shares-available
 ;; (-> Shares Hotel Natural)

 (rename-out [ext:shares-available? shares-available?])
 ;; (-> Shares (Listof Hotel) Boolean)
 ;; Precondition: (shares-order? s*)
 ;; "Can the given order of shares be satisfied in this wallet?

 shares-minus
 ;; (-> Shares Shares Shares)

 shares-plus
 ;; (-> Shares Shares Shares)

 shares-compatible
 ;; (-> Shares (-> Shares Boolean))

 shares-combinable?
 ;; (-> (Listof Shares) Boolean)

 (rename-out [ext:*combine-shares *combine-shares])
 ;; (-> (Listof Shares) Shares)
 ;; Precondition: shares-combinable

 *create-shares
 ;; (-> Hotel Natural Shares)

 shares->string
 ;; (-> Shares String)

 cash?
 ;; (-> Any Boolean)

 CASH0
 ;; Cash

 price-per-share
 ;; (-> Hotel Natural (Option Cash))

 bonus
 ;; (-> M*rity Hotel Natural Cash)
)

;; ---------------------------------------------------------------------------------------------------

(require
 require-typed-check
 "../base/types.rkt"
)
(require/typed/check "auxiliaries.rkt"
 (randomly-pick (-> (Listof Hotel) Hotel))
 )

;; ---------------------------------------------------------------------------------------------------

(: MIN-PLAYER# Natural)
(define MIN-PLAYER# 3)
(: MAX-PLAYER# Natural)
(define MAX-PLAYER# 6)

(: SAFE# Natural)
(define SAFE# 12)
(: FINAL# Natural)
(define FINAL# 40)

(: AMERICAN Hotel)
(define AMERICAN    "American")
(: CONTINENTAL Hotel)
(define CONTINENTAL "Continental")
(: FESTIVAL Hotel)
(define FESTIVAL    "Festival")
(: IMPERIAL Hotel)
(define IMPERIAL    "Imperial")
(: SACKSON Hotel)
(define SACKSON     "Sackson")
(: TOWER Hotel)
(define TOWER       "Tower")
(: WORLDWIDE Hotel)
(define WORLDWIDE   "Worldwide")

(: HOTELS (Listof Hotel))
(define HOTELS
  `(,AMERICAN ,CONTINENTAL ,FESTIVAL ,IMPERIAL ,SACKSON ,TOWER ,WORLDWIDE))
(: HOTEL:C (Listof Color))
(define HOTEL:C
  '(red      blue        green    yellow   purple  brown   orange))

;; Hotel  :: HOTELS 

(: hotel? (-> Any Boolean))
(define (hotel? x)
  (cons? (member x HOTELS)))

(: hotel<=? (-> Hotel Hotel Boolean))
(define hotel<=? string<=?)

(: ALL-HOTELS (Listof Hotel))
(define ALL-HOTELS HOTELS)

(: random-hotel (-> Hotel))
(define random-hotel (lambda () (randomly-pick HOTELS)))

(: hotel->color (-> Hotel Color))
(define (hotel->color h)
  (define r
    (for/fold : (Option Color)
              ([r : (Option Color) #f])
              ((i : Hotel (in-list HOTELS))
               (c : Symbol (in-list HOTEL:C)))
      (or r (and (equal? h i) c))))
  (or r 
      (error 'hotel->color (format "Unbound hotel ~a" h))))

(: string->hotel (-> String (Option Hotel)))
(define (string->hotel n)
  (and n (member n HOTELS) n))

(: hotel->label (-> Hotel String))
(define (hotel->label x)
  x)

;; ---------------------------------------------------------------------------------------------------
;; SHARES = [Hashof Hotel Nat]

(: SHARES0 Share)
(define SHARES0 25)
(: SHARES-PER-TURN# Share)
(define SHARES-PER-TURN# 2)

;;bg; changed from shares-order/c
(: shares-order? (-> Any Boolean))
(define (shares-order? x*)
  (define h* (cast x* (Listof Hotel)))
  (and
   (not (null? h*))
   (let ([h1 : Hotel (car h*)])
     (for/and : Boolean
              ([h2 : Hotel (in-list (cdr h*))])
       (string=? h1 h2)))
   (<= SHARES-PER-TURN# (length h*))))

(: player-shares0 Shares)
(define player-shares0
  (make-hash (for/list : (Listof (Pairof Hotel Share))
                       ([h (in-list ALL-HOTELS)])
               (cons h 0))))

(define banker-shares0
  (make-hash (for/list : (Listof (Pairof Hotel Share))
                       ([h (in-list ALL-HOTELS)])
               (cons h SHARES0))))

(: shares? (-> Any Boolean))
(define (shares? x)
  (and (hash? x)
       (for/and ([(k v) (in-hash x)])
         (and (hotel? k)
              (exact-nonnegative-integer? k)))))

(: shares-minus (-> Shares Shares Shares))
(define (shares-minus s t)
  (for/fold : Shares
            ((s : Shares s))
            ([(hotel n) (in-hash t)])
    (hash-update s hotel (λ ([m : Share]) (max 0 (- m n))))))

(: shares-plus (-> Shares Shares Shares))
(define (shares-plus s t)
  (for/fold : Shares
            ((s : Shares s))
            ([(hotel n) (in-hash t)])
    (hash-update s hotel (λ ([m : Share]) (+ m n)))))

(: ext:shares-- (-> Shares Hotel Shares))
(define (ext:shares-- s h)
  (unless (> (shares-available s h) 0)
    (error 'shares-- (format "Precondition failed: (> (shares-available ~a ~a) 0)" s h)))
  (shares-- s h))

(: shares-- (-> Shares Hotel Shares))
(define (shares-- s h)
  (hash-update s h sub1))

(: shares++ (-> Shares Hotel Shares))
(define (shares++ s h)
  (hash-update s h add1))

(: ext:shares-available (-> Shares Hotel Share))
(define (ext:shares-available s h)
  (unless (shares-order? h)
    (error 'shares-available (format "Precondition: shares-order ~a\n" h)))
  (shares-available s h))

(: shares-available (-> Shares Hotel Share))
(define (shares-available s h)
  (hash-ref s h))

(: ext:shares-available? (-> Shares (Listof Hotel) Boolean))
(define (ext:shares-available? available-s hotels)
  (unless (shares-order? available-s)
    (error 'shares-available "Precondition"))
  (shares-available? available-s hotels))

(: shares-available? (-> Shares (Listof Hotel) Boolean))
(define (shares-available? available-s hotels)
  (hash?
   (for/fold : (Option Shares)
             ((s : (Option Shares) available-s))
             ((h : Hotel (in-list hotels)))
     (and s
          (> (shares-available s h) 0)
          (shares-- s h)))))

(: shares->list (-> Shares [Listof Hotel]))
(define (shares->list s)
  (for/fold : (Listof Hotel)
            ((l : (Listof Hotel) '()))
            ([(hotel count) (in-hash s)])
    (append (make-list count hotel) l)))

(: list->shares (-> [Listof Hotel] Shares))
(define (list->shares hotels)
  (for/fold : Shares
            ((s : Shares player-shares0))
            ((h : Hotel (in-list hotels)))
    (shares++ s h)))

(: shares-compatible (-> Shares (-> Shares Boolean)))
(define ((shares-compatible s) t)
  (for/and ([(hotel count) (in-hash t)])
    (>= (shares-available s hotel) count)))

(: string->count (-> String (Option Share)))
(define (string->count x)
  (define n (string->number x))
  (and n (exact-integer? n) (<= 0 n) (<= n SHARES0) n))

(: shares->string (-> Shares String))
(define (shares->string sh)
  (string-join (for/list : (Listof String)
                         ([(h c) (in-hash sh)])
                 (format "~a : ~a " h c))))

(: *create-shares (-> Hotel Natural Shares))
(define (*create-shares h n)
  (for/fold : Shares
            ((s : Shares player-shares0))
            ((i : Integer (in-range n)))
    (shares++ s h)))

(: shares-combinable? (-> (Listof Shares) Boolean))
(define (shares-combinable? ls)
  (define s (foldr shares-plus player-shares0 ls))
  (for/and ([(key count) (in-hash s)])
    (<= count SHARES0)))

(: *combine-shares (-> (Listof Shares) Shares))
(define (*combine-shares s)
  (foldr shares-plus player-shares0 s))

(: ext:*combine-shares (-> (Listof Shares) Shares))
(define (ext:*combine-shares s)
  (unless (shares-combinable? s)
    (error '*combine-shares (format "Precondition error: shares-combinable ~a" s)))
  (*combine-shares s))

;; ---------------------------------------------------------------------------------------------------
;; CASH

(: CASH0 Cash)
(define CASH0 8000)

(define-predicate cash? Cash)

(: string->cash (-> String (Option Cash)))
(define (string->cash s)
  (define n (string->number s))
  (and n (exact-integer? n) (>= n 0) n))

(: cash->string (-> Cash String))
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

(: bonus (-> M*ority Hotel Natural Cash))
(define (bonus mode hotel tile#)
  (* (or (price-per-share hotel tile#) (error 'bonus))
     (if (eq? mode 'majority) 10 5)))

;; determine the price per share for a hotel of size tile#

(: price-per-share (-> Hotel Natural (Option Cash)))
(define (price-per-share hotel tile#)
  (define table (rest PRICES))
  (define limit-selector
    (or
     (for/or : (Option (-> (Listof Real) Real))
             ([hotels : (Listof Hotel) (in-list (rest (first PRICES)))]
              [selector : (-> (Listof Real) Real) (in-list (list second third fourth))])
       (and (member hotel hotels) selector))
     (error 'price-per-share)))
  (define price*
    (for/fold : (Listof Real)
              ([acc : (Listof Real) '()])
              ([price* : (Listof Real) (in-list table)])
      (cons (first price*) acc)))
  (define limit*
    (for/fold : (Listof Real)
              ([acc : (Listof Real) '()])
              ([price* : (Listof Real) (in-list table)])
      (cons (limit-selector price*) acc)))
  (for/fold : (Option Cash)
            ([acc : (Option Cash) #f])
            ([price : Real (in-list price*)]
             [limit : Real (in-list limit*)])
    (or acc (and (>= tile# limit) (assert price exact-nonnegative-integer?)))))

