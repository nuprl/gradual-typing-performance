#lang racket

;; ---------------------------------------------------------------------------------------------------
;; implementing Acquire game strategies 

(require "strategy-intf.rkt")

(strategy& ordered-s random-s largest-alpha smallest-anti)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(require "basics.rkt" "board.rkt" "Lib/auxiliaries.rkt")

(module+ test (require rackunit (submod "board.rkt" tiles+spots)))

;; (cons Hotel [Listof Hotel]) -> Hotel 
(define (alphabetically-first w)
  (first (sort w hotel<=?)))

;; Board Tile [or/c FOUNDING GROWING MERGING SINGLETON] [Listof Hotel] 
;; [ [Listof Hotel] [Listof Hotel] -> Hotel ] -> [Option Hotel]
(define (pick-hotel b to-place kind available-hotels select-founding-hotel select-merger)
  (cond
    [(eq? FOUNDING kind)
     (and (cons? available-hotels) (select-founding-hotel available-hotels))]
    [(eq? MERGING kind)
     (define-values (w _) (merging-which b to-place))
     (select-merger w)]
    [else #f]))

;; Board Cash Shares N [ [Listof Hotel] -> [Listof Hotel] ] -> [Listof Hotel]
;; buy N hotels in alphabetical order that fit into budget
(define (to-buy b my-cash available-shares BUY-N ordering)
  (let loop ((hotels (ordering ALL-HOTELS)) (n 0) (to-buy '()) (cash my-cash) (as available-shares))
    (cond 
      [(or (empty? hotels) (= BUY-N n)) (reverse to-buy)]
      [else 
       (define h (first hotels))
       (define available-h (shares-available as h))
       (cond
         [(= 0 available-h) (loop (rest hotels) n to-buy cash as)]
         [else (define price (price-per-share h (size-of-hotel b h)))
               (if (and price (<= price cash))
                   (if (and (<= (* 2 price) cash) (> available-h 1))
                       (list h h)
                       (list h))
                   (loop (rest hotels) n to-buy cash as))])])))

;; SYNTAX: 
;; (strategy for-loop [ [Listof Hotel] -> Hotel ] [ [Listof Hotel] -> Hotel ] [Nat -> Nat] 
;; -> 
;; strategy/c
;; where (for-loop iteration-clause body ...) computes an optional tile placement/action pair
;;       choose-founding picks a hotel to be founded 
;;       choose-merger   picks the hotel from the winners that is to be the acuirere 
;;       choose-shares#  determines how many shares are to be bought 
;;       hotel-ordering  orders the hotels, which determines in which order shares are bought
(define-syntax strategy
  (syntax-rules ()
    [(strategy for choose-founding choose-merger)
     (strategy for choose-founding choose-merger values values)]
    [(strategy for choose-founding choose-merger choose-shares# hotel-ordering)
     (lambda (turn)
       (define b (get-field board turn))
       (define my-tiles (get-field tiles turn))
       (define my-cash (get-field cash turn))
       (define available-shares (get-field shares turn))
       (define available-hotels (get-field hotels turn))
       (define tile-kind
         (for ((t (sort my-tiles tile<=?))
               #:when 
               (let ([s (what-kind-of-spot b t)])
                 (and (not (eq? s IMPOSSIBLE)) s)))
           (list t (what-kind-of-spot b t))))
       (cond 
         [tile-kind
          (define-values (to-place kind) (apply values tile-kind))
          (define hotel  (pick-hotel b to-place kind available-hotels choose-founding choose-merger))
          (define board  (set-board b to-place kind hotel))
          (define shares 
            (if (and (eq? FOUNDING kind) hotel (> (shares-available available-shares hotel) 0))
                (shares-- available-shares hotel)
                available-shares))
          (when (eq? MERGING kind)
            ;; ignore the decisions of the other players:
            ;; all it means is that the player may buy fewer shares than possible
            (send turn place to-place hotel))
          (values to-place hotel (to-buy board my-cash shares SHARES-PER-TURN# hotel-ordering))]
         [else
          (define buy 
            (to-buy b my-cash available-shares (choose-shares# SHARES-PER-TURN#) hotel-ordering))
          (values #f #f buy)]))]))

[define (strategy/d choose-founding choose-merger choose-shares# hotel-ordering)
  (lambda (turn)
    (define b (get-field board turn))
    (define my-cash (get-field cash turn))
    (define available-shares (get-field shares turn))
    (define available-hotels (get-field hotels turn))
    (define tile-kind
      (for ((t (sort (get-field tiles turn) tile<=?))
            #:when 
            (let ([s (what-kind-of-spot b t)])
              (and (not (eq? s IMPOSSIBLE)) s)))
        (list t (what-kind-of-spot b t))))
    (cond 
      [tile-kind
       (define-values (to-place kind) (apply values tile-kind))
       (define hotel  (pick-hotel b to-place kind available-hotels choose-founding choose-merger))
       (define board  (set-board b to-place kind hotel))
       (define shares 
         (if (and (eq? FOUNDING kind) hotel (> (shares-available available-shares hotel) 0))
             (shares-- available-shares hotel)
             available-shares))
       (when (eq? MERGING kind)
         ;; ignore the decisions of the other players:
         (send turn place to-place hotel))
       (values to-place hotel (to-buy board my-cash shares SHARES-PER-TURN# hotel-ordering))]
      [else
       (define buy 
         (to-buy b my-cash available-shares (choose-shares# SHARES-PER-TURN#) hotel-ordering))
       (values #f #f buy)]))]

(define-syntax-rule 
  (for/random . b)
  (let ([x (for/list . b)]) (if (empty? x) #f (randomly-pick x))))

(define-syntax-rule 
  (for/ror ((t tiles) . clauses) body)
  (for/or ((t (reverse tiles)) . clauses) body))

(define (random+1 n) (random (+ n 1)))

(define ordered-s     (strategy for/or     first         alphabetically-first))
(define random-s      (strategy for/random randomly-pick randomly-pick        random+1 values))
(define largest-alpha (strategy for/ror    first         alphabetically-first))
(define smallest-anti (strategy for/or     first         alphabetically-first values   reverse))

;; ---------------------------------------------------------------------------------------------------
;; testing 

(module+ test 
  
  ;; Strategy Board Cash [Listof Tile] Shares [Listof Hotel] [Maybe Tile] [Maybe Hotel] Shares -> Any 
  (define (test S b cash tiles available-sh available-htls e-placement e-hotel e-shares)
    (call-with-values 
     (lambda ()
       (S (new sturn% [board b][tiles tiles][cash cash][shares available-sh][hotels available-htls])))
     (lambda (placement hotel shares-to-buy)
       ;; santity check for placement and hotel 
       (define kind (if placement (what-kind-of-spot b placement) IMPOSSIBLE))
       (check-true (if (and hotel placement) (cons? (member kind (list FOUNDING MERGING))) #t))
       (check-equal? hotel e-hotel)
       (check-equal? placement e-placement)
       (check-equal? shares-to-buy e-shares))))
  
  (define sturn%
    (class object% 
      (init-field board tiles cash shares hotels)
      (super-new)
      (define/public (place tile hotel)
        '())))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; placing a starter tile, buying hotels with limited shares and cash 
  
  (define b0 (board))
  
  (define t0 (list A2 A3 A4 A5 A6 A7))
  
  (define tower-shares (*create-shares TOWER 3))
  
  (define s0 
    (*combine-shares 
     (list 
      ;; cannot buy Worldwide or Sackson because there are no tiles on the board
      (*create-shares FESTIVAL 1) tower-shares)))
  
  (define h0 ALL-HOTELS)
  
  (test ordered-s     b0 CASH0 t0 banker-shares0 h0 A2 #f `(,AMERICAN ,AMERICAN))
  (test largest-alpha b0 CASH0 t0 banker-shares0 h0 A7 #f `(,AMERICAN ,AMERICAN))
  (test smallest-anti b0 CASH0 t0 banker-shares0 h0 A2 #f `(,TOWER ,TOWER))
  
  (test ordered-s     b0 CASH0 t0 tower-shares h0 A2 #f `(,TOWER ,TOWER))
  (test largest-alpha b0 CASH0 t0 tower-shares h0 A7 #f `(,TOWER ,TOWER))
  (test smallest-anti b0 CASH0 t0 tower-shares h0 A2 #f `(,TOWER ,TOWER))
  
  (test ordered-s     b0 CASH0 t0 s0 h0 A2 #f `(,FESTIVAL))
  (test largest-alpha b0 CASH0 t0 s0 h0 A7 #f `(,FESTIVAL))
  (test smallest-anti b0 CASH0 t0 s0 h0 A2 #f `(,TOWER ,TOWER))
  
  (test ordered-s     b0 10 t0 s0 h0 A2 #f '())
  (test largest-alpha b0 10 t0 s0 h0 A7 #f '())
  (test smallest-anti b0 10 t0 s0 h0 A2 #f '())
  
  ;; -------------------------------------------------------------------------------------------------
  ;; founding a hotel 
  
  (define b1 (*create-board-with-hotels (list A1) '()))
  
  (test ordered-s     b1 CASH0 t0 banker-shares0 h0 A2 AMERICAN `(,AMERICAN ,AMERICAN))
  (test largest-alpha b1 CASH0 t0 banker-shares0 h0 A7 #f       `(,AMERICAN ,AMERICAN))
  (test smallest-anti b1 CASH0 t0 banker-shares0 h0 A2 AMERICAN `(,TOWER ,TOWER))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; limit buying to hotels that have price 
  
  (define Sackson-tiles
    (cons SACKSON
          (append 
           (build-list 12 (lambda (i) (ctile F (+ i 1))))
           (build-list 12 (lambda (i) (ctile G (+ i 1))))
           (build-list 12 (lambda (i) (ctile H (+ i 1)))))))
  
  (define b2 (*create-board-with-hotels (list I12) (list Sackson-tiles)))
  (define price-of-Sackson (price-per-share SACKSON (length Sackson-tiles)))
  (define price-of-Worldwide (price-per-share WORLDWIDE 0))
  ;; cannot buy Worldwide because there are no tiles on the board
  (define s2 (*combine-shares (list (*create-shares SACKSON 2) (*create-shares WORLDWIDE 10))))
  (define h2 (remove SACKSON ALL-HOTELS))
  
  (test ordered-s     b2 CASH0 t0 s2 h2 A2 #f `(,SACKSON ,SACKSON))
  (test largest-alpha b2 CASH0 t0 s2 h2 A7 #f `(,SACKSON ,SACKSON))
  (test smallest-anti b2 CASH0 t0 s2 h2 A2 #f `(,SACKSON ,SACKSON))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; no placement possible 
  
  (define Worldwide-tiles (cons WORLDWIDE (build-list 6 (lambda (i) (ctile D (+ i 1))))))
  (define b3 (*create-board-with-hotels '() (list Worldwide-tiles Sackson-tiles)))
  (define h3 (remove WORLDWIDE h2))
  (define t3 (list E1 E2 E3 E4 E5 E6))
  
  (test ordered-s     b3 CASH0 t3 s2 h3 #f #f `(,SACKSON ,SACKSON))
  (test largest-alpha b3 CASH0 t3 s2 h3 #f #f `(,SACKSON ,SACKSON))
  (test smallest-anti b3 CASH0 t3 s2 h3 #f #f `(,WORLDWIDE ,WORLDWIDE))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; merging two hotels 
  
  (define Tower-tiles (cons TOWER (list F1 F2)))
  (define b4 (*create-board-with-hotels '() (list Worldwide-tiles Tower-tiles)))
  (define h4 (remove* `(,WORLDWIDE ,TOWER) ALL-HOTELS))
  
  (test ordered-s     b4 CASH0 t3 s2 h4 E1 WORLDWIDE `(,WORLDWIDE ,WORLDWIDE))
  (test largest-alpha b4 CASH0 t3 s2 h4 E6 #f        `(,WORLDWIDE ,WORLDWIDE))
  (test smallest-anti b4 CASH0 t3 s2 h4 E1 WORLDWIDE `(,WORLDWIDE ,WORLDWIDE))
  
  ;; merging three hotels 
  
  (define American-tiles (cons AMERICAN (list E3 E4)))
  (define Continental-tiles (cons CONTINENTAL (list D1 D2)))
  (define b5 (*create-board-with-hotels '() (list American-tiles Continental-tiles Tower-tiles)))
  (define h5 (remove* `(,AMERICAN ,CONTINENTAL ,TOWER) ALL-HOTELS))
  
  (test ordered-s     b5 CASH0 (list E2) s2 h5 E2 AMERICAN '())
  (test largest-alpha b5 CASH0 (list E2) s2 h5 E2 AMERICAN '())
  (test smallest-anti b5 CASH0 (list E2) s2 h5 E2 AMERICAN '())
  
  ;; -------------------------------------------------------------------------------------------------
  ;; debugging player during integration 
  
  (define b7 (*create-board-with-hotels '() (list American-tiles)))
  (define t7 (cons B7 (rest (build-list 6 (lambda (i) (ctile A (+ i 1)))))))
  
  ;; pick tiles in correct order 
  (test ordered-s     b7 CASH0 t7 banker-shares0 ALL-HOTELS A2 #f `(,AMERICAN ,AMERICAN))
  (test largest-alpha b7 CASH0 t7 banker-shares0 ALL-HOTELS B7 #f `(,AMERICAN ,AMERICAN))
  (test smallest-anti b7 CASH0 t7 banker-shares0 ALL-HOTELS A2 #f `(,TOWER ,TOWER))
  
  ;; price shares after placing the first tile 
  (define b8 (*create-board-with-hotels '() (list Continental-tiles)))
  (define t8 (list D3))
  (define h8 (remove CONTINENTAL ALL-HOTELS))
  
  (test ordered-s     b8 400 t8 (*create-shares CONTINENTAL 1) h8 D3 #f '())
  (test largest-alpha b8 400 t8 (*create-shares CONTINENTAL 1) h8 D3 #f '())
  (test smallest-anti b8 400 t8 (*create-shares CONTINENTAL 1) h8 D3 #f '())
  
  ;; remove one share for founding action, which may reduce the number of shares to be bought
  (define b9 (*create-board-with-hotels (list A1) '()))
  (define t9 (list A2))
  
  (test ordered-s     b9 300 t9 (*create-shares AMERICAN 1) `(,AMERICAN) A2 AMERICAN '())  
  (test largest-alpha b9 300 t9 (*create-shares AMERICAN 1) `(,AMERICAN) A2 AMERICAN '())  
  (test smallest-anti b9 300 t9 (*create-shares AMERICAN 1) `(,AMERICAN) A2 AMERICAN '())  
  )
