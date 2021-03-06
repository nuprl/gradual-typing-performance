#lang typed/racket

;; ---------------------------------------------------------------------------------------------------
;; implementing Acquire game strategies

(provide
 ordered-s
 random-s
)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(require
 benchmark-util
 "../base/types.rkt"
 "board.rkt"
 "state.rkt"
 )
(require/typed/check "basics.rkt"
  (ALL-HOTELS (Listof Hotel))
  (SHARES-PER-TURN# Integer)
  (hotel<=? (-> Hotel Hotel Boolean))
  (price-per-share (-> Hotel Natural (Option Cash)))
  (shares++ (-> Shares Hotel Shares))
  (shares-- (-> Shares Hotel Shares))
  (shares-available (-> Shares Hotel Share))
)
(require/typed/check "auxiliaries.rkt"
  (randomly-pick (All (A) (-> (Listof A) A)))
  )
(define-type Board (HashTable Tile Content))
(define-type Tile tile)
(define-type State state)
(define-type Player player)
(define-type Decisions (Listof (List Player (Listof (List Hotel Boolean)))))
(define-type Score (Listof (List String Cash)))

(define-type Administrator%
  (Class
   (init-field
    (next-tile (-> (Listof Tile) Tile)))
   (sign-up (-> String (Instance Player%) String))
   (show-players (-> (Listof String)))
   (run (->* (Natural) (#:show (-> Void)) RunResult))
   ))

(define-type Turn%
  (Class
   (init-field (current-state State))
   (field
    (board Board)
    (current Player)
    (cash Cash)
    (tiles (Listof Tile))
    (shares Shares)
    (hotels (Listof Hotel))
    (players (Listof Player)))
   (reconcile-shares (-> Shares Shares))
   (eliminated (-> (Listof Player)))
   (place-called (-> Boolean))
   (decisions (-> (Values (Option Tile) (Option Hotel) Decisions)))
   ;; Precondition: (send this place-called)
   (place (-> Tile Hotel (U Void (Listof Player))))
   ))

(define-type Player%
  (Class
   (init-field
    [name String]
    [choice Strategy])
   (field
    [*players (Listof Player)]
    [*bad (Listof Player)])
   (go (-> (Instance Administrator%) Void))
   (setup (-> State Void))
   (take-turn (-> (Instance Turn%) (Values (Option Tile) (Option Hotel) (Listof Hotel))))
   (keep (-> (Listof Hotel) (Listof Boolean)))
   (receive-tile (-> Tile Void))
   (inform (-> State Void))
   (the-end (-> State Any Void))))

(define-type RunResult (List (U 'done 'exhausted 'score 'IMPOSSIBLE) Any (Listof State)))
(define-type Strategy (-> (Instance Turn%) (Values (Option Tile) (Option Hotel) (Listof Hotel))))


(: nat-SHARES-PER-TURN# Natural)
(define nat-SHARES-PER-TURN# (assert SHARES-PER-TURN# exact-nonnegative-integer?))

;; =============================================================================

;; (cons Hotel [Listof Hotel]) -> Hotel
(: alphabetically-first (-> (Listof Hotel) Hotel))
(define (alphabetically-first w)
  (first (sort w hotel<=?)))

(: pick-hotel (-> Board Tile Kind [Listof Hotel] 
                  [-> [Listof Hotel] Hotel]
                  [-> [Listof Hotel] Hotel]
                  [Option Hotel]))
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
(: to-buy (-> Board Cash Shares Natural [Listof Hotel]))
(define (to-buy b my-cash available-shares BUY-N)
  (let loop : (Listof Hotel)
       ((hotels : (Listof Hotel) ALL-HOTELS)
        (n : Natural 0)
        (to-buy : (Listof Hotel) '())
        (cash : Cash my-cash)
        (as : Shares available-shares))
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

(: strategy/d (-> [-> [Listof Hotel] Hotel]
                  [-> [Listof Hotel] Hotel]
                  (-> Natural Natural)
                  Strategy))
[define (strategy/d choose-founding choose-merger choose-shares#)
  (lambda ([turn : (Instance Turn%)])
    (define b (get-field board turn))
    (define my-cash (get-field cash turn))
    (define available-shares (get-field shares turn))
    (define available-hotels (get-field hotels turn))
    (define tile-kind
      (for/or : (Option (List Tile Kind))
              ((t : Tile (sort (get-field tiles turn) tile<=?)))
        (let ([s (what-kind-of-spot b t)])
          (and (not (eq? s IMPOSSIBLE))
               (list t s)))))
    (cond 
      [tile-kind
       (define-values (to-place kind) (values (car tile-kind) (cadr tile-kind)))
       (define hotel  (pick-hotel b to-place kind available-hotels choose-founding choose-merger))
       (define board  (set-board b to-place kind hotel))
       (define shares 
         (if (and (eq? FOUNDING kind) hotel (> (shares-available available-shares hotel) 0))
             (shares-- available-shares hotel)
             available-shares))
       (when (and (eq? MERGING kind) hotel) ;;bg; checks for hotel
         ;; ignore the decisions of the other players:
         (send turn place to-place hotel))
       (define buy
               (to-buy board my-cash shares nat-SHARES-PER-TURN#))
       (values to-place hotel buy)]
      [else
       (define buy 
         (to-buy b my-cash available-shares (choose-shares# nat-SHARES-PER-TURN#)))
       (values #f #f buy)]))]

(: id (All (A) (-> A A)))
(define (id x) x)

(: ordered-s Strategy)
(define ordered-s
  (strategy/d (inst first Hotel Hotel) alphabetically-first (inst id Natural)))

(: random+1 (-> Natural Natural))
(define (random+1 n)
  (ann (random (+ n 1)) Natural))

(: random-s Strategy)
(define random-s
  (strategy/d randomly-pick randomly-pick random+1))

