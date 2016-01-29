#lang racket

;; ---------------------------------------------------------------------------------------------------
;; interface specification for the interaction between the game administrator and 

(require "basics.rkt" "board.rkt" "state.rkt" "Lib/contract.rkt")

(provide
 (contract-out
  [turn-player/c 
   ;; the proxy object that encapsulates all needed information 
   contract?]
  
  [board-well-formed
   (-> board? any)]
  
  [complementary-hotels 
   ;; do the hotels on the board and the available hotels add up
   (-> board? (-> (listof hotel?) any))]
  
  ;; [player] refers to an automated player who presumably has cash and tiles 
  [good-placement 
   ;; can the [player] place the tile? 
   (-> (instanceof/c turn-player/c) (-> (maybe/c tile?) any))]
  
  [good-hotel-for-placement 
   ;; can the [player] use the specified hotel for placing the tile?
   (-> (instanceof/c turn-player/c) (maybe/c tile?) (-> (maybe/c hotel?) any))]
  
  [good-shares 
   ;; can the [player] buy the shares and are they available? 
   (-> (instanceof/c turn-player/c) (maybe/c tile?) (maybe/c hotel?) (-> shares-order/c any))]))


#| the turn is a proxy power object that
   enables the player to execute actions
   during its turn

   see admin-intf for player signatures

            Admin                                        Player1   Player2  Player3 
              |                                             |        |        |
  internal    |                                             |        |        |
   state      |   create(s = internal state)                |        |        |  create turn from 
              |------------------------------> t = Turn     |        |        |  internal state 
              |                                |            |        |        |
              |   take turn(t)                 |            |        |        |
              |-------------------------------------------->|        |        |  initiate turn 
              |                                |            |        |        |
              |                                |            |        |        |  initiate place:
              |                                | place(p,h) |        |        |  place callback for
              |                                |<-----------|        |        |  a merger placement
              |                                |            |        |        |  p = tile, h = hotel 
              |                                |            |        |        |
              |                                | keep(hs)   |        |        |  do you wish to keep
              |                                |----------->|        |        |  any shares in the
              |                                | keep(hs)   |        |        |  acquired hotels hs? 
              |                                |-------------------->|        |
              |                                | keep(hs)   |        |        |
              |                                |----------------------------->|
              |                                |            |        |        |
              |                                |            |        |        |  end place:
              |                                | place(hd)  |        |        |  here are everyone's
              |                                |===========>|        |        |  'keep' decisions hd
              |                                |            |        |        |
              |                                |            |        |        |
              |   take turn(p,h,o)             |            |        |        |  end turn:
              |<============================================|        |        |  tile p, hotel h,
              |                                |            |        |        |  buy o shares 
              |                                |            |        |        |
              |                                |            |        |        |
              |   check protocol()             |            |        |        |  check whether player
              |                                |            |        |        |  overstep its powers
              |                               ---           |        |        |
              |                              -----          |        |        |  if okay, commit 
              |                                             |        |        |  requested actions
              |                                |            |        |        |  otherwise, throw 
              |                                |            |        |        |  out player
|#

(define turn-player/c
  (class/c 
   (init-field 
    (current-state state?))
   
   (field 
    ;; (board-well-formed b)
    (board board?)
    (current player?) 
    (shares shares?) 
    ;; (complementary-hotels board)
    (hotels (listof hotel?)))
   
   (reconcile-shares 
    ;; update this turn's understanding of how many shares are available 
    (->m shares? any))
   
   (place
    ;; this method is called once, and exactly once, when a merger occurs 
    (->dm ([t tile?][h hotel?]) #:pre (eq? (what-kind-of-spot (get-field board this) t) MERGING)
          (players (listof player?))))))

;; --- turn input 
(define (board-well-formed b)
  ;; if I had someone else implement the player and/or the strategy, he
  ;; would have to refine this function to require minimal coherence 
  #t)

(define (complementary-hotels b)
  (define founded-hotels (filter (lambda (h) (size-of-hotel b h)) ALL-HOTELS))
  (define ALL-HOTELS-set (apply seteq ALL-HOTELS))
  (lambda (lh)
    (set=? (apply seteq (append founded-hotels lh)) ALL-HOTELS-set)))

;; --- turn output 
(define (good-placement turn)
  (define board (get-field board turn))
  (define players-tiles (player-tiles (get-field current turn)))
  (lambda (t)
    (or (boolean? t)
        (and (member t players-tiles)
             (not (eq? (what-kind-of-spot board t) IMPOSSIBLE))))))

(define (good-hotel-for-placement turn placement)
  (define board (get-field board turn))
  (define available-h (get-field hotels turn))
  (lambda (h)
    (define t placement)
    (cond 
      [(boolean? t) (not h)]
      [(boolean? h) 
       (define s (what-kind-of-spot board t))
       (and (memq s (list FOUNDING SINGLETON GROWING)) (==> (eq? s FOUNDING) (empty? available-h)))]
      [else 
       (define s (what-kind-of-spot board t))
       (cond
         ;; if founding action, make sure hotel is in provided list
         [(eq? FOUNDING s) (==> (cons? available-h) (member h available-h))]
         ;; if merging, make sure the hotel is the majority owner 
         [(eq? MERGING s) (let-values ([(w _) (merging-which board t)]) (member h w))]
         ;; if placement and hotel aren't #f, it should not be a different kind of spot 
         [else #f])])))

(define (good-shares turn tile hotel)
  (cond
    [(boolean? tile) (lambda (x) #t)]
    [else 
     (define state 
       (let ((state (get-field current-state turn)))
         (cond
           ;; did turn use a side-effect to update the state?, if not:
           [(free-spot? (state-board state) tile)
            (if hotel
                (state-place-tile state tile hotel)
                (state-place-tile state tile))]
           [else state])))
     (define board (state-board state))
     (define player-s-cash (player-money (state-current-player state)))
     (define available-s (state-shares state))
     (define (l-affordable? hotels) (affordable? board hotels player-s-cash))
     (define (l-available? hotels) (shares-available? available-s hotels))
     (and/c l-available? l-affordable?)]))
