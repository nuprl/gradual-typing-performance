#lang racket

;; ---------------------------------------------------------------------------------------------------
;; a data representation for internal game states 
;; -- inspecting the game state 
;; -- manipulating the game state 
;; including a data structure for internalizing the state of the players 

(require "state-intf.rkt")

(internal& 
 player? player-money player-tiles player-shares player-external player-name player0
 xplayer? player->xexpr *create-player
 state? state-hotels state-shares state-sub-shares state-tiles state-board state-players 
 state-current-player
 state0
 state-place-tile state-buy-shares state-return-shares state-move-tile state-next-turn 
 state-remove-current-player state-eliminate state-score state-final? state-draw 
 xstate? state->xexpr
 *create-state *cs0)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION: 

(require "basics.rkt" "board.rkt" "Lib/auxiliaries.rkt" "Lib/xml.rkt" 2htdp/image)

(module+ test (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; DATA: 

(struct player (name tiles money shares external) #:transparent)
;; Player = (player String [Listof Tile] Amount Shares)
;; Amount = Nat
;; (player t a s) is the represetation of a player 
;; -- t is the list of tiles the player owns 
;; -- a is the amount of available money 
;; -- s are the shares owned 

(define player0
  (case-lambda 
    [(n t1 t2 t3 t4 t5 t6 x)
     (player n (list t1 t2 t3 t4 t5 t6) CASH0 player-shares0 x)]
    [(n t1 t2 t3 t4 t5 t6)
     (player n (list t1 t2 t3 t4 t5 t6) CASH0 player-shares0 #f)]
    [(n . v)
     (player n  v CASH0 player-shares0 #f)]))

(define (*create-player name cash shares tiles)
  (player name tiles cash shares #f))

;; Player Tile -> Player 
(define (player-tile- p t)
  (struct-copy player p (tiles (remove t (player-tiles p)))))

;; Player Tile -> Player 
(define (player-tile+ p t)
  (struct-copy player p (tiles (cons t (player-tiles p)))))

;; Player [Listof Hotel] -> Player 
(define (player-shares++ p . h)
  (if (empty? h)
      p
      (struct-copy player p (shares (for/fold ((s (player-shares p))) ((h h)) (shares++ s h))))))

;; Player ShareOrder Board -> Player 
(define (player-buy-shares p0 sh board)
  (define amount (for/sum ((h sh)) (price-per-share h (size-of-hotel board h))))
  (apply player-shares++ (struct-copy player p0 (money (- (player-money p0) amount))) sh))

;; Player Shares Board -> Player 
(define (player-returns-shares p0 transfers board)
  (struct-open player p0 shares money)
  (define amount (shares->money transfers board))
  (struct-copy player p0 (money (+ money amount)) (shares (shares-minus shares transfers))))

(define TEXT-SIZE 22) 

;; Player -> Image 
(define (player-image p)
  (struct-open player p name money tiles shares)
  (above (frame 
          (above/align 'left 
                       (text (format "~a :: owns $~a" name money) TEXT-SIZE 'red)
                       (text (string-join (map tile->string tiles)) TEXT-SIZE 'red)
                       (text (shares->string shares) TEXT-SIZE 'red)))
         (square 10 'solid 'white)))

;; ---------------------------------------------------------------------------------------------------
(struct state (board players tiles hotels shares bad) #:transparent)
;; State  = (state Board [Listof Player] [Listof Hotel] Shares [Listof Player])
;; (state b p t h s bad) is a representation of a game state: 
;; -- b is the current board 
;; -- p is the state of all the players 
;;    the list order determines the order of turns,
;;    it is the turn of the first player on the list
;; -- t is the list of available tiles 
;; -- h is the list of available hotels 
;; -- s is the list of available shares 
;; -- bad is the list of players gone bad 

;; ---------------------------------------------------------------------------------------------------
;; FUNCTIONS: placing a tile and all consequences 

(define (state0 . p)
  (define tiles-owned-by-players (apply append (map player-tiles p)))
  (define tiles-in-pool (remove* tiles-owned-by-players ALL-TILES))
  (state (board) p tiles-in-pool ALL-HOTELS banker-shares0 '()))

(define (state-sub-shares s bad-shares)
  (struct-copy state s (shares (shares-minus (state-shares s) bad-shares))))
  
;; Player *-> State 
;; create players with given names, assigning random tiles from the full pool 
(define (*cs0 . names)
  (let loop ((names names) (tiles ALL-TILES) (players '()))
    (cond
      [(empty? names) (state (board) (reverse players) tiles ALL-HOTELS banker-shares0)]
      [else (define first-six (take tiles STARTER-TILES#))
            (define player1 (player (first names) first-six CASH0 player-shares0))
            (loop (rest names) (drop tiles STARTER-TILES#) (cons player1 players))])))

;; Board [Listof Player] -> State
;; create a state by subtracting the tiles on the board and the players from the pool
;; subtracting the shares of the players from the full shares 
;; subtracting the placed hotels from the available hotels 
(define (*create-state board players)
  (define players-shares (map player-shares players))
  (define remaining-shares 
    (for/fold ((remaining-shares banker-shares0)) [(s players-shares)]
      (shares-minus remaining-shares s)))
  (define remaining-hotels 
    (for/list ((h ALL-HOTELS) #:when (= (size-of-hotel board h) 0)) h))
  (define remaining-tiles 
    (remove* (apply append (board-tiles board) (map player-tiles players)) ALL-TILES))
  (state board players remaining-tiles remaining-hotels remaining-shares '()))

(require "Lib/struct.rkt")

;; State Tile [Hotel] -> State 
;; place the tile (in the possession of player 1) onto board
;; if founding, use hotel 
;; if merging, hotel is acquirer 
(define (state-place-tile s tile (hotel #f))
  (struct-open state s board hotels players shares tiles)
  (define current (player-tile- (first players) tile))
  (define others (rest players))
  (define players-next (cons current others))
  (define tiles-next (remove tile tiles))
  (define spot (what-kind-of-spot board tile))
  (cond
    [(or (eq? SINGLETON spot) (eq? GROWING spot) (and (eq? FOUNDING spot) (not hotel)))
     (define new-board 
       (if (eq? GROWING spot)
           (grow-hotel board tile)
           (place-tile board tile)))
     (struct-copy state s (board new-board) (tiles tiles-next) (players players-next))]
    [(eq? FOUNDING spot)
     (define t 
       (struct-copy state s
                    (hotels (remove hotel hotels))
                    (tiles tiles-next)
                    (board (found-hotel board tile hotel))))
     (if (= (shares-available shares hotel) 0)
      (struct-copy state t (players players-next))
      (struct-copy state t
                   (shares (shares-- shares hotel))
                   (players (cons (player-shares++ current hotel) others))))]
    [(eq? MERGING spot)
     (define-values (w l) (merging-which board tile))
     (define acquired (append (remove hotel w) l))
     (define next-state
       (struct-copy state s
                    (board (merge-hotels board tile hotel))
                    (hotels (append acquired hotels))
                    (tiles tiles-next)
                    (players players-next)))
     ;; now distribute the bonus
     (foldr (state-distribute-bonus board) next-state acquired)]))

;; [Listf Hotel Nat] State -> State 
;; distribute bonus for the acquired hotel 
(define ((state-distribute-bonus board) acquired-hotel s)
  (define size-acquired (size-of-hotel board acquired-hotel))
  (define players (state-players s))
  (define selector (lambda (p) (shares-available (player-shares p) acquired-hotel)))
  (define owners-of-acquired (filter (lambda (p) (> (selector p) 0)) players))
  (define owners-of-acquired-sorted (sort owners-of-acquired > #:key selector))
  (cond
    [(empty? owners-of-acquired-sorted) s]
    [else 
     (define majority-minority (partition owners-of-acquired-sorted selector))
     (define majority (first majority-minority))
     (define minority (if (empty? (rest majority-minority)) '() (second majority-minority)))
     (define majority-bonus (bonus 'majority acquired-hotel size-acquired))
     (define minority-bonus (bonus 'minority acquired-hotel size-acquired))
     (cond
       [(cons? (rest majority))
        ;; distribute the majority+minority bonus
        (define total-bonus (+ majority-bonus minority-bonus))
        (define bonus-per (quotient total-bonus (length majority)))
        (struct-copy state s (players (foldr (state-pay-out bonus-per) players majority)))]
       [(cons? minority)  ;; (empty? (rest majority))
        (define single-majority (first majority))
        (define majority-payed ((state-pay-out majority-bonus) single-majority players))
        (define bonus-per (quotient minority-bonus (length minority)))
        (struct-copy state s (players (foldr (state-pay-out bonus-per) majority-payed minority)))]
       [(empty? minority)  ;; (empty? (rest majority))
        (define single-majority (first majority))
        (struct-copy state s (players ((state-pay-out majority-bonus) single-majority players)))])]))

;; Cash -> (Player [Listof Players] -> [Listof Player])
(define (state-pay-out bonus)
  ;; add cash to wallet of pay-to in players
  (lambda (pay-to players)
    (define the-name (player-name pay-to))
    (for/list ((p players))
      (struct-open player p name money)
      (if (string=? name the-name) (struct-copy player p (money (+ money bonus))) p))))

;; ---------------------------------------------------------------------------------------------------
;; FUNCTIONS: external 

(define (state-move-tile s t)
  (struct-open state s players tiles)
  (struct-copy state s 
               (players (cons (player-tile+ (first players) t) (rest players)))
               (tiles (remove t tiles))))

(define (state-next-turn s)
  (define players (state-players s))
  (struct-copy state s (players (append (rest players) (list (first players))))))

(define (state-remove-current-player s)
  (define players (state-players s))
  (struct-copy state s (players (rest players)) (bad (cons (first players) (state-bad s)))))

(define (state-eliminate s ep)
  (struct-copy state s (players (remove* ep (state-players s))) (bad (append ep (state-bad s)))))

(define (state-current-player s)
  (first (state-players s)))

(define (state-buy-shares s sh)
  (struct-open state s players board shares)
  (struct-copy state s 
               (players (cons (player-buy-shares (first players) sh board) (rest players)))
               (shares (for/fold ((s shares)) ((h sh)) (shares-- s h)))))

(define (state-return-shares s decisions (board (state-board s)))
  (for/fold ((s s)) ((d decisions))
    (state-return-shares/player s (first d) (second d) board)))

;; State Player [Listof [List Hotel Any]] -> State 
;; return player p shares to state s according to its decisions 
(define (state-return-shares/player s p p-s-decisions (board (state-board s)))
  (define the-name (player-name p))
  (define player-s (player-shares p))
  (define transfers (shares-to-be-moved+their-value player-s p-s-decisions))
  (struct-open state s players shares)
  (define new-players 
    (for/list ((q players)) 
      (if (string=? (player-name q) the-name)
          (player-returns-shares q transfers board)
          q)))
  (struct-copy state s (shares (shares-plus shares transfers)) (players new-players)))

;; Board Shares [Listof [List Hotel Boolean]] -> Shares 
;; determine how the shares that must be transfered from player-s-shares to banker-s-shares 
(define (shares-to-be-moved+their-value player-s-shares decisions)
  (for/fold ((shares player-shares0)) ((d decisions))
    (define hotel (first d))
    (cond
      [(second d) shares]
      [else (define available (shares-available player-s-shares hotel))
            (for/fold ((shares shares)) ((n (in-range available))) (shares++ shares hotel))])))

(define (state-score s0)
  (define board (state-board s0))
  (define bonus (state-distribute-bonus board))
  (define state/bonus
    (foldr (lambda (h s) (if (= (size-of-hotel board h) 0) s (bonus h s))) s0 ALL-HOTELS))
  (define scores 
    (for/list ((p (state-players state/bonus)))
      (struct-open player p name money shares)
      (list name (+ money (shares->money shares board)))))
  (sort scores 
        (lambda (p q) 
          (or (> (second p) (second q)) 
              (and (= (second p) (second q)) (string<=? (first p) (first q)))))))

(define (shares->money shares board)
  (for/sum ([(hotel n) shares])
    (define size (size-of-hotel board hotel))
    (define price (price-per-share hotel size))
    (if (and (> size 0) price) (* price n) 0)))

(define (state-final? s)
  (define board  (state-board s))
  (define-values (winner? founded safe)
    (for/fold ((winner? #f) (founded '()) (safe '())) ((h ALL-HOTELS))
      (define s (size-of-hotel board h))
      (cond
        [(>= s SAFE#) (values (>= s FINAL#) (cons h founded) (cons h safe))]
        [(> s 0) (values winner? (cons h founded) safe)]
        [else (values winner? founded safe)])))
  (or winner? (and (cons? founded) (= (length founded) (length safe)))))

(define (state-draw s)
  (struct-open state s board players)
  (define board:image (draw board))
  (define players:image (map player-image players))
  (above/align 'left 
               board:image
               (rectangle 10 10 'solid 'white)
               (cond
                 [(empty? players:image) empty-image]
                 [(empty? (rest players:image)) (first players:image)]
                 [else (apply above/align 'left players:image)])))

;; ---------------------------------------------------------------------------------------------------
;; externalizing state 

;; State -> Xexpr
(define (state->xexpr s)
  (struct-open state s players board)
  `(state () ,(board->xexpr board) ,@(map player->xexpr players)))

;; Player -> Xexpr 
(define (player->xexpr p)
  (struct-open player p tiles name money shares)
  `(player ((name ,name) (cash ,(cash->string money))) 
           ,@(shares->xexpr shares)
           ,@(map tile->xexpr (sort tiles tile<=?))))

(define xplayer?
  (xml-predicate (player ((name string?) (cash string->cash)) xshare? ... xtile? ...)))

(define xstate?
  (xml-predicate (state () xboard? xplayer? ... )))

;; ---------------------------------------------------------------------------------------------------
;; testing 
(module+ test
  
  (define-syntax (test-place-tile stx)
    (syntax-case stx (--bonus--)
      [(test-place-tile merger? (board b*) (tile t*) (hotel h* all-h) players-1 next-tiles expected)
       #'(test-place-tile merger? (board b*) (tile t*) (hotel h* all-h) 
                          --bonus-- (('none 0 0)) players-1 next-tiles expected)]
      [(test-place-tile merger? (board b*) (tile t*) players-1 next-tiles expected)
       #'(test-place-tile merger? (board b*) (tile t*) (hotel #f ALL-HOTELS) 
                          --bonus-- (('none 0 0)) players-1 next-tiles expected)]
      [(test-place-tile merger? (board b*) (tile t*) (hotel h* all-h)
                        --bonus-- ((acquired no-shares pay-out) ...) players-1 next-tiles expected)
       #'(let ()
           (define extra-tiles (list (ctile A 2) (ctile A 3) (ctile A 4)))
           (define tile t*)
           (define shares-player 
             (for/fold ((ps player-shares0)) ((a (list acquired ...)) (i (list no-shares ...)))
               (for/fold ((ps ps)) ((i i)) (shares++ ps a))))
           (define player-0 
             (player "a" (cons tile extra-tiles) CASH0 shares-player #f))
           (define extra-player
             (player "b" '() CASH0 player-shares0 #f))
           
           (define board b*)
           (define hotel h*)
           (define next-tiles (remove tile ALL-TILES))
           (define state-0
             (state board (list player-0 extra-player) next-tiles all-h banker-shares0 '()))
           
           (define players-1
             (list
              (player "a"
                      extra-tiles
                      (+ CASH0 pay-out ...)
                      (if (and hotel (not (eq? MERGING merger?)))
                          (shares++ shares-player hotel)
                          shares-player)
                      #f)
              extra-player))
           
           (check-equal? 
            (if hotel (state-place-tile state-0 tile hotel) (state-place-tile state-0 tile)) 
            expected
            (format "~a" merger?)))]))
  
  ;; --- placing a tile as a singleton --- 
  (test-place-tile SINGLETON
                   (b (board))
                   (t (ctile A 1))
                   p p-tiles 
                   (state (place-tile b t) p p-tiles ALL-HOTELS banker-shares0 '()))
  
  ;; --- growing a hotel ---
  (test-place-tile GROWING
                   (b board-a2-b2-american)
                   (t (ctile C 2))
                   p p-tiles 
                   (state (grow-hotel b t) p p-tiles ALL-HOTELS banker-shares0 '()))
  
  ;; --- placing a founding tile on board --- 
  (test-place-tile FOUNDING
                   (b board-a1-b2-c6)
                   (t (ctile D 6))
                   (h AMERICAN ALL-HOTELS)
                   p p-tiles 
                   (state (found-hotel b t h) p p-tiles (remove h ALL-HOTELS) 
                          (shares-- banker-shares0 h)
                          '()))
  
  ;; --- merging a hotel chain: no owner --- 
  (test-place-tile MERGING 
                   (b board-b2-c2-am-c4-d4-tw-e4)
                   (t (ctile C 3))
                   (h TOWER (remove* `(,TOWER ,AMERICAN) ALL-HOTELS))
                   new-players p-tiles 
                   (state (merge-hotels b t h) new-players p-tiles (remove h ALL-HOTELS) 
                          banker-shares0
                          '()))
  
  ;; --- merging a hotel chain: one majority owner --- 
  (test-place-tile MERGING 
                   (b board-b2-c2-am-c4-d4-tw-e4)
                   (t (ctile C 3))
                   (h TOWER (remove* `(,TOWER ,AMERICAN) ALL-HOTELS))
                   --bonus-- ((AMERICAN 3 (bonus 'majority AMERICAN 2)))
                   p p-tiles 
                   (state (merge-hotels b t h) p p-tiles (remove h ALL-HOTELS) banker-shares0 '()))
  
  ;; --- merging three hotel chains: one majority and one minority owner 
  (test-place-tile MERGING 
                   (b board-3way-merger-at-d3)
                   (t (ctile D 3))
                   (h TOWER (remove* `(,TOWER ,AMERICAN ,WORLDWIDE) ALL-HOTELS))
                   --bonus-- ((AMERICAN 3 (bonus 'majority AMERICAN 3))
                              (WORLDWIDE 2 (bonus 'majority WORLDWIDE 2)))
                   p tiles 
                   (state (merge-hotels b t h) ;; one hotel left 
                          p ;; bonus-added to player 1 
                          tiles ;; tile t removed 
                          (list* AMERICAN WORLDWIDE (remove* `(,TOWER ,AMERICAN ,WORLDWIDE) ALL-HOTELS))
                          banker-shares0 
                          '()))
  
  ;; --- merging with three majority owners 
  (define C3 (ctile C 3))
  
  (define board-b2-c2-am-c4-d4-tw-e4-c3-d3 (merge-hotels board-b2-c2-am-c4-d4-tw-e4 C3 TOWER))
  
  (define maj (bonus 'majority AMERICAN 2))
  (define min (bonus 'minority AMERICAN 2))
  (define bouns-3-majority-owners (+ (quotient maj 3) (quotient min 3) 6000))
  
  (check-equal? (state-place-tile 
                 (state board-b2-c2-am-c4-d4-tw-e4
                        (list (player "a" (list C3) 6000 (shares++ player-shares0 AMERICAN) #f)
                              (player "b" '() 6000 (shares++ player-shares0 AMERICAN) #f)
                              (player "c" '() 6000 (shares++ player-shares0 AMERICAN) #f))
                        ALL-TILES
                        (remove* `(,TOWER ,AMERICAN) ALL-HOTELS)
                        banker-shares0
                        '())
                 C3
                 TOWER)
                (state
                 board-b2-c2-am-c4-d4-tw-e4-c3-d3
                 (list (player "a" '() bouns-3-majority-owners (shares++ player-shares0 AMERICAN) #f)
                       (player "b" '() bouns-3-majority-owners (shares++ player-shares0 AMERICAN) #f)
                       (player "c" '() bouns-3-majority-owners (shares++ player-shares0 AMERICAN) #f))
                 (remove C3 ALL-TILES)
                 (remove TOWER ALL-HOTELS)
                 banker-shares0
                 '()))
  
  ;; -- merger case with one majority and one minority owner 
  
  (check-equal? 
   (state-place-tile
    (state board-b2-c2-am-c4-d4-tw-e4
           (list (player "a" (list C3) 6000 (shares++ (shares++ player-shares0 AMERICAN) AMERICAN) #f)
                 (player "b" '() 6000 (shares++ player-shares0 AMERICAN) #f)
                 (player "c" '() 6000 player-shares0 #f))
           ALL-TILES
           (remove* `(,TOWER ,AMERICAN) ALL-HOTELS)
           banker-shares0
           '())
    C3
    TOWER)
   (state board-b2-c2-am-c4-d4-tw-e4-c3-d3
          (list (player "a" '() (+ maj 6000) (shares++ (shares++ player-shares0 AMERICAN) AMERICAN) #f)
                (player "b" '() (+ min 6000) (shares++ player-shares0 AMERICAN) #f)
                (player "c" '() 6000 player-shares0 #f))
          (remove C3 ALL-TILES)
          (remove TOWER ALL-HOTELS)
          banker-shares0
          '()))
  
  ;; -- merger case with one majority and two minority owners 
  
  (define min/2 (quotient min 2))
  
  (check-equal? 
   (state-place-tile
    (state board-b2-c2-am-c4-d4-tw-e4
           (list (player "a" (list C3) 6000 (shares++ (shares++ player-shares0 AMERICAN) AMERICAN) #f)
                 (player "b" '() 6000 (shares++ player-shares0 AMERICAN) #f)
                 (player "c" '() 6000 (shares++ player-shares0 AMERICAN) #f))
           ALL-TILES
           (remove* `(,TOWER ,AMERICAN) ALL-HOTELS)
           banker-shares0
           '())
    C3
    TOWER)
   (state board-b2-c2-am-c4-d4-tw-e4-c3-d3
          (list (player "a" '() (+ maj 6000) (shares++ (shares++ player-shares0 AMERICAN) AMERICAN) #f)
                (player "b" '() (+ min/2 6000) (shares++ player-shares0 AMERICAN) #f)
                (player "c" '() (+ min/2 6000) (shares++ player-shares0 AMERICAN) #f))
          (remove C3 ALL-TILES)
          (remove TOWER ALL-HOTELS)
          banker-shares0
          '()))
  
  ;; --- moving a tile from the banker's pool to the current players 
  (check-equal? (state-move-tile (state0 (player0 "a")) (ctile A 1)) (state0 (player0 "a" (ctile A 1))))
  
  ;; --- switching turns 
  (check-equal? (state-next-turn (state0 (player0 (ctile A 1)))) (state0 (player0 (ctile A 1))))
  (check-equal? (state-next-turn 
                 (state0 (player0 (ctile A 1)) (player0 (ctile B 2)) (player0 (ctile C 3))))
                (state0 (player0 (ctile B 2)) (player0 (ctile C 3)) (player0 (ctile A 1))))
  
  ;; --- banker sells shares to current player ---
  (define p (player0 "a" (ctile A 1)))
  (define s (state board-b2-c2-am-c4-d4-tw-e4 (list p) ALL-TILES ALL-HOTELS banker-shares0 '()))
  (check-equal? (state-buy-shares s (list AMERICAN TOWER AMERICAN))
                (let* ([ps player-shares0]
                       [ps (shares++ ps AMERICAN)]
                       [ps (shares++ ps AMERICAN)]
                       [ps (shares++ ps TOWER)]
                       [ss banker-shares0]
                       [ss (shares-- ss AMERICAN)]
                       [ss (shares-- ss AMERICAN)]
                       [ss (shares-- ss TOWER)])
                  (struct-copy state s
                               (shares ss)
                               (players 
                                (list (player "a"
                                              (list (ctile A 1))
                                              (- CASH0
                                                 (* 2 (price-per-share AMERICAN 2))
                                                 (price-per-share TOWER 3))
                                              ps
                                              #f))))))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; is the state final? 
  
  ;; an initial state with two partially "tiled" players
  (define state-not-final
    (state0 (player0 "a" (ctile A 1) (ctile A 2)) (player0 "b" (ctile B 1) (ctile B 2))))
  (check-false (state-final? state-not-final))
  (state-draw state-not-final)
  
  (define American-safe-tiles (build-list SAFE# (lambda (i) (ctile C (+ i 1)))))
  
  (define b-with-one-safe-hotel (*create-board-with-hotels '() `((,AMERICAN ,@American-safe-tiles))))
  
  ;; --- final state testing and scoring for a state with a board that contains of ONE safe hotel 
  (define s-with-one-safe-hotel 
    (state b-with-one-safe-hotel (list p) ALL-TILES ALL-HOTELS banker-shares0 '()))
  (check-true (state-final? s-with-one-safe-hotel))
  
  (check-equal? (state-score s-with-one-safe-hotel) (list (list (player-name p) (player-money p))))
  
  ;; --- final state testing and scoring for board with a final hotel 
  (define Continental-final-tiles
    (append 
     (build-list SAFE# (lambda (i) (ctile E (+ i 1))))
     (build-list SAFE# (lambda (i) (ctile F (+ i 1))))
     (build-list SAFE# (lambda (i) (ctile G (+ i 1))))
     (build-list SAFE# (lambda (i) (ctile H (+ i 1))))))
  
  (define American-non-safe-tiles
    (build-list (- SAFE# 1) (lambda (i) (ctile C (+ i 1)))))
  
  (unless (>= (length Continental-final-tiles) FINAL#)
    (check-true #f))
  
  (define b-with-final-hotel
    (*create-board-with-hotels 
     '()
     `((,AMERICAN ,@American-non-safe-tiles)
       (,CONTINENTAL ,@Continental-final-tiles))))
  
  (define s-with-final-hotel
    (state b-with-final-hotel 
           (list p) 
           ALL-TILES 
           (remove* (list AMERICAN CONTINENTAL) ALL-HOTELS)
           banker-shares0
           '()))
  
  (check-true (state-final? s-with-final-hotel))
  (check-equal? (state-score s-with-final-hotel) (list (list (player-name p) (player-money p))))
  
  ;; --- scoring for two players w/o ownership
  (define pb (player0 "b" (ctile A 2)))
  (define s-with-final-hotel2
    (state b-with-final-hotel 
           (list pb p) 
           ALL-TILES 
           (remove* (list AMERICAN CONTINENTAL) ALL-HOTELS)
           banker-shares0
           '()))
  
  (check-equal? (state-score s-with-final-hotel2) 
                (list (list (player-name p) (player-money p))
                      (list (player-name pb) (player-money pb))))
  
  ;; --- scoring with player who owns shares of a hotel that does not exist 
  (define pbad (player "bad" (list (ctile A 2)) CASH0 (shares++ player-shares0 AMERICAN) #f))
  (check-equal? (state-score (state (board) (list pbad) ALL-TILES ALL-HOTELS banker-shares0 '()))
                (list (list (player-name pbad) (player-money pbad))))
  
  
  ;; --- scoring with two owners (majority and minority) for one hotel   
  (define pc 
    (player "c" (list (ctile A 2)) CASH0 (shares++ (shares++ player-shares0 AMERICAN) AMERICAN) #f))
  (define portfolio-a (shares++ player-shares0 AMERICAN))
  (define pa (player "a" (list (ctile A 2)) CASH0 portfolio-a #f))
  
  (define s-with-final-hotel3
    (state b-with-final-hotel 
           (list pc pa) 
           ALL-TILES 
           (remove* (list AMERICAN CONTINENTAL) ALL-HOTELS)
           banker-shares0
           '()))
  
  (define (p-money pc status)
    (+ (bonus status AMERICAN (length American-non-safe-tiles))
       (*  (hash-ref (player-shares pc) AMERICAN) 
           (price-per-share AMERICAN (length American-non-safe-tiles)))
       (player-money pc)))
  
  (check-equal? (state-score s-with-final-hotel3) 
                (list (list (player-name pc) (p-money pc 'majority))
                      (list (player-name pa) (p-money pa 'minority))))
  
  ;; --- which shares to move from players to bank, what is their value 
  (define American-non-safe-size (length American-non-safe-tiles))
  (define s1 (shares-to-be-moved+their-value player-shares0 `((,AMERICAN #t))))
  (check-equal? s1 player-shares0)
  
  (define shares0 (shares-available banker-shares0 AMERICAN))
  (define s2 (shares-to-be-moved+their-value banker-shares0 `((,AMERICAN #f))))
  (check-equal? s2 (for/fold ((sh player-shares0)) ((n (in-range shares0))) (shares++ sh AMERICAN)))
  
  (define s3 (shares-to-be-moved+their-value portfolio-a `((,AMERICAN #f))))
  (check-equal? s3 portfolio-a)
  
  ;; --- perform move of shares for one player 
  (define price-per-AMERICAN (price-per-share AMERICAN American-non-safe-size))
  
  (check-equal? 
   (state-return-shares/player s-with-final-hotel3 pa `((,AMERICAN #f)))
   (state b-with-final-hotel
          (list pc (player "a" (list (ctile A 2)) (+ CASH0 price-per-AMERICAN) player-shares0 #f))
          ALL-TILES 
          (remove* (list AMERICAN CONTINENTAL) ALL-HOTELS)
          (shares++ banker-shares0 AMERICAN)
          '()))
  
  ;; --- and for two 
  (check-equal? 
   (state-return-shares s-with-final-hotel3 `((,pa ((,AMERICAN #f))) (,pc ((,CONTINENTAL #f)))))
   (state b-with-final-hotel
          (list pc (player "a" (list (ctile A 2)) (+ CASH0 price-per-AMERICAN) player-shares0 #f))
          ALL-TILES 
          (remove* (list AMERICAN CONTINENTAL) ALL-HOTELS)
          (shares++ banker-shares0 AMERICAN)
          '()))
  
  (check-equal? 
   (state-return-shares s-with-final-hotel3 `((,pa ((,AMERICAN #f))) (,pc ((,AMERICAN #f)))))
   (state b-with-final-hotel
          (list
           (player "c" (list (ctile A 2)) (+ CASH0 (* 2 price-per-AMERICAN)) player-shares0 #f)
           (player "a" (list (ctile A 2)) (+ CASH0 price-per-AMERICAN) player-shares0 #f))
          ALL-TILES 
          (remove* (list AMERICAN CONTINENTAL) ALL-HOTELS)
          (shares++ (shares++ (shares++ banker-shares0 AMERICAN) AMERICAN) AMERICAN)
          '()))
  
  ;; --- test state-return-shares under the same conditions as used in tree
  (define D3 (ctile D 3))
  (define shares-for-am-tw-ww 
    (*combine-shares
     (list (*create-shares AMERICAN 3) (*create-shares TOWER 3) (*create-shares WORLDWIDE 3))))
  (define player-can-place-d3 (*create-player "a" 1000 shares-for-am-tw-ww (list D3)))
  (define state-merge-at-d3 (*create-state board-3way-merger-at-d3 (list player-can-place-d3)))
  (define acquired `(,AMERICAN ,WORLDWIDE))
  (define d* `((,player-can-place-d3 ,(map (lambda (h) (list h #f)) acquired))))
  (define state/tile (state-place-tile state-merge-at-d3 D3 TOWER))
  (check-equal? (player-money (first (state-players (state-return-shares state/tile d* board-3way-merger-at-d3))))
                (+ (player-money player-can-place-d3)
                   (for/sum ((h acquired))
                     (+ (bonus 'majority h (size-of-hotel board-3way-merger-at-d3 h))
                        (* (shares-available shares-for-am-tw-ww h)
                           (price-per-share h (size-of-hotel board-3way-merger-at-d3 h)))))))
  
  ;; --- removing players 
  (define p1 (player "1" '() 0 player-shares0 #f))
  (define p2 (player "2" '() 0 player-shares0 #f))
  (define p3 (player "3" '() 0 player-shares0 #f))
  (define s0 (state (board) (list p1 p2 p3) ALL-TILES ALL-HOTELS banker-shares0 '()))
  (define s0-removed (state (board) (list p2) ALL-TILES ALL-HOTELS banker-shares0 (list p1 p3)))
  (check-equal? (state-eliminate s0 (list p1 p3)) s0-removed)
  )

(module+ sample-states 
  (require (submod "board.rkt" tiles+spots) "board.rkt")
  (provide s0 s1 s2 s0-name s3-merge-player-gets-bonus-can-buy)
  
  (define s0 
    (state board-b2-c2-am-c4-d4-tw-e4
           (list (player0 "a" A1))
           (remove* (list A1 B2 C2 C4 D4 E4) ALL-TILES)
           (remove* (list AMERICAN TOWER) ALL-HOTELS)
           banker-shares0
           '()))
  
  (define (s0-name name)
    (state board-b2-c2-am-c4-d4-tw-e4
           (list (player0 name A1))
           (remove* (list A1 B2 C2 C4 D4 E4) ALL-TILES)
           (remove* (list AMERICAN TOWER) ALL-HOTELS)
           banker-shares0
           '()))
  
  (define s1
    (state board-b2-c2-am-c4-d4-tw-e4
           (list (player0 "a" A1)
                 (player "b" (list A11) CASH0 (shares++ player-shares0 AMERICAN) #f))
           (remove* (list A1 B2 C2 C4 D4 E4 A11) ALL-TILES)
           (remove* (list AMERICAN TOWER) ALL-HOTELS)
           (shares-- banker-shares0 AMERICAN)
           '()))
  
  (define (s2 ep2a ep2b ep2c)
    (define p2a (player "a" (list C3) 6000 (shares++ (shares++ player-shares0 AMERICAN) AMERICAN) ep2a))
    (define p2b (player "b" '() 6000 (shares++ player-shares0 AMERICAN) ep2b))
    (define p2c (player "c" '() 6000 (shares++ player-shares0 AMERICAN) ep2c))
    (state board-b2-c2-am-c4-d4-tw-e4
           (list p2a p2b p2c)
           (remove* (list B2 C2 C4 D4 E4 C3) ALL-TILES)
           (remove* `(,TOWER ,AMERICAN) ALL-HOTELS)
           (for/fold ((s banker-shares0)) [(n (in-range 4))] (shares-- s AMERICAN))
           '()))
  
  (define (s3-merge-player-gets-bonus-can-buy sh)
    (state board-b2-c2-am-c4-d4-tw-e4
           (list (player "a" (list C3) 300 (shares++ (shares++ player-shares0 AMERICAN) AMERICAN) #f)
                 (player "b" '() 6000 (shares++ player-shares0 AMERICAN) #f)
                 (player "c" '() 6000 player-shares0 #f))
           (remove* (list B2 C2 C4 D4 E4 C3) ALL-TILES)
           (remove* `(,TOWER ,AMERICAN) ALL-HOTELS)
           sh
           '()))
  
  )
