#lang typed/racket

;; ---------------------------------------------------------------------------------------------------
;; Acquire player that manages all the interactions; delegates decision making to separate strategies 

(require "player-intf.rkt")

(player& player? create)
(provide xaction? action->xexpr)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION 

(require "require-typed-check.rkt" "strategy.rkt" (except-in "Lib/xml.rkt" numbered-attributes)
         (except-in "typed-wrapper.rkt" player?))

(require/typed/check "admin.rkt" [turn% Turn-Player%])

(require/typed "Lib/xml.rkt" [numbered-attributes (String (Hotel -> String) (Listof Any) -> (Listof (List Symbol String)))])

(module+ test 
  (require typed/rackunit (submod "board.rkt" tiles+spots)))

(: player? (Any -> Boolean))
(define (player? x)
  (is-a? x player%))

(: create (String Strategy -> (Instance Player%)))
(define (create n c)
  (new player% [name n][choice c]))

(: player% Player%)
(define player%
  (class object%
    (init-field name choice)
    (super-new)

    (define: *players : (Listof Player) '())
    (define: *bad : (Listof Player) '())

    (define *my-game-name "")
    
    (define/public (go administrator)
      (set! *my-game-name (send administrator sign-up name this)))
    
    (define/public (setup s)
      (bad-players (state-players s)))
    
    (define/public (take-turn turn) 
      (bad-players (get-field players turn))
      ;; reconcile is added for remote players that don't get complete information 
      (define-values (to-place hotel shares-to-buy) (choice (reconcile turn)))
      (values to-place hotel (assert shares-to-buy shares-order?)))
    
    (define/public (keep hotels)
      (map (lambda (h) (< (random 100) 50)) hotels))
    
    (define/public (receive-tile t)
      (void))
    
    (define/public (inform s)
      (bad-players (state-players s)))
    
    (define/public (the-end state results)
      ;; I should figure out what to do here 
      (void))
    
    ;; given the list of current players, find out which ones dropped out since the last update
    ;; effect: move the drop-out players to this.*bad, keep the good ones in this.*players
    (: bad-players ((Listof Player) -> Void))
    (define/private (bad-players players)
      (set! *bad 
            (for/fold: : (Listof Player) ((bad : (Listof Player) *bad)) ((old-player : Player *players)) 
              (define n (player-name old-player))
              (if (findf (lambda: ([current : Player]) (string=? (player-name current) n)) players) 
                  bad
                  (cons old-player bad))))
      (set! *players players))
    
    ;; effect: reduce the turn's banker-shares by the shares of this.*bad players
    (: reconcile ((Instance Turn-Player%) -> (Instance Turn-Player%)))
    (define/private (reconcile turn)
      (define bad-shares (*combine-shares (map player-shares *bad)))
      (send turn reconcile-shares bad-shares)
      turn)))

;; ---------------------------------------------------------------------------------------------------
;; externalizing actions 

(: xaction? (String -> Boolean))
(define xaction?
  (xml-predicate 
   (action ())
   (action () xplace?)
   (action ((hotel1 string->hotel)) xplace?)
   (action ((hotel1 string->hotel) (hotel2 string->hotel)) xplace?)
   (action ((hotel1 string->hotel) (hotel2 string->hotel) (hotel3 string->hotel)) xplace?)
   (action ((hotel1 string->hotel)))
   (action ((hotel1 string->hotel) (hotel2 string->hotel)))
   (action ((hotel1 string->hotel) (hotel2 string->hotel) (hotel3 string->hotel)))))

;; does the given xpexression represent a tile placement? 
(: xplace? (String -> Boolean))
(define xplace?
  (xml-predicate 
   (place ((column string->column) (row string->row) (hotel string->hotel)))
   (place ((column string->column) (row string->row)))))

(: action->xexpr ((Option Tile) (Option Hotel) Shares-Order -> Any))
(define (action->xexpr placement hotel shares-to-buy)
  (define attributes (numbered-attributes "hotel~a" hotel->label (assert shares-to-buy list?)))
  (cond
    [(not placement) `(action (,@attributes))]
    [else 
     (define spot (second (assert (tile->xexpr placement) list?)))
     (define hotl (if hotel `((hotel ,(hotel->label hotel))) '()))
     `(action (,@attributes) (place (,@(assert spot list?) ,@hotl)))]))

;; ---------------------------------------------------------------------------------------------------
;; (: testx (Strategy Board Cash (Listof Tile) Shares (Listof Hotel) (Board (Instance Player%) -> (Tile (Listof Shares) -> Any)) -> Any))
#;
(define (testx S board money tiles available-shares available-hotels checker)
  (define p (create "a" S))
  (define ip (*create-player "a" money player-shares0 tiles))
  (call-with-values 
   (lambda () (send p take-turn (create-test-turn board ip available-shares available-hotels)))
   (checker board p)))

(: create-test-turn (Board Player Shares (Listof Hotel) -> (Instance Test-Turn%)))
(define (create-test-turn board ip available-shares available-hotels)
  (define c0 (*create-state board (list ip)))
  (define c1 (state-sub-shares c0 (shares-minus banker-shares0 available-shares)))
  (new test-turn% [current-state c1][available-hotels available-hotels]))

(define-type Test-Turn%
  (Class #:implements Turn-Player%
         (init-field [available-hotels (Listof Hotel)]
                     [current-state State])))

(: test-turn% Test-Turn%)
(define test-turn%
  (class turn% 
    (init-field available-hotels)
    (inherit-field current-state shares hotels)
    (super-new)
    
    (set! hotels available-hotels)
    
    ;; to simulate remote turn 
    (define/override (reconcile-shares t)
      (set! current-state (state-sub-shares current-state t))
      (set! shares (state-shares current-state))
      t)))

;; ---------------------------------------------------------------------------------------------------
;; testing 

#;
(module+ test 
  
  ;; Strategy Board Cash [Listof Tile] Shares [Listof Hotel] [Maybe Tile] [Maybe Hotel] Shares -> Any 
  (define (test S board0 cash t* available-shares available-hotels e-placement e-hotel e-shares)
    (define (checker board p)
      (define money cash)
      (lambda (placement hotel shares-to-buy)
        (check-equal? hotel e-hotel)
        (define FM (list FOUNDING MERGING))
        (define kind (if placement (what-kind-of-spot board0 placement) IMPOSSIBLE))
        (check-true (if (and hotel placement) (cons? (member kind FM)) #t))
        (define next-board 
          (if placement
              (set-board board placement kind hotel)
              board))
        (check-equal? placement e-placement)
        (check-equal? shares-to-buy e-shares)))
    (testx S board0 cash t* available-shares available-hotels checker))
  
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
  
  ;; --- testing bad player management 
  
  (define-syntax-rule 
    (test-reconcile p pa pb snd ... ordered-s)
    (let () ;; pa drops out between setup and inform (and also in turn)
      (define pb (*create-player "b" CASH0 (shares++ player-shares0 AMERICAN) (list A1)))
      (define pa (*create-player "a" CASH0 (shares++ player-shares0 AMERICAN) '()))
      (define p (create "b" ordered-s))
      (define t (create-test-turn (board) pb (shares++ player-shares0 AMERICAN) '()))
      snd ...      
      (define-values (_t _h o) (send p take-turn t))
      (check-equal? o '() "cannot buy stock because bad guy owns it")))  
  
  (test-reconcile p pa pb (send p setup (state0 pb pa)) (send p inform (state0 pb)) ordered-s)
  (test-reconcile p pa pb (send p setup (state0 pb pa)) ordered-s)
  (test-reconcile p pa pb (send p inform (state0 pb pa)) ordered-s)
  
  (test-reconcile p pa pb (send p setup (state0 pb pa)) (send p inform (state0 pb)) random-s)
  (test-reconcile p pa pb (send p setup (state0 pb pa)) random-s)
  (test-reconcile p pa pb (send p inform (state0 pb pa)) random-s)
  )
