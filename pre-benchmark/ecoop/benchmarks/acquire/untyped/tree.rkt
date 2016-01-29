#lang racket

;; ---------------------------------------------------------------------------------------------------
;; provides an abstract type Tree: generation plus inspection from admin and player perspective

;; ---------------------------------------------------------------------------------------------------
;; INTERFACE

(require "tree-intf.rkt" unstable/contract)

(tree& 
 tree? decision-tree? generate-tree
 tree-state tree-next
 tree-founding tree-merging)
(provide (struct-out hand-out))

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(require "board.rkt" "state.rkt" "basics.rkt" "Lib/auxiliaries.rkt")

(module+ test (require rackunit (submod "board.rkt" tiles+spots)))

;; from board: 
;; ACTION = {MERGING, FOUNDING, SINGELTON, GROWING, IMPOSSIBLE}

#| ---------------------------------------------------------------------------------------------------
   data definition

A TREE is: interpreter pattern: 

     ; represent tree of all decisions in an Acquire game
           +----------------------------------------------------------------------------------+
           |                                                                                  |
           v                                                                                  |
     +---------------+  implements     +---------------+                                      |
     | tree<%>       |--------<--------| atree%        |                                      |
     +---------------+                 +---------------+                                      |
     +---------------+                 | state         |                                      |
     | to-state      |                 +---------------+                                      |
     | next          |                 | to-state      |                                      |
     | founding      |                 | founding      |                                      |
     | merging       |                 | merging       |                                      |
     +---------------+                 | traversal   a |                                      |
                                       | lookup-tile a |                                      |
                                       +---------------+                                      |
                                              |                                               |
                                              ^ extension                                     |
                                              |                                               |
                                              |                                               |
       +--------------------------------------+---------------+                               |
       |                                                      |                               |
     ; represent leaf                                  ; represent list of decision branches  |
     ;  no decisions possible                          ;   for placing tiles and hotels       |
     ; adapter: interface for state                                                           |
     +---------------+                                 +-----------------+                    |
     | state%        |                                 | lplaced%        |                    |
     +---------------+                                 +-----------------+                    |
     +---------------+                                 | placed          |*----+              |
     | traversal     |                                 +-----------------+     |              |
     | lookup-tile   |                                 | traversal       |     |              |
     |               |                                 | lookup-tile     |     |              |
     +---------------+                                 | lookup-purchase |     |              |
                                                       +-----------------+     |              |
                                                                               |              |
                                                                               |              |
                                                                               v              |
                                             ; represent decision branch                      |
                                             ;   place tile with hotel                        |
                                             +--------------------------------------------+   |
                                             | placed%                                    |   |
                                             +--------------------------------------------+   |
                                             | state                                      |   |
                                             | tile                                       |   |
                                             | hotel                                      |   |
                                             | cached: state/tile                         |   |
                                             | cached: reason                             |   |
                                             +--------------------------------------------+   |
                                             | purchase: Decisions ShareOrder -> HandOut* |   |
                                             | to-tree : Decisions ShareOrder -> TREE*    |---+
                                             | acceptable-policies                        |
                                             +--------------------------------------------+

HandOut: 

[hand-out 
        |
        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+ ...
        |                            ...                        |
        |                                                       |
    admin's tile handed to player, tree<%> ]
]

;; Decisions = [listof (list/c player (listof (list/c hotel boolean]))]
;; does the player keep the specified hotel? Absence means yes

|#

(struct hand-out (tile tree))
;; HandOut = (hand-out t st)
;; denotes that player received tile t and st is the Tree generated from the resulting state


(define tree<%> 
  (interface () 
    ;; map THIS tree to a game state
    to-state ;; -> State
    
    ;; navigate from THIS tree to the next tree
    ;; given tile placement, optional hotel, and shares to buy plus a way to select the hand out tile
    next ;; Tile [Maybe Hotel] [Listof Hotel] [ [Listof Tile] -> Tile ] ->* Tile Tree
    
    ;; how many founding transitions are in THIS tree up to depth n
    founding ;; Nat [Listof ShareOrder] -> Nat
    
    ;; how many merging transitions are in THIS tree up to depth n
    merging ;; Nat [Listof ShareOrder] -> Nat 
    ))

(define atree% 
  (class* object% (tree<%>)
    (init-field state)
    
    (super-new)

    (define/public (decision-tree?) (error "abstract method"))

    (define/public (to-state) 
      state)
    
    (abstract next)
    
    ;; template hook pattern: template
    (define/public (founding n order-policies)
      (traversal n order-policies (is-action FOUNDING)))
    
    ;; template hook pattern: template
    (define/public (merging n order-policies)
      (traversal n order-policies (is-action MERGING)))
    
    ;; hook
    ;; how many transitions in THIS tree (up to depth n) satisfy the given predicate 
    ;; Nat [Listof ShareOrder] [Placed -> Nat] -> Nat   
    (abstract traversal)
    
    ;; private field: ACTION -> Placed -> {0,1}
    ;; is the alternative a merging action? 
    (define ((is-action tag) p)
      (if (and (placed-hotel p) (eq? (get-field reason p) tag)) 1 0))
    
    ;; use pick-tile to hand out a tile; extract the corresponding subtree 
    ;; [[Listof Tile] -> Tile] [Listof HandOut] ->* [Maybe Tile] Tree 
    (abstract lookup-tile)))

(define state% 
  (class* atree% (tree<%>)
    (super-new)

    (define/override (decision-tree?) #f)

    (define/override (next . _) 
      (error 'tree-next "finale state can't transition"))
    
    (define/override (traversal n policies p?) 0)
    
    (define/override (lookup-tile pick-tile lo-handout)
      (values #f this))))

(define lplaced%
  (class* atree% (tree<%>)
    (super-new)
    (init-field lplaced)

    (define/override (decision-tree?) (cons? lplaced))

    (define/override (next tile hotel decisions shares-to-buy pick-tile) 
      (define intermediate (send (lookup-purchase tile hotel) purchase decisions shares-to-buy))
      (send this lookup-tile pick-tile intermediate)
      #;(lookup-tile pick-tile intermediate))
    
    
    ;; Tile [Maybe Hotel] -> Placed 
    ;; lookup the one and only Placed from lo-placed that represents the action of placing t (& h)
    (define/private (lookup-purchase t h)
      (for/first ((p lplaced) #:when (and (equal? (placed-hotel p) h) (equal? (placed-tile p) t)))
        p))
    
    (define/override (lookup-tile pick-tile lo-hand-out)
      (define tile (pick-tile (map hand-out-tile lo-hand-out)))
      (define st (for/first ((p lo-hand-out) #:when (equal? (hand-out-tile p) tile)) (hand-out-tree p)))
      (values tile st))
    
    (define/override (traversal n policies p?)
      (if (= n 0)
          0
          (for/sum ((branch lplaced))
            (define d* (map (lambda (p) (list p '())) (state-players (get-field state/tile branch))))
            (define a (send branch acceptable-policies policies))
            (+ (p? branch)
               ;; do not inspect every subtree because share buying does not affect actions
               (if (empty? a)
                   0
                   (* (length a) 
                      (for/sum ((st (send branch to-trees d* (first a))))
                        (send st traversal (- n 1) policies p?))))))))))

(define placed%
  (class object%
    (init-field state tile hotel state/tile reason)
    (super-new)
    
    ;; Decisions ShareOrder -> state% or [Listof HandOut]
    ;; given merger decisions and a purchase order, generate the next stage from THIS decision point
    (define/public (purchase decisions share-order)
      ;; ---------------------------------------------------------------------------------------------
      ;; contract checking 
      (when (eq? MERGING reason)
        (define players (state-players state/tile))
        (unless (= (length decisions) (length players))
          (printf "contract failure: received wrong number of decisions")
          (pretty-print players)
          (pretty-print (map first decisions))
          (error 'purchase "done")))
      ;; ---------------------------------------------------------------------------------------------
      (define state/decisions 
        (if (eq? MERGING reason)
            (state-return-shares state/tile decisions (state-board state))
            state/tile))
      (define state/bought (state-buy-shares state/decisions share-order))
      (define available-tiles (state-tiles state/bought))
      (if (empty? available-tiles) 
          (new state% [state state/bought])
          (for/list ((tile available-tiles))
            (hand-out tile (generate-tree (state-next-turn (state-move-tile state/bought tile)))))))
    
    ;; Decisions ShareOrder -> [Listof Tree]
    ;; given a purchase order, generate list of trees from THIS decision point's purchases 
    (define/public (to-trees decisions share-order)
      (define state-or-hand-out (purchase decisions share-order))
      (cond
        [(cons? state-or-hand-out) (map hand-out-tree state-or-hand-out)]
        [else state-or-hand-out]))
    
    ;; [Listof ShareOrder] -> [Listof ShareOrder]
    ;; filter out those share orders that are acceptable given THIS decision point's state 
    (define/public (acceptable-policies policies)
      (define state state/tile)
      (define budget (player-money (state-current-player state)))
      (define board  (state-board state))
      (define shares (state-shares state))
      (for/list ((p policies) 
                 #:when (and (shares-available? shares p) (affordable? board p budget)))
        p))))

(define (placed-tile p) (get-field tile p))
(define (placed-hotel p) (get-field hotel p))

;; ---------------------------------------------------------------------------------------------------
;; tree generation 

(define (generate-tree state)
  (cond
    [(state-final? state) (new state% [state state])]
    [else (define board (state-board state))
          (define available-hotels (state-hotels state))
          (define lplaced
            (for/fold ((lo-placed '())) ((t (player-tiles (state-current-player state))))
              (define kind (what-kind-of-spot board t))
              (define hotels 
                (cond
                  [(eq? kind IMPOSSIBLE) '()]
                  [(and (eq? FOUNDING kind) (cons? available-hotels)) available-hotels]
                  [(eq? MERGING kind)
                   (define-values (acquirers _) (merging-which board t))
                   acquirers]
                  [else (list #f)]))
              (define new-placements
                (for/list ((h hotels))
                  (define state/tile 
                    (if h (state-place-tile state t h) (state-place-tile state t)))
                  (new placed% [state state][tile t][hotel h][state/tile state/tile][reason kind])))
              (append new-placements lo-placed)))
          (new lplaced% (state state) (lplaced lplaced))]))

(module+ test 
  
  (define (make-decisions players)
    (map (lambda (p) (list p '())) players))
  
  (define Alist (list A1 A2 A3 A4 A5 A6))
  (define p1 (*create-player "a" 1000 player-shares0 Alist))
  (define p2 (*create-player "b" 1000 player-shares0 (list B1 B2 B3 B4 B5 B6)))
  (define p3 (*create-player "c" 1000 player-shares0 (list C1 C2 C3 C4 C5 C6)))
  (define p4 (*create-player "d" 1000 player-shares0 (list D1 D2 D3 D4 D5 D6)))
  (define p5 (*create-player "e" 1000 player-shares0 (list E1 E2 E3 E4 E5 E6)))
  (define p6 (*create-player "f" 1000 player-shares0 (list F1 F2 F3 F4 F5 F6)))
  
  ;; create a final state: it is a tree 
  (let ()
    (define safe-hotel `((,TOWER ,@(build-list SAFE# (lambda (i) (ctile H  (+ i 1)))))))
    (define safe-board (*create-board-with-hotels '() safe-hotel))
    (define safe-state (*create-state safe-board (list p1 p2 p3)))
    (check-equal? (tree-state (generate-tree safe-state)) safe-state))
  
  ;; create a founding state: player can found all hotels 
  (let ()
    (define player-can-place-c5 (*create-player "a" 1000 player-shares0 (list C5)))
    (define board-found-at-c5 board-a1-b2-c6)
    (define state-found-at-c5 (*create-state board-found-at-c5 (list player-can-place-c5)))
    (define tree-foundings-at-c5 (generate-tree state-found-at-c5))
    
    (check-equal? (apply set (map placed-hotel (get-field lplaced tree-foundings-at-c5)))
                  (apply set ALL-HOTELS)
                  "found all possible hotels"))
  
  ;; create a merging state: board with American and Tower can acquire WorldWide
  (let ()
    (define shares-for-am-tw-ww 
      (*combine-shares
       (list (*create-shares AMERICAN 3) (*create-shares TOWER 3) (*create-shares WORLDWIDE 3))))
    (define player-can-place-d3 (*create-player "a" 1000 shares-for-am-tw-ww (list D3)))
    (define state-merge-at-d3 (*create-state board-3way-merger-at-d3 (list player-can-place-d3)))
    (define tree-merging-at-d3 (generate-tree state-merge-at-d3))
    (define branches (get-field lplaced tree-merging-at-d3))
    
    (check-equal? (apply set (map placed-hotel branches)) (set TOWER AMERICAN) 
                  "Tower and American can acquire WorldWide")
    
    ;; now let's check whether the money is correct 
    (define (check-money placed)
      (define (other h)
        (cond
          [(equal? AMERICAN h) (list TOWER WORLDWIDE)]
          [(equal? TOWER h)    (list AMERICAN WORLDWIDE)]
          [else (error 'other "can't happen: ~e" h)]))
      (define acquired (other (placed-hotel placed)))
      (define decisions (map (lambda (h) (list h #f)) acquired))
      (define trees  (send placed to-trees `((,player-can-place-d3 ,decisions)) '()))
      (define tree1  (randomly-pick trees)) ;; <-- replace with fixed branch if buggy
      (define state1 (get-field state tree1))

      (check-equal? (player-money (first (state-players state1)))
                    (+ (player-money player-can-place-d3)
                       (for/sum ((h acquired))
                         (+ (bonus 'majority h (size-of-hotel board-3way-merger-at-d3 h))
                            (* (shares-available shares-for-am-tw-ww h)
                               (price-per-share h (size-of-hotel board-3way-merger-at-d3 h))))))))

    (check-money (first branches))
    (check-money (second branches)))
  
  ;; create a state whose list of available tiles is empty 
  (let ()
    (define p1-p6 (list p1 p2 p3 p4 p5 p6))
    (define all-remaining-tiles (remove* (apply append (map player-tiles p1-p6)) ALL-TILES))
    (define board-with-remaining-tiles (*create-board-with-hotels all-remaining-tiles '()))
    (define state-without-admin-tiles (*create-state board-with-remaining-tiles p1-p6))
    (define tree-without-admin-tiles (generate-tree state-without-admin-tiles))
    
    (check-equal? (apply set (map placed-tile (get-field lplaced tree-without-admin-tiles)))
                  (apply set Alist) 
                  "placed list uses all the player's tiles")
    
    (for ((p (get-field lplaced tree-without-admin-tiles)))
      (define s (send p to-trees (make-decisions p1-p6) '()))
      (check-true (is-a? s state%) "the resulting subtree is awlays a state"))))

;; ---------------------------------------------------------------------------------------------------
;; tree navigation: game administrator side 

;; ASSUME: current player has enough money to buy the desired shares 
(define (tree-next current-tree tile hotel decisions shares-to-buy pick-tile)
  (send current-tree next tile hotel decisions shares-to-buy pick-tile))

(define (tree? x)
  (or (is-a? x state%) (is-a? x lplaced%)))

(define (decision-tree? x)
  (send x decision-tree?))

(define (tree-state t)
  (send t to-state))

(module+ test
  (define (smallest-tile lot)
    ;; for speeding up tests; exploit local knowledge 
    (first lot)
    #;
    (first (sort lot tile<=?)))
  
  ;; create a final state: it is returned as is 
  (let ()
    (define safe-hotel `((,TOWER ,@(build-list SAFE# (lambda (i) (ctile H  (+ i 1)))))))
    (define safe-board (*create-board-with-hotels '() safe-hotel))
    (define safe-state (*create-state safe-board (list p1 p2 p3)))
    
    (check-exn exn:fail? 
               (lambda () (tree-next (generate-tree safe-state) I12 #f '() '() randomly-pick))))
  
  ;; create one founding state with exactly one choice 
  (let ()
    (define player-can-place-c5 (*create-player "a" 1000 player-shares0 (list C5)))
    (define board-with-six-hotels-found-at-c5
      (*create-board-with-hotels
       (list A1 B2 C6)
       `((,AMERICAN ,C1 ,D1)
         (,CONTINENTAL ,C10 ,D10)
         (,FESTIVAL ,F7 ,G7)
         (,IMPERIAL ,F4 ,G4)
         (,SACKSON ,H11 ,H12)
         (,TOWER ,I1 ,I2))))
    (define p (list player-can-place-c5))
    (define t (generate-tree (*create-state board-with-six-hotels-found-at-c5 p)))
    (define-values (_ lo-decisions) (tree-next t C5 WORLDWIDE (make-decisions p) '() smallest-tile))
    
    (check-equal? (length (get-field lplaced lo-decisions)) 1))
  
  ;; create one founding state, then the player has only singleton choices
  (let ()
    (define player-can-place-3 (list (*create-player "a" 1000 player-shares0 (list A2 B1 C5))))
    (define board-found-at-3 board-a1-b2-c6)
    (define state-found-at-3 (*create-state board-found-at-3 player-can-place-3))
    (define tree-foundings-at-3 (generate-tree state-found-at-3))
    
    (define-values (_ lo-placed1) 
      (tree-next tree-foundings-at-3 C5 AMERICAN (make-decisions player-can-place-3) '() smallest-tile))
    (define lo-placed (get-field lplaced lo-placed1))
    ;; can now found six additional hotels, twice, plus place a singleton with smallest-tile obtained
    (check-equal? (length lo-placed) (+ (* 2 (length (state-hotels (tree-state lo-placed1)))) 1)))
  
  ;; merging state, then player has one choice 
  (let ()
    (define shares-for-am-tw-ww 
      (list (*create-shares AMERICAN 3) (*create-shares TOWER 3) (*create-shares WORLDWIDE 3)))
    (define player-can-place-d3 
      (list (*create-player "a" 1000 (*combine-shares shares-for-am-tw-ww) (list D3))))
    (define state-merge-at-d3 
      (*create-state board-3way-merger-at-d3 player-can-place-d3))
    (define tree-merging-at-d3 (generate-tree state-merge-at-d3))
    
    (define d* (make-decisions player-can-place-d3))
    (define-values (_1 lop1) (tree-next tree-merging-at-d3 D3 TOWER d* '() smallest-tile))
    (check-equal? (length (get-field lplaced lop1)) 1)
    (define-values (_2 lop2) (tree-next tree-merging-at-d3 D3 AMERICAN d* '() smallest-tile))
    (check-equal? (length (get-field lplaced lop2)) 1)))

;; ---------------------------------------------------------------------------------------------------
;; tree navigation: player side 

(define (tree-founding current-tree n order-policies)
  (send current-tree founding n order-policies))

(define (tree-merging current-tree n order-policies)
  (send current-tree merging n order-policies))

(module+ test
  (define policy ;; [Listof ShareOrder]
    `((,AMERICAN ,AMERICAN)
      (,TOWER ,TOWER )))
  
  ;; a final state
  (let ()
    (define safe-hotel `((,TOWER ,@(build-list SAFE# (lambda (i) (ctile H  (+ i 1)))))))
    (define safe-board (*create-board-with-hotels '() safe-hotel))
    (define safe-state (*create-state safe-board (list p1 p2 p3)))
    
    (check-equal? (tree-founding (generate-tree safe-state) 1 policy) 0))
  
  ;; one founding state
  (let ()
    (define player-can-place-3 (*create-player "a" 1000 player-shares0 (list A2 B1 C5)))
    (define board-found-at-3 board-a1-b2-c6)
    (define state-found-at-3 (*create-state board-found-at-3 (list player-can-place-3)))
    (define tree-foundings-at-3 (generate-tree state-found-at-3))
    
    ;; three opportunities to found hotels, all hotels available are usable in each case 
    (check-equal? (tree-founding tree-foundings-at-3 1 policy) 
                  (* 3 (length (state-hotels state-found-at-3)))))
  
  ;; create one founding states, in any number of steps 
  (let ()
    (define pc5 (*create-player "a" 1000 player-shares0 (list C5)))
    (define bff
      (*create-board-with-hotels
       (list A1 B2 C6)
       `((,AMERICAN ,C1 ,D1)
         (,CONTINENTAL ,C10 ,D10)
         (,FESTIVAL ,F7 ,G7)
         (,IMPERIAL ,F4 ,G4)
         (,SACKSON ,H11 ,H12)
         (,TOWER ,I1 ,I2))))
    (define sff (*create-state bff (list pc5)))
    
    (define tree (generate-tree sff))
    
    (check-equal? (tree-founding tree 1 policy) 1)
    (check-equal? (tree-founding tree 2 policy) 1)
    (check-equal? (tree-founding tree 3 policy) 1))
  
  ;; a merging state 
  (let ()
    (define shares-for-am-tw-ww 
      (list (*create-shares AMERICAN 3) (*create-shares TOWER 3) (*create-shares WORLDWIDE 3)))
    (define player-can-place-d3 
      (*create-player "a" 1000 (*combine-shares shares-for-am-tw-ww) (list D3)))
    (define state-merge-at-d3 
      (*create-state board-3way-merger-at-d3 (list player-can-place-d3)))
    (define mmm (generate-tree state-merge-at-d3))
    
    (check-equal? (tree-merging mmm 1 policy) 2)
    (check-equal? (tree-merging mmm 2 policy) 2)))
