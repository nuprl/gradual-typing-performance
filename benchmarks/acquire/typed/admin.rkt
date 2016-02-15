#lang typed/racket

;; ---------------------------------------------------------------------------------------------------
;; the Acquire game administrator class: sign up players and manage their play 

(provide
 Administrator%
 administrator%
 Turn%
 turn%
)

;; ---------------------------------------------------------------------------------------------------

(require
 benchmark-util
 "../base/types.rkt"
 "board-adapted.rkt"
 "state-adapted.rkt"
)
(require/typed racket/sandbox
  (call-with-limits (All (A) (-> Natural Natural (-> A) A)))
  (exn:fail:resource? (All (A) (-> A Boolean)))
)
(require/typed/check "basics.rkt"
  (ALL-HOTELS (Listof Hotel))
  (hotel? (-> Any Boolean))
  (shares-available? (-> Shares (Listof Hotel) Boolean))
  (shares? (-> Any Boolean))
  (shares-order? (-> Any Boolean))
)

;; =============================================================================

(define-type Tree<%>
  (Class
   (to-state (-> State))
   (next (-> Tile (Option Hotel) Decisions (Listof Hotel) (-> (Listof Tile) Tile) (Values (Option Tile) (Instance ATree%))))
   (founding (-> Natural (Listof (Listof Hotel)) Natural))
   ;; Precondition: share-order
   (traversal (-> Natural (Listof (Listof Hotel)) (-> (Instance Placed%) Natural) Natural))
   (lookup-tile (-> (-> (Listof Tile) Tile) (Listof HandOut) (Values (Option Tile) (Instance ATree%))))
   (merging (-> Natural (Listof (Listof Hotel)) Natural))))

(define-type ATree%
  (Class
   #:implements Tree<%>
   (init-field (state State))))

;(define-type Tree (Instance ATree%))

(define-type Placed%
  (Class
   (init-field
    (state State)
    (tile Tile)
    (hotel (Option Hotel))
    (state/tile State)
    (reason SpotType))
   (purchase (-> Decisions (Listof Hotel) (U (Instance ATree%) (Listof HandOut))))
   ;; Precondition: share-order?
   (to-trees (-> Decisions (Listof Hotel) (Listof (Instance ATree%))))
   ;; Precondition: share-order?
   (acceptable-policies (-> (Listof (Listof Hotel)) (Listof (Listof Hotel))))))

(define-type LPlaced%
  (Class
   #:implements ATree%
   (init-field
    (lplaced (Listof (Instance Placed%)))
    (state State))
   (nothing-to-place? (-> Boolean))
   ))

;; -----------------------------------------------------------------------------
;; from sandbox.rkt

(: in-sandbox (All (A B)
                   (->* ((-> A)
                         (-> A B)
                         (-> Any B))
                        (#:time Natural
                         #:memory Natural)
                        B)))
(define (in-sandbox producer consumer failure #:time (sec-limit 1) #:memory (mb-limit 30))
  ((let/ec fail : (-> B)
           (let ([a : A
                    (with-handlers
                      ((exn:fail:resource?
                        (lambda ([x : Any])
                          (fail
                           (lambda () (failure 'R)))));`(R ,(exn-message x)))))))
                       (exn:fail:out-of-memory?
                        (lambda ([x : Any])
                          (fail
                           (lambda () (failure 'R)))));`(R ,(exn-message x)))))))
                       (exn:fail?
                        (lambda ([x : Any])
                          (fail
                           (lambda () (failure 'X)))))) ;`(X ,(exn-message x))))))))
                      ((inst call-with-limits A) sec-limit mb-limit producer))])
             (lambda () (consumer a))))))

(struct hand-out (
  [tile : Tile]
  [tree : (Instance ATree%)]))

(define-type HandOut hand-out)
;; HandOut = (hand-out t st)
;; denotes that player received tile t and st is the Tree generated from the resulting state

;; -----------------------------------------------------------------------------
;; from tree.rkt

(: placed-tile (-> (Instance Placed%) Tile))
(define (placed-tile p)
  (get-field tile p))

(: placed-hotel (-> (Instance Placed%) (Option Hotel)))
(define (placed-hotel p)
  (get-field hotel p))

;; ;(: tree<%> Tree<%>)
;; (define tree<%> 
;;   (interface () 
;;     ;; map THIS tree to a game state
;;     to-state ;; -> State
    
;;     ;; navigate from THIS tree to the next tree
;;     ;; given tile placement, optional hotel, and shares to buy plus a way to select the hand out tile
;;     next ;; Tile [Maybe Hotel] [Listof Hotel] [ [Listof Tile] -> Tile ] ->* Tile Tree
    
;;     ;; how many founding transitions are in THIS tree up to depth n
;;     founding ;; Nat [Listof ShareOrder] -> Nat
    
;;     ;; how many merging transitions are in THIS tree up to depth n
;;     merging ;; Nat [Listof ShareOrder] -> Nat 
;;     ))

(: atree% ATree%)
(define atree% 
  (class object% ;(tree<%>)
    (init-field [state : State])
    
    (super-new)
    
    (define/public (to-state) 
      (get-field state this))
    
    ;(abstract next)
    (define/public (next t h d* h* pt)
      (error 'not-implemented))
    
    ;; template hook pattern: template
    (define/public (founding n order-policies)
      (unless (shares-order? order-policies)
        (error 'atree-founding "Precondition"))
      (traversal n order-policies (is-action FOUNDING)))
    
    ;; template hook pattern: template
    (define/public (merging n order-policies)
      (traversal n order-policies (is-action MERGING)))
    
    ;; hook
    ;; how many transitions in THIS tree (up to depth n) satisfy the given predicate 
    ;; Nat [Listof ShareOrder] [Placed -> Nat] -> Nat   
    (define/public (traversal n order-policies i)
      (error 'not-impolementd))
    
    ;; private field: ACTION -> Placed -> {0,1}
    ;; is the alternative a merging action?
    (: is-action (-> Symbol (-> (Instance Placed%) (U 1 0))))
    (define ((is-action tag) p)
      (if (and (placed-hotel p) (eq? (get-field reason p) tag)) 1 0))
    
    ;; use pick-tile to hand out a tile; extract the corresponding subtree 
    ;; [[Listof Tile] -> Tile] [Listof HandOut] ->* [Maybe Tile] Tree 
    (define/public (lookup-tile pick-tile lo-handout)
      (values #f this))
))

(: state% ATree%)
(define state% 
  (class atree% ;(tree<%>)
    (super-new)
    
    (define/override (next . _) 
      (error 'tree-next "finale state can't transition"))
    
    (define/override (traversal n policies p?) 0)
    
    (define/override (lookup-tile pick-tile lo-handout)
      (values #f this))))

(: lplaced% LPlaced%)
(define lplaced%
  (class atree% ;(tree<%>)
    (super-new)
    (init-field
     (lplaced : (Listof (Instance Placed%))))

    (define/public (nothing-to-place?)
      (null? lplaced))
    
    (define/override (next tile hotel decisions shares-to-buy pick-tile) 
      (define intermediate (send (lookup-purchase tile hotel) purchase decisions shares-to-buy))
      (unless (list? intermediate) (error "Expected a HandOut, got a State%"))
      (send this lookup-tile pick-tile intermediate))
    
    ;; Tile [Maybe Hotel] -> Placed 
    ;; lookup the one and only Placed from lo-placed that represents the action of placing t (& h)
    (: lookup-purchase (-> Tile (Option Hotel) (Instance Placed%)))
    (define/private (lookup-purchase t h)
      (or 
       (for/or : (Option (Instance Placed%))
               ((p : (Instance Placed%) lplaced)
                #:when (and (equal? (placed-hotel p) h)
                            (equal? (placed-tile p) t)))
         p)
       (error 'noplace)))
    
    (define/override (lookup-tile pick-tile lo-hand-out)
      (define tile (pick-tile (map hand-out-tile lo-hand-out)))
      (define st (for/or : (Option (Instance ATree%))
                         ((p : HandOut lo-hand-out)
                          #:when (equal? (hand-out-tile p) tile))
                   (hand-out-tree p)))
      (values tile (or st (error 'lookupfailed))))
    
    (define/override (traversal n policies p?)
      (if (= n 0)
          0
          (for/sum : Natural
                   ((branch : (Instance Placed%) (in-list lplaced)))
            (define d* (map (lambda ([p : Player])
                              (list p '()))
                            (state-players (get-field state/tile branch))))
            (define a (send branch acceptable-policies policies))
            (+ (p? branch)
               ;; do not inspect every subtree because share buying does not affect actions
               (if (empty? a)
                   0
                   (* (length a) 
                      (for/sum : Natural
                               ((st : (Instance ATree%) (send branch to-trees d* (first a))))
                        (send st traversal (- n 1) policies p?))))))))))

(: placed% Placed%)   
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
          ;; (pretty-print players)
          ;; (pretty-print (map first decisions))
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
          (for/list : (Listof HandOut)
                    ((tile : Tile available-tiles))
            (hand-out tile (generate-tree (state-next-turn (state-move-tile state/bought tile)))))))
    
    ;; Decisions ShareOrder -> [Listof Tree]
    ;; given a purchase order, generate list of trees from THIS decision point's purchases 
    (define/public (to-trees decisions share-order)
      (define state-or-hand-out (purchase decisions share-order))
      (cond
        [(list? state-or-hand-out) (map hand-out-tree state-or-hand-out)]
        [else (list state-or-hand-out)]))
    
    ;; [Listof ShareOrder] -> [Listof ShareOrder]
    ;; filter out those share orders that are acceptable given THIS decision point's state 
    (define/public (acceptable-policies policies)
      (unless (andmap shares-order? policies)
        (error 'acceptable-policies "Precondigion"))
      (define state state/tile)
      (define budget (player-money (state-current-player state)))
      (define board  (state-board state))
      (define shares (state-shares state))
      (for/list ((p : (Listof Hotel) policies)
                 #:when (and (shares-available? shares p)
                             (affordable? board p budget)))
        p))))

(: generate-tree (-> State (Instance ATree%)))
(define (generate-tree state)
  (cond
    [(state-final? state)
     (new state% [state state])]
    [else (define board (state-board state))
          (define available-hotels (state-hotels state))
          (define lplaced
            (for/fold : (Listof (Instance Placed%))
                      ((lo-placed : (Listof (Instance Placed%)) '()))
                      ((t : Tile (player-tiles (state-current-player state))))
              (define kind (what-kind-of-spot board t))
              (: hotels (Listof (Option Hotel)))
              (define hotels 
                (cond
                  [(eq? kind IMPOSSIBLE) '()]
                  [(and (eq? FOUNDING kind) (cons? available-hotels)) available-hotels]
                  [(eq? MERGING kind)
                   (define-values (acquirers _) (merging-which board t))
                   acquirers]
                  [else (list #f)]))
              (define new-placements
                (for/list : (Listof (Instance Placed%))
                          ((h : (Option Hotel) (in-list hotels)))
                  (define state/tile 
                    (if h (state-place-tile state t h) (state-place-tile state t)))
                  (new placed% [state state][tile t][hotel h][state/tile state/tile][reason kind])))
              (append new-placements lo-placed)))
          (new lplaced% (state state) (lplaced lplaced))]))

;; ASSUME: current player has enough money to buy the desired shares
(: tree-next (-> (Instance ATree%) Tile Hotel Decisions (Listof Hotel) (-> (Listof Tile) Tile) (Values (Option Tile) (Instance ATree%))))
(define (tree-next current-tree tile hotel decisions shares-to-buy pick-tile)
  (send current-tree next tile hotel decisions shares-to-buy pick-tile))

(: tree-state (-> (Instance ATree%) State))
(define (tree-state t)
  (send t to-state))

;; -----------------------------------------------------------------------------

(: log (-> Any Any Void))
(define (log status msg)
  (void))
  ;; (display "[LOG:")
  ;; (display status)
  ;; (display "] ")
  ;; (displayln msg))

(: administrator% Administrator%)
(define administrator%
  (class object% 
    (init-field next-tile)
    (super-new)

    (: *count Natural)
    (define *count 10)
    (: *named-players (Listof (List String (Instance Player%))))
    (define *named-players '())
    
    ;; effect: keep track of players 
    (define/public (sign-up name player)
      (define pre (if (< (random 100) 50) "player" "spieler"))
      (set! *count (+ *count 1 (random 10)))
      (set! name (format "~a~a:~a" pre (number->string *count) name))
      (set! *named-players (cons (list name player) *named-players))
      name)
    
    (define/public (show-players)
      (for/list : (Listof String)
                ([n+p (in-list *named-players)])
        (car n+p)))
    
    (define/public (run turns# #:show (show void))
      (define players (player->internals))
      (when (empty? players) (error 'run "players failed to sign up properly"))
      (define tree0 (generate-tree (setup-all players (apply state0 players))))
      ;; generative recursion: n to 0, but terminate before if final state is reached
      ;; accumulator: states is reverese list of states encountered since tile distribution 
      (let loop : RunResult
           ([tree   : (Instance ATree%) tree0]
            [n      : Integer turns#]
            [states : (Listof State) (list (tree-state tree0))])
        (define state (tree-state tree))
        (cond
          [(= n 0)
           (list DONE (state-score state) (reverse states))]
          [(empty? (state-players state))
           (list EXHAUSTED (state-score state) (reverse states))]
          ;; [(not (decision-tree? tree))
          [(and (is-a? tree lplaced%)
                (send (cast tree (Instance LPlaced%)) nothing-to-place?))
           (finish state)
           (list SCORE (state-score state) (reverse states))]
          [else 
           (define external (or (player-external (state-current-player state)) (error 'external/fail)))
           ;; (show n state)
           (define turn (new turn% [current-state state]))
           ((inst in-sandbox (List (Option Tile) (Option Hotel) (Listof Hotel)) RunResult)
            #:time (* (+ (length (state-players state)) 1) 3)
            (lambda ()
              (let-values ([(a b c) (send external take-turn turn)])
                (list a b c)))
            ;; success
            (lambda ([arg : (List (Option Tile) (Option Hotel) (Listof Hotel))])
              (define-values (tile hotel-involved buy-shares)
                (values (car arg) (cadr arg) (caddr arg)))
                ;; (let ([arg+ (cast arg* (List Tile Hotel (-> (Instance ATree%) State Any)))])
                ;;   (values (car arg+) (cadr arg+) (caddr arg+))))
              (cond
                [(boolean? tile) 
                 (finish state)
                 (list IMPOSSIBLE (state-score state) (reverse (cons state states)))]
                [(not hotel-involved)
                 (error "bad hotel")]
                [else 
                 (define merger? (eq? (what-kind-of-spot (state-board state) tile) MERGING))
                 (cond
                   ;; -------------------------------------------------------------------------------
                   ;; temporal contract: 
                   [(and merger? (not (send turn place-called)))
                    (loop (generate-tree (state-remove-current-player state)) (- n 1) states)]
                   ;; -------------------------------------------------------------------------------
                   [else 
                    (define-values (t1 h1 d*)
                      (if merger? (send turn decisions) (values #f #f '())))
                    ;; assert: if merging?:
                    (and (equal? tile t1) (equal? hotel-involved h1))
                    (define eliminate (send turn eliminated))
                    (cond
                      [(member (state-current-player state) eliminate)
                       ((failure state states (lambda ([s : (Instance ATree%)]) (loop s  (- n 1) states)))
                        `(S  "current player failed on keep"))]
                      [else 
                       (define tree/eliminate 
                         (if (empty? eliminate)
                             tree
                             (generate-tree (state-eliminate state eliminate))))
                       (exec external tree/eliminate tile hotel-involved d* buy-shares
                             (lambda ([next-tree : (Instance ATree%)]
                                      [state : State])
                               (inform-all 
                                next-tree state
                                (lambda ([next-tree : (Instance ATree%)]
                                         [state : State])
                                  (cond
                                    [(empty? (state-tiles state))
                                     (finish state)
                                     (list EXHAUSTED (state-score state) (reverse states))]
                                    [else
                                     (loop next-tree (- n 1) (cons state states))]))))
                             ;; failure 
                             (failure state states (lambda ([s : (Instance ATree%)]) (loop s  (- n 1) states))))])])]))
            ;; failure: 
            (failure state states (lambda ([s : (Instance ATree%)]) (loop s  (- n 1) states))))
           ])))

    ;; (-> State
    ;;     (Listof State)
    ;;     (-> State RunResult)
    ;;     (-> State RunResult))
    (: failure (-> State (Listof State) (-> (Instance ATree%) RunResult) (-> Any RunResult)))
    (define/private ((failure state states continue) status)
      ;; this should be a logging action
      (log status `(turn failure  ,(player-name (state-current-player state))))
      (define state/eliminate (state-remove-current-player state))
      (if (empty? (state-players state/eliminate))
          (list EXHAUSTED '(all players failed) (reverse states))
          (continue (generate-tree state/eliminate))))
          
    ;; [ (cons Tile [Listof Tile]) -> Tile ] -> Tree Tile [Maybe Hotel] Decisions [Listof Hotel]
    ;; (Any -> Any) -- success continuation 
    ;; (Any -> Any) -- failure continuation 
    ;; -> (Instance ATree%)
    (: exec (-> (Instance Player%) (Instance ATree%) Tile Hotel Decisions (Listof Hotel) (-> (Instance ATree%) State RunResult) (-> Any RunResult) RunResult))
    (define/private (exec external tree0 placement hotel decisions shares-to-buy succ fail)
      (define-values (tile tree)
        (tree-next tree0 placement hotel decisions shares-to-buy next-tile))
      (unless tile (error 'no-tile))
      ((inst in-sandbox Void RunResult)
       (lambda ()
         (send external receive-tile tile))
       (lambda (_void)
         (succ tree (tree-state tree)))
       fail))
    
    ;; State (Tree State -> Any)  -> Any
    (: inform-all (All (A) (-> (Instance ATree%) State (-> (Instance ATree%) State A) A)))
    (define/private (inform-all tree state k)
      (define eliminate
        (for/fold : (Listof Player)
                  ((throw-out : (Listof Player) '()))
                  ((p : Player (state-players state)))
          ((inst in-sandbox Void (Listof Player))
           (lambda arg*
             ;;bg; seems to return Void in original
             (let ([pe (or (player-external p) (error 'external-fail))])
               (send pe inform state)))
           (lambda (_void)
             throw-out)
           (lambda arg*
             (log (car arg*) `(inform ,(player-name p)))
             (cons p throw-out)))))
      (cond
        [(empty? eliminate) (k tree state)]
        [else (define state/eliminate (state-eliminate state eliminate))
              (k (generate-tree state/eliminate) state/eliminate)]))
    
    ;; -> [Listof InternalPlayer]
    ;; create internal players for each external player
    ;;bg should these be player objects?
    (: player->internals (-> (Listof Player)))
    (define/private (player->internals)
      (define-values (internals _)
        (for/fold : (values (Listof Player) (Listof Tile))
                  ((internals : (Listof Player) '())
                   (tile* : (Listof Tile) ALL-TILES))
                  ((name+eplayer : (List String (Instance Player%)) *named-players))
          (define name (first name+eplayer))
          (define tiles (take tile* STARTER-TILES#))
          (define player
            ;;bg; STARTER-TILES# = 6, shows the typechecker which case-> to use
            (let ([t1 (first  tiles)]
                  [t2 (second tiles)]
                  [t3 (third  tiles)]
                  [t4 (fourth tiles)]
                  [t5 (fifth  tiles)]
                  [t6 (sixth  tiles)])
            (player0 name t1 t2 t3 t4 t5 t6 (second name+eplayer))))
          (values (cons player internals) (drop tile* STARTER-TILES#))))
      internals)
    
    ;; [Listof Player] State -> State 
    (: setup-all (-> (Listof Player) State State))
    (define/private (setup-all players state)
      (define misbehaving
        (for/fold : (Listof Player)
                  ((misbehaving : (Listof Player) '()))
                  ((p : Player players))
          ((inst in-sandbox Void (Listof Player))
           (lambda arg*
             (let ([pe (or (player-external p) (error 'external-failed))])
               (send pe setup state)))
           (lambda (_void)
             misbehaving)
           (lambda status
             (log status `(setup ,(player-name p)))
             (cons p misbehaving)))))
      (if (empty? misbehaving) state (state-eliminate state misbehaving)))
    
    ;; State -> Void 
    ;; score the final state and send the final state and the score board to the players
    (: finish (-> State Void))
    (define/private (finish state)
      (define score (state-score state))
      (for ((e : Player (state-players state)))
        ((inst in-sandbox Void Void)
         (lambda ()
           (send (or (player-external e) (error 'noexternal))  the-end state score)) 
         void
         (lambda (status)
           (log status `(end game ,(player-name e)))))))))

(define DONE 'done)
(define EXHAUSTED 'exhausted)
(define SCORE 'score)

(: turn% Turn%)
(define turn%
  (class object% 
    (init-field current-state)
    
    (field 
     [board   (state-board current-state)]
     [current (state-current-player current-state)]
     [cash    (player-money current)]
     [tiles   (player-tiles current)]
     [shares  (state-shares current-state)]
     [hotels  (state-hotels current-state)]
     [players (state-players current-state)])
    
    (super-new)
    
    (define/public (reconcile-shares t)
      ;; in principle: 
      ;; (error 'reconcile-shares "not possible for local game play")
      ;; but I need to accommodate game testing
      t)
    
    (define/public (eliminated)
      *eliminated)
    
    (define/public (decisions)
      ;; (unless (send this place-called) (error "Temporal contract violation"))
      (values *tile *hotel *decisions))
    
    (define/public (place-called)
      *called)

    (: *tile (Option Tile))
    (define *tile #f)
    (: *hotel (Option Hotel))
    (define *hotel #f)
    (: *decisions Decisions)
    (define *decisions '())
    (: *eliminated (Listof Player))
    (define *eliminated '())
    (: *called Boolean)
    (define *called #f)
    
    ;; effect: increments *called, sets *decisions to players' decisions
    (define/public (place tile hotel)
      ;; -------------------------
      ;; temporal contract
      (unless *called
        (set! *called #t)
        (set! *tile tile)
        (set! *hotel hotel)
        ;; -------------------------
        (define-values (acquirers acquired) (merging-which board tile))
        (define acquired-hotels (append (remove hotel acquirers) acquired))
        
        ;; determine decisions and update *decisions 
        (let keep-to-all : Any
             ((players : (Listof Player) players))
          (unless (empty? players)
            (define p (first players))
            ((inst in-sandbox (Listof Boolean) Any)
             (lambda ()
               (define ex (player-external p))
               (if (boolean? ex)
                   ;; accommodate fake players, and say that they always keep all shares 
                   (map (lambda (_) #t) acquired-hotels)
                   (send ex keep acquired-hotels)))
             (lambda ([p-s-decisions : (Listof Boolean)])
               (set! *decisions
                     (cons
                      (list p
                            (for/list : (Listof (List Hotel Boolean))
                                      ([h : Hotel (in-list acquired-hotels)]
                                       [d : Any (in-list p-s-decisions)])
                              (list h (if d #t #f))))
                      *decisions))
               (keep-to-all (rest players)))
             (lambda (status)
               (log status `(keep failure ,(player-name p)))
               (set! *eliminated (cons p *eliminated))
               (keep-to-all (rest players))))))
        
        ;; now change state and let the current player know 
        (define state/eliminated (state-eliminate current-state *eliminated))
        (define state/returns (state-return-shares state/eliminated *decisions))
        ;; interesting question: could a return blow up?
        (set! current-state state/returns)
        (state-players state/returns)))))

