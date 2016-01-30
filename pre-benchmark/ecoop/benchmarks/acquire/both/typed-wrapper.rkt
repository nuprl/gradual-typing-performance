#lang typed/racket

(require "require-typed-check.rkt")

(require/typed/provide "basics.rkt"
  [#:opaque Shares shares?] 
  [#:opaque Hotel hotel?]
  [#:opaque Cash cash?]
  [#:opaque Shares-Order shares-order?]
  [shares-available? (Shares Shares-Order -> Boolean)]
  [*combine-shares ((Listof Shares) -> Shares)]
  [string->hotel (String -> (Option Hotel))]
  [hotel->label (Hotel -> String)]
  [player-shares0 Shares]
  [banker-shares0 Shares]
  [shares-minus (Shares Shares -> Shares)])

(require/typed/provide "board.rkt" 
  [#:opaque Tile tile?]
  [#:opaque Board board?]
  [#:opaque Row row?]
  [#:opaque Column column?]
  [#:opaque XTile xtile?]
  [tile->xexpr (Tile -> XTile)] 
  [string->column (String -> (Option Column))]
  [string->row (String -> (Option Row))]
  [STARTER-TILES# Natural]
  [ALL-TILES (Listof Tile)]
  [FOUNDING Symbol]
  [MERGING Symbol]
  [IMPOSSIBLE Symbol]
  [affordable? (Board Shares-Order Cash -> Boolean)]
  [what-kind-of-spot (Board Tile -> Symbol)]
  [merging-which (Board Tile -> (Values (Listof Hotel) (Listof Hotel)))])

(require/typed/provide "state.rkt"
  [#:opaque State state?]
  [state-board (State -> Board)]
  [#:opaque Player player?]
  [state-score (State -> Score)]
  [state0 (Player * -> State)]
  [player0 (case-> (String Tile Tile Tile Tile Tile Tile -> Player)
                   (String Tile Tile Tile Tile Tile Tile Any -> Player)
                   (String Tile * -> Player))]
  [state-eliminate (State (Listof Player) -> State)]
  [player-name (Player -> String)]
  [player-external (Player -> (Instance Player%))]
  [state-remove-current-player (State -> State)]
  [state-players (State -> (Listof Player))]
  [state-return-shares (case-> (State (Listof (List Player (Listof (List Hotel Boolean)))) -> State)
                               (State (Listof (List Player (Listof (List Hotel Boolean)))) Board -> State))]
  [state-buy-shares (State Shares-Order -> State)]
  [state-tiles (State -> (Listof Tile))]
  [state-next-turn (State -> State)]
  [state-move-tile (State Tile -> State)]
  [player-money (Player -> Cash)]
  [state-current-player (State -> Player)]
  [state-shares (State -> Shares)]
  [state-final? (State -> Boolean)]
  [state-hotels (State -> (Listof Hotel))]
  [player-tiles (Player -> (Listof Tile))]
  [state-place-tile (case-> (State Tile -> State) (State Tile Hotel -> State))]
  [state-sub-shares (State Shares -> State)] 
  [*create-state (Board (Listof Player) -> State)]
  [*create-player (String Cash Shares (Listof Tile) -> Player)] 
  [player-shares (Player -> Shares)])

(require/typed/provide "Lib/log.rkt"
  [log ((List Any Any) Any -> Any)])

(require/typed "Lib/sandbox.rkt"
  [(in-sandbox in-sandbox-3) 
   (-> (-> Any) (Void -> (Values Any Any Any)) 
       ((List Any Any) -> (Values Symbol Any (Listof State)))
       [#:time Natural] [#:memory Natural] 
       (Values Symbol Any (Listof State)))]
  [(in-sandbox in-sandbox-2) 
   (-> (-> (Values (Option Tile) (Option Hotel) Shares-Order))
       ((Option Tile) (Option Hotel) Shares-Order -> (Values Any Any Any)) 
       ((List Any Any) -> (Values Symbol Any (Listof State)))
       [#:time Natural] [#:memory Natural] 
       (Values Symbol Any (Listof State)))]
  [(in-sandbox in-sandbox-1) 
   (All (a b c)
     (-> (-> a) (a -> c) ((List Any Any) -> c) [#:time Natural] [#:memory Natural] c))]
  [in-sandbox 
   (-> (-> Any) (Any -> Any) (Any -> Any) [#:time Natural] [#:memory Natural] Any)])

(provide in-sandbox-3 in-sandbox-2 in-sandbox-1 in-sandbox Player% Administrator%)
(provide Score Turn-Player% Turn-Administrator% Strategy)
(provide (all-defined-out) (struct-out hand-out))

(define-type Policies (Listof Shares-Order))

(cond-untyped "tree.rkt"
  (require/typed/provide "tree.rkt"
    [#:struct hand-out ([tile : Tile]
                        [tree : (Instance ATree%)])])
  (begin (struct: hand-out ([tile : Tile] [tree : (Instance ATree%)]))
         (provide (struct-out hand-out))))

(define-type Tree<%>
  (Class [to-state (-> State)]
         [next (-> Tile (Option Hotel)
                   (Listof (List Player (Listof (List Hotel Boolean))))
                   Shares-Order ((Listof Tile) -> Tile)
                   (Values (Option Tile) (Instance ATree%)))]
         [founding (Natural Policies -> Natural)]
         [merging (Natural Policies -> Natural)]))

(define-type ATree%
  (Class #:implements Tree<%>
         (init-field [state State])
         [decision-tree? (-> Boolean)]
         [traversal (Natural Policies ((Instance Placed%) -> Natural) -> Natural)]
         [lookup-tile  (-> ((Listof Tile) -> Tile)
                           (Listof hand-out)
                           (Values (Option Tile) (Instance ATree%)))]))

(define-type State%
  (Class #:implements ATree% #:implements Tree<%>
         (init-field [state State])))

(define-type LPlaced%
  (Class #:implements ATree%
         #:implements Tree<%>
         #:implements State%
         (init-field  [lplaced (Listof (Instance Placed%))]
                      [state State])))

(define-type Decisions (Listof (List Player (Listof (List Hotel Boolean)))))
(define-type Placed%
  (Class (init-field [state State] [tile Tile] [hotel (Option Hotel)]
                     [state/tile State] [reason Symbol])
         [purchase (Decisions Shares-Order -> (U (Listof hand-out) (Instance State%)))]
         [to-trees (Decisions Shares-Order -> (Listof (Instance ATree%)))]
         [acceptable-policies ((Listof Shares-Order) -> (Listof Shares-Order))]))

(define-type Player%
  (Class (init-field [name String] [choice Strategy])
         [go ((Instance Administrator%) -> Any)]
         [setup (State -> Any)]
         [take-turn ((Instance Turn-Player%) -> (Values (Option Tile) (Option Hotel) Shares-Order))]
         [keep ((Listof Hotel) -> (Listof Boolean))]
         [receive-tile (Tile -> Any)]
         [inform [State -> Any]]
         [the-end (State Score -> Any)]))

(define-type Administrator%
  (Class (init-field [next-tile ((Listof Tile) -> Tile)])
         [sign-up (String (Instance Player%) -> String)]
         [show-players (-> (Listof String))]
         [run (-> Natural [#:show (Natural State -> Any)] (Values Symbol Any (Listof State)))]))

(define-type Score (Listof (List String Cash)))

(define-type Turn-Administrator%
  (Class [eliminated (-> (Listof Player))]
         [place-called (-> Boolean)]
         [decisions (-> (Values Tile Hotel (Listof (List Player (Listof (List Hotel Boolean))))))]))

(define-type Turn-Player%
  (Class #:implements Turn-Administrator%
         (init-field [current-state State])
         (field [board Board]
                [current Player]
                [cash Cash]
                [tiles (Listof Tile)]
                [shares Shares]
                [hotels (Listof Hotel)]
                [players (Listof Player)])
         [reconcile-shares (Shares -> Any)]
         [place (Tile Hotel -> (U Void (Listof Player)))]))

(define-type Strategy ((Instance Turn-Player%) -> (Values (Option Tile) (Option Hotel) Shares-Order)))
