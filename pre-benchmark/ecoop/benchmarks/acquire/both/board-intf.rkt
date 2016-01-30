#lang racket

;; ---------------------------------------------------------------------------------------------------
;; interface specification for inspecting and manipulating the Acquire board, its spots, and tiles 
;; also provides all tiles: A1 ... I12 via tiles+spots submodule 

(require "basics.rkt" "Lib/auxiliaries.rkt" "Lib/contract.rkt" 2htdp/image)

(interface basics&
  ;; creation of tiles, spots 
  [row?           (-> any/c boolean?)]
  [string->row    (-> string? (maybe/c row?))]
  [column?        (-> any/c boolean?)]
  [string->column (-> string? (maybe/c column?))]
  [tile           (-> column? row? any)]
  ;; SYNTAX: 
  ;; (ctile COLUMN ROW) creates a column and avoids the quoting 
  ctile
  
  ;; properties 
  [tile?   (-> any/c boolean?)]
  [tile<=? (-> tile? tile? boolean?)]
  [tile>?  (-> tile? tile? boolean?)]
  [tile<?  (-> tile? tile? boolean?)]
  [tile->string (-> tile? string?)]
  
  [ALL-TILES      (and/c (listof tile?) (sorted tile<=?))]
  [STARTER-TILES# natural-number/c]
  
  ;; externalize tiles
  [xtile?      (-> any/c boolean?)]
  [tile->xexpr (-> tile? xtile?)]
  
  ;; -------------------------------------------------------------------------------------------------
  ;; placing a tile can have one of these effects 
  
  [FOUNDING   symbol?] ;; if a tile is placed here, player may found a hotel IF chains are available
  [GROWING    symbol?]
  [MERGING    symbol?]
  [SINGLETON  symbol?] ;; placing a tile makes it a singleton
  [IMPOSSIBLE symbol?] ;; a hotel is safe from merging 
  
  ;; -------------------------------------------------------------------------------------------------
  ;; creating a board 
  [board?      (-> any/c boolean?)]
  [board       (-> board?)]
  [board-tiles (-> board? (listof tile?))]
  
  ;; -------------------------------------------------------------------------------------------------
  [draw (-> board? image?)]
  [draw-cell (-> tile? image?)]
  
  ;; -------------------------------------------------------------------------------------------------
  ;; properties of the board 
  
  [what-kind-of-spot
   ;; determine whether t is a spot for 
   ;; -- founding a hotel <=> there is exactly one horizonatl or vertical 'free' tile 
   ;; -- merging hotels <=> there are two distinct hotel neighbors 
   ;; -- growing a hotel <=> there is one hotel heighbor 
   ;; -- placing a singleton <=> no neighbors whatsoever 
   ;; -- impossible: if the placement would cause a merger and involved a safe hotel 
   (->i ((b board?) (t tile?))
        #:pre/name (b t) "unoccupied spot" (free-spot? b t)
        (result (or/c FOUNDING GROWING MERGING SINGLETON IMPOSSIBLE)))]
  
  [growing-which
   ;; which hotel grows if place a tile at (c,r)
   (->i ((b board?) (t tile?)) #:pre (b t) (eq? (what-kind-of-spot b t) GROWING) (hotel hotel?))]
  
  [merging-which
   ;; which hotels are merged if place a tile at (c,r)
   (->i ((b board?) (t tile?)) #:pre/name (b t) "merger spot" (eq? (what-kind-of-spot b t) MERGING)
        (values (acquirer (non-empty-listof hotel?)) (acquired (listof hotel?))))]
  
  [size-of-hotel
   ;; how large is the specified hotel on this board   
   (-> board? hotel? natural-number/c)]
  
  [free-spot?
   ;; is this spot unoccupied? (needed for contract of what-kind-of-spot)
   (-> board? tile? boolean?)]
  
  ;; -------------------------------------------------------------------------------------------------
  ;; placing tiles on the board
  
  [merge-hotels 
   ;; place a tile that merges hotels
   (->i ((b board?) (t tile?) (h hotel?))
        #:pre/name (b t) "tile designates a merger spot" (eq? (what-kind-of-spot b t) MERGING)
        #:pre/name (b t h) "... a winner" (let-values ([(w _) (merging-which b t)]) (member h w))
        (new-board board?))]
  
  [found-hotel 
   ;; place a tile and found a hotel 
   (->i ((b board?) (t tile?) (h hotel?)) #:pre (b t) (eq? (what-kind-of-spot b t) FOUNDING) 
        (new-board board?))]
  
  [grow-hotel 
   ;; place a tile and found a hotel 
   (->i ((b board?) (t tile?)) #:pre (b t) (eq? (what-kind-of-spot b t) GROWING) 
        (new-board board?))]
  
  [place-tile
   ;; place a tile that neither merges hotels nor founds one 
   (->i ((b board?) (t tile?))
        #:pre (b t) (memq (what-kind-of-spot b t) (list SINGLETON GROWING FOUNDING))
        (new-board board?))]
  
  [set-board 
   ;; a derived function that re-discovers the appropriate situation and places a tile 
   (->i ((b board?) (t tile?) (a [or/c FOUNDING GROWING MERGING SINGLETON]) (h (maybe/c hotel?)))
        #:pre/name (b t) "good spot" (free-spot? b t)
        #:pre/name (a h) "hotel => founding & merging" (==> h (or (eq? FOUNDING a) (eq? MERGING a)))
        #:pre/name (a h) "merging => hotel" (==> (eq? MERGING a) h)
        (new-board board?))]
  
  [affordable? 
   ;; is the list of hotels for this hotel affordable given the budget constraint 
   (-> board? shares-order/c cash? boolean?)]
  
  ;; -------------------------------------------------------------------------------------------------
  ;; externalizing boards 
  [xboard?      (-> any/c boolean?)]
  [board->xexpr (-> board? xboard?)]
  [xspot?       (-> any/c boolean?)]
  [spot->xexpr  (-> board? tile? [or/c SINGLETON FOUNDING GROWING MERGING IMPOSSIBLE] xspot?)]
  
  [*create-board-with-hotels
   (->i ([t (and/c (listof tile?) distinct)] 
         [lh (t) (and/c (listof (cons/c hotel? (listof tile?))) (distinct-and-properly-formed t))])
        [b board?])]
  
  [distinct-and-properly-formed
   (->i ((free-tiles (listof tile?))) 
        (check-hotels 
         (->i ((hotels-as-lists (listof (cons/c hotel? (listof tile?))))) (ok boolean?))))]
  
  ;; -------------------------------------------------------------------------------------------------
  ;; some sample boards
  board-a1-b2-c6 
  board-a2-b2-american
  board-b2-c2-am-c4-d4-tw-e4
  board-3way-merger-at-d3)
