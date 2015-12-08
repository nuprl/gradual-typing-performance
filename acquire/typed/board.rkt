#lang typed/racket

;; ---------------------------------------------------------------------------------------------------
;; a data representation for the board with operations for 
;; -- inspecting the board to strategize 
;; -- placing tiles on the board to realize a player's action 
;; the actions do not enforce the rules of the game only consistent placements 

;; also exports tiles+spots: A1 ... I12 via submodule tiles+spots

;; ---------------------------------------------------------------------------------------------------
(provide
 Board
 Tile
 
 tile?
 ;; (-> Any Boolean)

 tile<=?
 ;; (-> Tile Tile Boolean)

 tile->string
 ;; (-> Tile String)

 ALL-TILES
 ;; (Listof Tile)
 ;; Postcondition: Sorted

 STARTER-TILES#
 ;; Natural

 FOUNDING GROWING MERGING SINGLETON IMPOSSIBLE
 ;; Symbol

 board?
 ;; (-> Any Boolean)

 (rename-out [board make-board])
 ;; (-> Board)

 board-tiles
 ;; (-> Board (Listof Tile))

 (rename-out [ext:what-kind-of-spot what-kind-of-spot])
 ;; (-> Board Tile SpotType
 ;; (define-type SpotType (U FOUNDING GROWING MERGING SINGLETON IMPOSSIBLE))
 ;; Precondition:
 ;;   (free-spot? b t)
 ;; Postcondition:
 ;;   (U FOUNDING GROWING MERGING SINGLETON IMPOSSIBLE)

 (rename-out [ext:growing-which growing-which])
 ;; (-> Board Tile Hotel)
 ;; Precondition: (eq? (what-kind-of-spot b t) GROWING)

 (rename-out [ext:merging-which merging-which])
 ;; (-> Board Tile (Values (Pairof Hotel (Listof Hotel)) (Listof Hotel)))
 ;; Precondition: (eq? (what-kind-of-spot b t) MERGING)

 deduplicate/hotel

 size-of-hotel
 ;; (-> Board Hotel Natural)

 free-spot?
 ;; (-> Board Tile Boolean)

 (rename-out [ext:merge-hotels merge-hotels])
 ;; (-> Board Tile Hotel Board)
 ;; Precondition: (eq? (what-kind-of-spot b t) MERGING)
 ;; Precondition: (let-values ([(w _) (merging-which b t)]) (member h w))

 (rename-out [ext:found-hotel found-hotel])
 ;; (-> Board Tile Hotel Board)
 ;; Precondition: (eq? (what-kind-of-spot b t) FOUNDING)

 (rename-out [ext:grow-hotel grow-hotel])
 ;; (-> Board Tile Board)
 ;; Precondition: (eq? (what-kind-of-spot b t) GROWING)

 (rename-out [ext:place-tile place-tile])
 ;; (-> Board Tile Board)
 ;; Precondition: (memq (what-kind-of-spot b t) (list SINGLETON GROWING FOUNDING))

 (rename-out [ext:set-board set-board])
 ;; (-> Board Tile (U FOUNDING GROWING MERGING SINGLETON) (Option Hotel) Board)
 ;; Precondition: (free-spot? b t)
 ;; Precondition: (==> h (or (eq? FOUNDING a) (eq? MERGING a)))
 ;; Precondition: (==> (eq? MERGING a) h)

 (rename-out [ext:affordable? affordable?])
 ;; (-> Board Shares-Order Cash Boolean)

 (rename-out [ext:*create-board-with-hotels *create-board-with-hotels])
 ;; (-> (Listof Tile) (Listof (Pairof Hotel (Listof Tile))) Board)
 ;; Precondition: (distinct t*)
 ;; Precondition: ((distinct-and-properly-formed t) ht*)

 distinct-and-properly-formed
 ;; (-> (Listof Tile) (-> (Listof (Pairof Hotel (Listof Tile))) Boolean))
 )

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION: SPOTS

(require
 "../base/types.rkt"
 )
(require/typed "basics.rkt"
 (hotel? (-> Any Boolean))
 (SAFE# Natural)
 (price-per-share (-> Hotel Natural (Option Cash)))
 (shares-order? (-> Any Boolean))
)
(require/typed "auxiliaries.rkt"
  (aux:partition (->* [(Listof (List Hotel Natural)) (-> (List Hotel Natural) Real)] [#:info (-> (List Hotel Natural) Hotel)] (Listof (Listof Hotel))))
  (distinct (-> (Listof Any) Boolean))
)

(define-type Board (HashTable Tile Content))
(define-type HT (Listof Tile)) ;; Should have at least 2 members

;; =============================================================================

(module tiles typed/racket
  (provide (all-defined-out))
  (require
   "../base/types.rkt")
  (require/typed "basics.rkt"
    (hotel->color (-> Hotel Color))
    (hotel->label (-> Hotel String)))
  (require/typed "auxiliaries.rkt"
    (randomly-pick (All (A) (-> (Listof A) A))))
  ;; (require 2htdp/image)
  ;; (require (only-in srfi/1 list-index))
  
  ;; ---------------------------------------------------------------------------------------------------
  ;; ROWS and COLUMNS


  (: COLUMNS (Listof Natural))
  (define COLUMNS
    '(1 2 3 4 5 6 7 8 9 10 11 12 13))

  (: column? (-> Any Boolean))
  (define (column? c)
    (if (member c COLUMNS) #t #f))

  (: random-column (-> Natural))
  (define (random-column)
    (randomly-pick COLUMNS))

  (: column-> (-> Natural (Option Natural)))
  (define (column-> c)
    (let loop : (Option Natural)
         ([col* : (Listof Natural) COLUMNS])
      (cond
       [(or (null? col*) (null? (cdr col*)))
        #f]
       [(eq? c (car col*))
        (cadr col*)]
       [else
        (loop (cdr col*))])))

  (: column-< (-> Natural (Option Natural)))
  (define (column-< c)
    (let loop : (Option Natural)
         ([prev : (Option Natural) #f]
          [col* : (Listof Natural) COLUMNS])
     (cond
       [(null? col*) #f]
       [(eq? c (car col*)) prev]
       [else (loop (car col*) (cdr col*))])))
  
  (: column<= (-> Natural Natural Boolean))
  (define column<= <=)

  ;; (: column->text (-> Natural 
  ;; (define (column->text c)
  ;;   (text/font (number->string c) 24 "black" #f 'system 'normal 'normal #f))

  (: string->column (-> Any (Option Natural)))
  (define (string->column s)
    (and (string? s)
         (let ([n (string->number s)])
           (and n
                (member n COLUMNS)
                (exact-nonnegative-integer? n)
                n))))

  (: column->string (-> Natural String))
  (define column->string number->string)

  
  (: ROWS (Listof Row))
  (define ROWS '(A B C D E F G H I))

  (: row? (-> Any Boolean))
  (define (row? r)
    (if (member r ROWS) #t #f))

  (: random-row (-> Row))
  (define (random-row) (randomly-pick ROWS))

  (: row-v (-> Row (Option Row)))
  (define (row-v r)
    (let loop : (Option Row)
         ([row* : (Listof Row) ROWS])
         (cond
           [(or (null? row*) (null? (cdr row*)))
            #f]
           [(eq? (car row*) r)
            (cadr row*)]
           [else
            (loop (cdr row*))])))

  (: row-^ (-> Row (Option Row)))
  (define (row-^ r)
    (let loop : (Option Row)
         ([prev : (Option Row) #f]
          [row* : (Listof Row) ROWS])
         (cond
           [(null? row*) #f]
           [(eq? (car row*) r) prev]
           [else (loop (car row*) (cdr row*))])))

  (: list-index (-> Symbol (Listof Symbol) Integer))
  (define (list-index x y*)
    (or
     (for/or : (Option Integer)
             ([y : Symbol (in-list y*)]
             [i : Natural (in-naturals)])
       (and (eq? x y) i))
     90))
  
  ;; ;; Row Row -> Boolean 
  ;; ;; does q appear before r in order?
  (: row<= (-> Row Row Boolean))
  (define (row<= q r)
    (<= (list-index q ROWS)
        (list-index r ROWS)))
  
  ;; ;; Row Row -> Boolean 
  ;; ;; does q appear strictly before r in order?
  (: row<< (-> Row Row Boolean))
  (define (row<< q r)
    (< (list-index q ROWS)
       (list-index r ROWS)))
  
  ;; (define (row->text r)
  ;;   (text/font  (symbol->string r) 24 "black" #f 'system 'normal 'normal #f))

  (: string->row (-> String (Option Row)))
  (define (string->row s)
    (and (string? s)
         (let ([n (string->symbol s)])
           (and n (member n ROWS) n))))

  (: row->string (-> Row String))
  (define row->string symbol->string)

  (: CELL-SIZE Natural)
  (define CELL-SIZE 66)
  
  ;; ---------------------------------------------------------------------------------------------------
  (define-type Tile tile)
  (struct tile
    ([column : Column]
     [row : Row]
     ) #:transparent)

  (: TILE-SIZE Natural)
  (define TILE-SIZE (assert (- CELL-SIZE 3) exact-nonnegative-integer?))
  (: TILE-COLOR Symbol)
  (define TILE-COLOR 'gray)
  (: STARTER-TILES# Natural)
  (define STARTER-TILES# 6)
  
  (define-syntax-rule
    ;; should be an identifier macro but contracts
    (ctile letter number) (tile/c number 'letter))

  ;; (-> column? row? any)
  (: tile/c (-> Column Row Tile))
  (define (tile/c c r)
    (if (and (column? c) (row? r))
        (tile c r)
        (error 'tile "not (column,row): ~e, ~e" c r)))

  (: ALL-TILES (Listof Tile))
  (define ALL-TILES 
    (for*/list : (Listof Tile)
               ((r : Row ROWS) (c : Natural COLUMNS))
      (tile c r)))
  
  ;; Tile Tile -> Boolean 
  ;; is t1 closer to the top-left corner than t2?
  (: tile<=? (-> Tile Tile Boolean))
  (define (tile<=? t1 t2)
    (or (row<< (tile-row t1) (tile-row t2))
        (and (row<= (tile-row t1) (tile-row t2))
             (column<= (tile-column t1) (tile-column t2)))))

  (: tile<? (-> Tile Tile Boolean))
  (define (tile<? s t)
    (and (tile<=? s t) (not (equal? s t))))

  (: tile>? (-> Tile Tile Boolean))
  (define (tile>? s t)
    (and (tile<=? t s) (not (equal? s t))))

  (: tile->string (-> Tile String))
  (define (tile->string t)
    (format "(~a,~a)" (tile-column t) (tile-row t)))

  )

(module tiles+spots typed/racket
  ;; define and export: A1 ... I12 as names for matching tiles 
  
  (require (submod ".." tiles))
  (provide A1) (define: A1 : Tile (ctile A 1))(provide A2) (define: A2 : Tile (ctile A 2))(provide A3) (define: A3 : Tile (ctile A 3))(provide A4) (define: A4 : Tile (ctile A 4))(provide A5) (define: A5 : Tile (ctile A 5))(provide A6) (define: A6 : Tile (ctile A 6))(provide A7) (define: A7 : Tile (ctile A 7))(provide A8) (define: A8 : Tile (ctile A 8))(provide A9) (define: A9 : Tile (ctile A 9))(provide A10) (define: A10 : Tile (ctile A 10))(provide A11) (define: A11 : Tile (ctile A 11))(provide A12) (define: A12 : Tile (ctile A 12))(provide A13) (define: A13 : Tile (ctile A 13))
  (provide B1) (define: B1 : Tile (ctile B 1))(provide B2) (define: B2 : Tile (ctile B 2))(provide B3) (define: B3 : Tile (ctile B 3))(provide B4) (define: B4 : Tile (ctile B 4))(provide B5) (define: B5 : Tile (ctile B 5))(provide B6) (define: B6 : Tile (ctile B 6))(provide B7) (define: B7 : Tile (ctile B 7))(provide B8) (define: B8 : Tile (ctile B 8))(provide B9) (define: B9 : Tile (ctile B 9))(provide B10) (define: B10 : Tile (ctile B 10))(provide B11) (define: B11 : Tile (ctile B 11))(provide B12) (define: B12 : Tile (ctile B 12))(provide B13) (define: B13 : Tile (ctile B 13))
  (provide C1) (define: C1 : Tile (ctile C 1))(provide C2) (define: C2 : Tile (ctile C 2))(provide C3) (define: C3 : Tile (ctile C 3))(provide C4) (define: C4 : Tile (ctile C 4))(provide C5) (define: C5 : Tile (ctile C 5))(provide C6) (define: C6 : Tile (ctile C 6))(provide C7) (define: C7 : Tile (ctile C 7))(provide C8) (define: C8 : Tile (ctile C 8))(provide C9) (define: C9 : Tile (ctile C 9))(provide C10) (define: C10 : Tile (ctile C 10))(provide C11) (define: C11 : Tile (ctile C 11))(provide C12) (define: C12 : Tile (ctile C 12))(provide C13) (define: C13 : Tile (ctile C 13))
  (provide D1) (define: D1 : Tile (ctile D 1))(provide D2) (define: D2 : Tile (ctile D 2))(provide D3) (define: D3 : Tile (ctile D 3))(provide D4) (define: D4 : Tile (ctile D 4))(provide D5) (define: D5 : Tile (ctile D 5))(provide D6) (define: D6 : Tile (ctile D 6))(provide D7) (define: D7 : Tile (ctile D 7))(provide D8) (define: D8 : Tile (ctile D 8))(provide D9) (define: D9 : Tile (ctile D 9))(provide D10) (define: D10 : Tile (ctile D 10))(provide D11) (define: D11 : Tile (ctile D 11))(provide D12) (define: D12 : Tile (ctile D 12))(provide D13) (define: D13 : Tile (ctile D 13))
  (provide E1) (define: E1 : Tile (ctile E 1))(provide E2) (define: E2 : Tile (ctile E 2))(provide E3) (define: E3 : Tile (ctile E 3))(provide E4) (define: E4 : Tile (ctile E 4))(provide E5) (define: E5 : Tile (ctile E 5))(provide E6) (define: E6 : Tile (ctile E 6))(provide E7) (define: E7 : Tile (ctile E 7))(provide E8) (define: E8 : Tile (ctile E 8))(provide E9) (define: E9 : Tile (ctile E 9))(provide E10) (define: E10 : Tile (ctile E 10))(provide E11) (define: E11 : Tile (ctile E 11))(provide E12) (define: E12 : Tile (ctile E 12))(provide E13) (define: E13 : Tile (ctile E 13))
  (provide F1) (define: F1 : Tile (ctile F 1))(provide F2) (define: F2 : Tile (ctile F 2))(provide F3) (define: F3 : Tile (ctile F 3))(provide F4) (define: F4 : Tile (ctile F 4))(provide F5) (define: F5 : Tile (ctile F 5))(provide F6) (define: F6 : Tile (ctile F 6))(provide F7) (define: F7 : Tile (ctile F 7))(provide F8) (define: F8 : Tile (ctile F 8))(provide F9) (define: F9 : Tile (ctile F 9))(provide F10) (define: F10 : Tile (ctile F 10))(provide F11) (define: F11 : Tile (ctile F 11))(provide F12) (define: F12 : Tile (ctile F 12))(provide F13) (define: F13 : Tile (ctile F 13))
  (provide G1) (define: G1 : Tile (ctile G 1))(provide G2) (define: G2 : Tile (ctile G 2))(provide G3) (define: G3 : Tile (ctile G 3))(provide G4) (define: G4 : Tile (ctile G 4))(provide G5) (define: G5 : Tile (ctile G 5))(provide G6) (define: G6 : Tile (ctile G 6))(provide G7) (define: G7 : Tile (ctile G 7))(provide G8) (define: G8 : Tile (ctile G 8))(provide G9) (define: G9 : Tile (ctile G 9))(provide G10) (define: G10 : Tile (ctile G 10))(provide G11) (define: G11 : Tile (ctile G 11))(provide G12) (define: G12 : Tile (ctile G 12))(provide G13) (define: G13 : Tile (ctile G 13))
  (provide H1) (define: H1 : Tile (ctile H 1))(provide H2) (define: H2 : Tile (ctile H 2))(provide H3) (define: H3 : Tile (ctile H 3))(provide H4) (define: H4 : Tile (ctile H 4))(provide H5) (define: H5 : Tile (ctile H 5))(provide H6) (define: H6 : Tile (ctile H 6))(provide H7) (define: H7 : Tile (ctile H 7))(provide H8) (define: H8 : Tile (ctile H 8))(provide H9) (define: H9 : Tile (ctile H 9))(provide H10) (define: H10 : Tile (ctile H 10))(provide H11) (define: H11 : Tile (ctile H 11))(provide H12) (define: H12 : Tile (ctile H 12))(provide H13) (define: H13 : Tile (ctile H 13))
  (provide I1) (define: I1 : Tile (ctile I 1))(provide I2) (define: I2 : Tile (ctile I 2))(provide I3) (define: I3 : Tile (ctile I 3))(provide I4) (define: I4 : Tile (ctile I 4))(provide I5) (define: I5 : Tile (ctile I 5))(provide I6) (define: I6 : Tile (ctile I 6))(provide I7) (define: I7 : Tile (ctile I 7))(provide I8) (define: I8 : Tile (ctile I 8))(provide I9) (define: I9 : Tile (ctile I 9))(provide I10) (define: I10 : Tile (ctile I 10))(provide I11) (define: I11 : Tile (ctile I 11))(provide I12) (define: I12 : Tile (ctile I 12))(provide I13) (define: I13 : Tile (ctile I 13))  
  ;; (define-syntax (define-all-tiles stx)
  ;;   #`(begin 
        ;; (for* ((r ROWS) (c COLUMNS))
        ;;      (define rc (format "~a~a" r c))
        ;;      (printf "(provide ~a) (define: ~a : Tile (ctile ~a ~a))" rc rc r c))
  )

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION: BOARD

(require (submod "." tiles))

;; ---------------------------------------------------------------------------------------------------
;; data

(define FOUNDING 'FOUNDING)
(define GROWING 'GROWING)
(define MERGING 'MERGING)
(define SINGLETON 'SINGLETON)
(define IMPOSSIBLE 'IMPOSSIBLE)

;; no tile is placed in this cell
(define UNTAKEN 'UNTAKEN)

;; a tile is placed but it does not belong to a hotel 
(define TAKEN-NO-HOTEL 'taken-no-hotel)

;; Content = Hotel | UNTAKEN | TAKEN-NO-HOTEL

;; board   = [Hashof Tile Content]
;; if a (c,r) key maps to a hotel, it belongs to this hotel; otherwise it is free

(: board? (-> Any Boolean))
(define (board? x)
  ;;bg; do more?
  (and (hash? x)))

(: board (-> Board))
(define (board) #hash())

(: board-tiles (-> Board (Listof Tile)))
(define (board-tiles b) (hash-keys b))

;; ---------------------------------------------------------------------------------------------------
;; FUNCTIONS: internal 

;; Tile *-> Board 
;; place tiles; do not found or create hotels
(: board* (-> Tile * Board))
(define (board* . t*)  
  (for/fold : Board
            ((b : Board (board)))
            ((t : Tile (in-list t*)))
    (place-tile b t)))

(: ext:*create-board-with-hotels (-> (Listof Tile) (Listof (Pairof Hotel (Listof Tile))) Board))
(define (ext:*create-board-with-hotels lt lh)
  (unless (distinct lt)
    (error 'create-board-with-hotels "precondition"))
  (unless ((distinct-and-properly-formed lt) lh)
    (error 'create-board-with-hotels "precondition"))
  (*create-board-with-hotels lt lh))

(: *create-board-with-hotels (-> (Listof Tile) (Listof (Pairof Hotel (Listof Tile))) Board))
(define (*create-board-with-hotels lt lh)
  (for/fold : Board
            ((b : Board (apply board* lt)))
            ((h : (Pairof Hotel (Listof Tile)) lh))
    (define name (first h))
    (define til* (rest h))
    (for/fold : Board
              ((b : Board b))
              ((t : Tile (in-list (rest h))))
      (hash-set b t name))))

(: board-ref (-> Board Column Row Content))
(define (board-ref b c r)
  ((inst hash-ref Tile Content Content) b (tile c r) (lambda () (ann UNTAKEN Content))))

;; Board Column Row -> Content
(: board-set (->* [Board Column Row] [Content] Board))
(define (board-set b c r [h (ann TAKEN-NO-HOTEL Content)])
  (hash-set b (tile c r) h))

;; Board Column Row [X X X X ->* Y] ->* Y 
;; produce neighbors in North East South and West
(: neighbors (-> Board Column Row (-> Content Content Content Content (Listof Content)) (Listof Content)))
(define (neighbors b c r f)
  (f (north b c r) (east b c r) (south b c r) (west b c r)))

(: cardinal-direction (-> (-> Row (U Boolean Row))
                          (-> Column (U Boolean Column))
                          (-> Board Column Row Content)))
(define (cardinal-direction n-s e-w)
  (lambda ([b : Board]
           [c : Column]
           [r : Row])
    (define north-south (n-s r))
    (define east-west (e-w c))
    (cond 
      [(boolean? north-south) (ann UNTAKEN Content)]
      [(boolean? east-west) (ann UNTAKEN Content)]
      [else (board-ref b east-west north-south)])))

(: north (-> Board Column Row Content))
(define north (cardinal-direction row-^ (lambda ([x : Column]) x)))
(: south (-> Board Column Row Content))
(define south (cardinal-direction row-v (lambda ([x : Column]) x)))
(: east (-> Board Column Row Content))
(define east (cardinal-direction (lambda ([x : Row]) x) column->))
(: west (-> Board Column Row Content))
(define west (cardinal-direction (lambda ([x : Row]) x) column-<))

;; Board Hotel -> [Listof Tile] | sorted * tile<=?
(: tiles-with-specific-label (-> Board Hotel (Listof Tile)))
(define (tiles-with-specific-label b h)
  (define t* (for/list : (Listof Tile) ([(s label) (in-hash b)] #:when (equal? h label)) s))
  (sort t* tile<=?))

;; ---------------------------------------------------------------------------------------------------
;; FUNCTIONS: external functions  

(: free-spot? (-> Board Tile Boolean))
(define (free-spot? board tile)
  (eq? (board-ref board (tile-column tile) (tile-row tile))
       UNTAKEN))

(: ext:what-kind-of-spot (-> Board Tile SpotType))
(define (ext:what-kind-of-spot board tile)
  (unless (free-spot? board tile)
    (error 'what-kind-of-spot (format "Precondition: (free-spot ~a ~a)" board tile)))
  (what-kind-of-spot board tile))

(: what-kind-of-spot (-> Board Tile SpotType))
(define (what-kind-of-spot board tile)
  (define column (tile-column tile))
  (define row (tile-row tile))
  (define surroundings (neighbors board column row list))
  (define hotels (deduplicate/hotel
                  (for/list : (Listof Hotel)
                            ([s : Content (in-list surroundings)]
                             #:when (hotel? s))
                    (assert s string?))))
  (define hotels# (length hotels))
  (define neighbor-taken-no-hotel?
    (for/or : (Option Content)
            ([s (in-list surroundings)])
      (and (eq? TAKEN-NO-HOTEL s) s)))
  (cond
    [(= hotels# 0) (ann (if neighbor-taken-no-hotel? FOUNDING SINGLETON) SpotType)]
    [(= hotels# 1) (ann (if neighbor-taken-no-hotel? IMPOSSIBLE GROWING) SpotType)]
    [(>= hotels# 2)
     (define any-hotel-safe?
       (for/or : Boolean
               ((h : Hotel (in-list hotels)))
         (>= (size-of-hotel board h) SAFE#)))
     (ann (if (or neighbor-taken-no-hotel? any-hotel-safe?) IMPOSSIBLE MERGING) SpotType)]
    [else (error 'nope)]))

(: ext:growing-which (-> Board Tile (Option Hotel)))
(define (ext:growing-which board tile)
  (unless (eq? (what-kind-of-spot board tile) GROWING)
    (error 'growing-which (format "Precondition: expected growing, got ~a" (what-kind-of-spot board tile))))
  (growing-which board tile))

(: growing-which (-> Board Tile (Option Hotel)))
(define (growing-which board tile)
  ;; the 'first' is guranateed by contract
  (define n* (neighbors board (tile-column tile) (tile-row tile) list))
  (for/or : (Option Hotel)
          ([c : (Option Content) (in-list n*)])
    (and (hotel? c) (assert c string?))))

(: ext:merging-which (-> Board Tile (Values (Pairof Hotel (Listof Hotel)) (Listof Hotel))))
(define (ext:merging-which board tile)
  (unless (eq? (what-kind-of-spot board tile) MERGING)
    (error 'merging-which (format "Precondition: expected merging, got ~a" (what-kind-of-spot board tile))))
  (merging-which board tile))

(: merging-which (-> Board Tile (Values (Pairof Hotel (Listof Hotel)) (Listof Hotel))))
(define (merging-which board tile)
  (define surroundings (neighbors board (tile-column tile) (tile-row tile) list))
  (define hotels (deduplicate/hotel
                  (for/list : (Listof Hotel)
                            ([s : Content (in-list surroundings)]
                             #:when (hotel? s))
                    (assert s string?))))
  (define sorted
    (let ([x* (for/list : (Listof (List Hotel Natural))
                        ([h : Hotel (in-list hotels)])
                (list h (size-of-hotel board h)))])
      (sort x* (lambda ([x : (List Hotel Natural)] [y : (List Hotel Natural)]) (> (cadr x) (cadr y))))))
  (define partitioned (aux:partition sorted second #:info (lambda ([x : (List Hotel Natural)]) (car x))))
  (values (cast (first partitioned) (Pairof Hotel (Listof Hotel))) (apply append (rest partitioned))))

(: deduplicate/hotel (-> (Listof Hotel) (Listof Hotel)))
(define (deduplicate/hotel h*)
  (let loop : (Listof Hotel) ([h* : (Listof Hotel) h*])
       (cond [(null? h*) '()]
             [(member (car h*) (cdr h*)) (loop (cdr h*))]
             [else (cons (car h*) (loop (cdr h*)))])))

(: size-of-hotel (-> Board Hotel Natural))
(define (size-of-hotel board hotel)
  (for/fold : Natural
            ((size : Natural 0))
            ([(key value) (in-hash board)])
    (if (equal? hotel value) (+ size 1) size)))

(: ext:grow-hotel (-> Board Tile Board))
(define (ext:grow-hotel board tile)
  (unless (eq? (what-kind-of-spot board tile) GROWING)
    (error 'grow-hotel (format "Precondition: expected founding, got ~a" (what-kind-of-spot board tile))))
  (grow-hotel board tile))

(: grow-hotel (-> Board Tile Board))
(define (grow-hotel board tile)
  (define row (tile-row tile))
  (define column (tile-column tile))
  (define surroundings (neighbors board column row list))
  (define hotel-that-touches (first (filter hotel? surroundings)))
  (board-set board column row hotel-that-touches))

(: ext:merge-hotels (-> Board Tile Hotel Board))
(define (ext:merge-hotels board tile hotel)
  (unless (eq? (what-kind-of-spot board tile) MERGING)
    (error 'merge-hotels (format "Precondition: expected merging, got ~a" (eq? (what-kind-of-spot board tile) MERGING))))
  (unless (let-values ([(w _) (merging-which board tile)]) (member hotel w))
    (error 'merge-hotels (format "Precondition: hotel ~a is not on a merging spot" hotel)))
  (merge-hotels board tile hotel))

(: merge-hotels (-> Board Tile Hotel Board))
(define (merge-hotels board tile hotel)
  (define row (tile-row tile))
  (define column (tile-column tile))
  (define-values (acquirers acquired) (merging-which board tile))
  (define acquired-hotels (append (remq hotel acquirers) acquired))
  (define relabeled-hotel
    (for/hash : Board
              (([key current-content] (in-hash board)))
      (if (memq current-content acquired-hotels)
          (values key hotel)
          (values key current-content))))
  (board-set relabeled-hotel column row hotel))

(: ext:found-hotel (-> Board Tile Hotel Board))
(define (ext:found-hotel board tile hotel)
  (unless (eq? (what-kind-of-spot board tile) FOUNDING)
    (error 'found-hotel (format "Precondition: expected founding, got ~a" (what-kind-of-spot board tile))))
  (found-hotel board tile hotel))

(: found-hotel (-> Board Tile Hotel Board))
(define (found-hotel board tile hotel)
  (define row (tile-row tile))
  (define column (tile-column tile))
  (board-set (hotel-take-over-neighboring-tiles board column row hotel) column row hotel))

;; Board Column Row Hotel -> Board 
;; mark all TAKEN-NO-HOTEL tiles on board reachable from (column,row) as belong to hotel
;; Global Invariant: This region does not touch any other hotels
(: hotel-take-over-neighboring-tiles (-> Board Column Row Hotel Board))
(define (hotel-take-over-neighboring-tiles board column row hotel)
  (let loop : Board
       ((board : Board board)
        (to-visit : (Listof Tile) (list (tile column row)))
        (visited : (Listof Tile) '()))
    (cond
      [(empty? to-visit) board]
      [(member (first to-visit) visited) (loop board (rest to-visit) visited)]
      [else 
       (define column (tile-column (first to-visit)))
       (define row (tile-row (first to-visit)))
       (define-values (n e s w)
         (let ([r (neighbors board column row list)])
           (values (car r) (cadr r) (caddr r) (cadddr r))))
       (define no-tiles (ann '() (Listof Tile)))
       (loop (board-set board column row hotel)
             (append (if (equal? TAKEN-NO-HOTEL n) (list (tile column (or (row-^ row) (error 'badrow)))) no-tiles)
                     (if (equal? TAKEN-NO-HOTEL e) (list (tile (or (column-> column) (error 'badcol)) row)) no-tiles)
                     (if (equal? TAKEN-NO-HOTEL s) (list (tile column (or (row-v row) (error 'badrow)))) no-tiles)
                     (if (equal? TAKEN-NO-HOTEL w) (list (tile (or (column-< column) (error 'badcol)) row)) no-tiles)
                     (rest to-visit))
             (cons (first to-visit) visited))])))

(: ext:place-tile (-> Board Tile Board))
(define (ext:place-tile board tile)
  (unless (memq (what-kind-of-spot board tile) (list SINGLETON GROWING FOUNDING))
    (error 'place-tile "precondition"))
  (place-tile board tile))

(: place-tile (-> Board Tile Board))
(define (place-tile board tile)
  (define row (tile-row tile))
  (define column (tile-column tile))
  (board-set board column row))

(: ext:set-board (-> Board Tile Kind (Option Hotel) Board))
(define (ext:set-board board tile kind hotel)
  (unless (free-spot? board tile)
    (error 'set-board "Precondition"))
  (unless (if hotel (or (eq? FOUNDING kind) (eq? MERGING kind)) #t)
    (error 'set-board "Precondition"))
  (unless (if (eq? MERGING kind) hotel #t)
    (error 'set-board "Precondition"))
  (set-board board tile kind hotel))

(: set-board (-> Board Tile Kind (Option Hotel) Board))
(define (set-board board tile kind hotel)
  (cond
    [(eq? FOUNDING kind) (if hotel (found-hotel board tile hotel) (place-tile board tile))]
    [(and hotel (eq? MERGING kind)) (merge-hotels board tile hotel)]
    [(and hotel (eq? SINGLETON kind)) (place-tile board tile)]
    [(and hotel (eq? GROWING kind)) (grow-hotel board tile)]
    [else (error 'nopers)]))

(: ext:affordable? (-> Board (Listof Hotel) Cash Boolean))
(define (ext:affordable? board hotels budget)
  (unless (shares-order? hotels)
    (error 'afoordable "precondigin"))
  (affordable? board hotels budget))

(: affordable? (-> Board (Listof Hotel) Cash Boolean))
(define (affordable? board hotels budget)
  (define prices
    (for/list : (Listof (Option Cash))
              ([h : Hotel (in-list hotels)])
      (price-per-share h (size-of-hotel board h))))
  (define s
    (for/fold : Cash
              ([acc : Cash 0])
              ([c : (Option Cash) (in-list prices)])
              (if c (+ c acc) acc)))
  (if (ormap boolean? prices) #f (<= s budget)))

;; ---------------------------------------------------------------------------------------------------
;; notions of contracts for creating a board with hotels specified as lists

;; (1) the hotels have distinct names 
;; (2) each hotel comes with at least two tiles 
;; (3) the tiles of the hotels and the unassociated tiles are distinct 
;; (4) the tiles of each hotel form connected graphs 
;; (5) no two hotel chains touch each other directly
(: distinct-and-properly-formed (-> (Listof Tile) (-> (Listof (Pairof Hotel (Listof Tile))) Boolean)))
(define ((distinct-and-properly-formed free-tiles) hotels-as-lists)
  (define hotel-tiles
    (for/list : (Listof (Listof Tile))
              ([hl : (Pairof Hotel (Listof Tile)) (in-list hotels-as-lists)])
      (cdr hl)))
  (define first*
    (for/list : (Listof Hotel)
              ([hl : (Pairof Hotel (Listof Tile)) (in-list hotels-as-lists)])
      (car hl)))
  (and (or (distinct first*) (tee/#f "hotel names not distinct"))
       (or (andmap contains-at-least-two hotel-tiles) (tee/#f "hotels don't have 2 tiles"))
       (or (distinct (apply append free-tiles hotel-tiles)) (tee/#f "hotel & free tiles overlap"))
       (or (andmap connected-graph hotel-tiles) (tee/#f "hotels not graphs"))
       (or (no-two-hotels-touch hotel-tiles) (tee/#f "two hotels touch"))))

(: tee/#f (-> String Boolean))
(define (tee/#f s)
  (error 'distint-and-properly-formed s))

;; ---------------------------------------------------------------------------------------------------
;; auxiliary notiones 

;; HT = (cons Tile (cons Tile [Listof Tile])) ;; the tiles of a syntactically well-formed hotel

;; ;; HT -> Boolean
(: contains-at-least-two (-> (Listof Tile) Boolean))
(define (contains-at-least-two ht)
  (and (cons? ht) (cons? (rest ht))))

;; ;; HT -> Boolean 
;; ;; do the tiles of a hotel form a connected graph?
(: connected-graph (-> HT Boolean))
(define (connected-graph hotel-tiles)
  (define start (first hotel-tiles))
  (define remaining (rest hotel-tiles))
  (define next (connected-to start remaining))
  (and (cons? next)
       (let loop : Boolean
            ((frontier next)
             (remaining (remove* next remaining)))
         (cond
           [(empty? remaining) #t]
           [else (define one-step (apply append
                                         (map (λ ([f : Tile])
                                                (connected-to f remaining))
                                              frontier)))
                 (and (cons? one-step) (loop one-step (remove* one-step remaining)))]))))

;; ;; [Listof HT] -> Boolean 
;; ;; are any pairs of HTs connected? if so, return #f
(: no-two-hotels-touch (-> (Listof HT) Boolean))
(define (no-two-hotels-touch hotel-tiles*)
  (and-over-pairs-of-distinct-hotels 
   (lambda ([one : HT] [other : HT])
     (not (connected-graph (append one other))))
   hotel-tiles*))

;; ;; [HT HT -> Boolean] [Listof HT] -> Boolean
;; ;; apply f to all pairs of distinct HTs on lh
(: and-over-pairs-of-distinct-hotels (-> (-> HT HT Boolean) (Listof HT) Boolean))
(define (and-over-pairs-of-distinct-hotels f hotel-tiles*)
  (or (empty? hotel-tiles*)
      (let loop : Boolean
           ([preceding : (Listof HT) '()]
            [current : HT (first hotel-tiles*)]
            [remaining : (Listof HT) (rest hotel-tiles*)])
        (cond
          [(empty? remaining) #t]
          [else (and (for/and : Boolean
                              ((h : (Listof Tile) (append preceding remaining)))
                       (f current h))
                     (loop (cons current preceding) (first remaining) (rest remaining)))]))))

;; ;; Tile HT -> [Listof Tile]
;; ;; find all (at most four) tiles in lh that are connected to t
(: connected-to (-> Tile HT (Listof Tile)))
(define (connected-to t hotel-tiles*)
  (define r (tile-row t))
  (define c (tile-column t))
  (: in (-> (Option Row) (Option Column) (Listof Tile)))
  (define (in r c)
    (if (and r c)
        (let ([m (member (tile c r) hotel-tiles*)])
          (if m
              (list (first m))
              '()))
        '()))
  (append (in (or (row-^ r) (error 'badr)) c)
          (in (or (row-v r) (error 'badr)) c)
          (in r (or (column-> c) (error 'badc)))
          (in r (or (column-< c) (error 'badc)))))

