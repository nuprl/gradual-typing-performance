#lang racket

;; ---------------------------------------------------------------------------------------------------
;; a data representation for the board with operations for 
;; -- inspecting the board to strategize 
;; -- placing tiles on the board to realize a player's action 
;; the actions do not enforce the rules of the game only consistent placements 

;; also exports tiles+spots: A1 ... I12 via submodule tiles+spots

;; ---------------------------------------------------------------------------------------------------
(require "board-intf.rkt")

(basics& row? string->row column? string->column tile ctile
         tile? tile<=? tile>? tile<? tile->string
         ALL-TILES STARTER-TILES#
         xtile? tile->xexpr 
         
         FOUNDING GROWING MERGING SINGLETON IMPOSSIBLE
         board? board board-tiles 
         draw draw-cell
         what-kind-of-spot growing-which merging-which size-of-hotel free-spot?
         merge-hotels found-hotel grow-hotel place-tile set-board affordable?
         
         xboard? board->xexpr xspot? spot->xexpr
         *create-board-with-hotels distinct-and-properly-formed
         
         board-a1-b2-c6 board-a2-b2-american board-b2-c2-am-c4-d4-tw-e4 board-3way-merger-at-d3)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION: SPOTS

(require "basics.rkt" "Lib/xml.rkt" "Lib/auxiliaries.rkt")

(module+ test (require rackunit))

(module tiles racket
  (provide (all-defined-out))
  
  (require "basics.rkt" "Lib/auxiliaries.rkt" "Lib/xml.rkt" 2htdp/image (only-in srfi/1 list-index))
  
  ;; ---------------------------------------------------------------------------------------------------
  ;; ROWS and COLUMNS
  
  (define COLUMNS (stream->list (in-range 1 13 1)))
  
  (define (column? c)
    (cons? (member c COLUMNS)))
  
  (define (random-column) (randomly-pick COLUMNS))
  
  (define (column-> c)
    (if (eq? (last COLUMNS) c) #f (list-ref COLUMNS (+ (list-index (curry eq? c) COLUMNS) 1))))
  
  (define (column-< c)
    (if (eq? (first COLUMNS) c) #f (list-ref COLUMNS (- (list-index (curry eq? c) COLUMNS) 1))))
  
  ;; Column Column -> Boolean 
  (define column<= <=)
  
  (define (column->text c)
    (text/font (number->string c) 24 "black" #f 'system 'normal 'normal #f))
  
  (define (string->column s)
    (and (string? s)
         (let ()
           (define n (string->number s))
           (and n (member n COLUMNS) n))))
  
  (define (column->string c)
    (number->string c))
  
  (define ROWS '(A B C D E F G H I))
  
  (define (row? r)
    (cons? (member r ROWS)))
  
  (define (random-row) (randomly-pick ROWS))
  
  (define (row-^ r)
    (if (eq? (first ROWS) r) #f (list-ref ROWS (- (list-index (curry eq? r) ROWS) 1))))
  
  (define (row-v r)
    (if (eq? (last ROWS)  r) #f (list-ref ROWS (+ (list-index (curry eq? r) ROWS) 1))))
  
  ;; Row Row -> Boolean 
  ;; does q appear before r in order?
  (define (row<= q r)
    (<= (list-index (curry eq? q) ROWS) (list-index (curry eq? r) ROWS)))
  
  ;; Row Row -> Boolean 
  ;; does q appear strictly before r in order?
  (define (row<< q r)
    (< (list-index (curry eq? q) ROWS) (list-index (curry eq? r) ROWS)))
  
  (define (row->text r)
    (text/font  (symbol->string r) 24 "black" #f 'system 'normal 'normal #f))
  
  (define (string->row s)
    (and (string? s)
         (let ()
           (define n (string->symbol s))
           (and n (member n ROWS) n))))
  
  (define (row->string r)
    (symbol->string r))
  
  (define CELL-SIZE 66)
  
  ;; ---------------------------------------------------------------------------------------------------
  ;; (struct tile (column row) #:prefab-as-list)
  
  (define TILE-SIZE (- CELL-SIZE 3))
  (define TILE-COLOR 'gray)
  (define STARTER-TILES# 6)
  
  (define-syntax-rule
    ;; should be an identifier macro but contracts
    (ctile letter number) (tile/c number 'letter))
  
  (define (tile number letter)
    (tile/c number letter))
  
  ;; (-> column? row? any)
  (define (tile/c c r)
    (if (and (column? c) (row? r))
        (list c r)
        (error 'tile "not (column,row): ~e, ~e" c r)))
  
  (define tile-column first)
  (define tile-row second)
  (define (tile? t)
    (and (pair? t) (pair? (rest t)) (null? (cddr t)) (row? (tile-row t)) (column? (tile-column t)) #t))
  
  (define ALL-TILES 
    (for*/list ((r ROWS) (c COLUMNS))
      (tile c r)))
  
  ;; Tile Tile -> Boolean 
  ;; is t1 closer to the top-left corner than t2? 
  (define (tile<=? t1 t2)
    (or (row<< (tile-row t1) (tile-row t2))
        (and (row<= (tile-row t1) (tile-row t2))
             (column<= (tile-column t1) (tile-column t2)))))
  
  (define (tile<? s t)
    (and (tile<=? s t) (not (equal? s t))))
  
  (define (tile>? s t)
    (and (tile<=? t s) (not (equal? s t))))
  
  (define (tile->string t)
    (format "(~a,~a)" (tile-column t) (tile-row t)))

  (define xtile?
    (xml-predicate (tile ((column string->column) (row string->row)))))
  
  (define (tile->xexpr t)
    `(tile ((column ,(column->string (tile-column t))) (row ,(row->string (tile-row t))))))
  
  (define (draw-grid f)
    (define rows-of-columns 
      (for/list ((r ROWS))
        (define columns 
          (for/list ((c COLUMNS)) 
            (overlay (draw-cell c r) (f c r) (square CELL-SIZE 'outline 'black))))
        (apply beside columns)))
    (apply above rows-of-columns))
  
  (define (draw-tile c r)
    (square TILE-SIZE 'solid TILE-COLOR))
  
  (define draw-cell
    ;; push column # to the right in its space and center the row letter 
    (let* ([row-width (apply max (map (compose image-width row->text) ROWS))]
           [col-width (apply max (map (compose image-width column->text) COLUMNS))]
           [height    (image-height (row->text (first ROWS)))]
           [transprnt 0]
           [space     (rectangle (+ col-width 2 row-width) height transprnt 'white)])
      (case-lambda 
        [(t) (draw-cell (tile-column t) (tile-row t))]
        [(c r)
         (let* ([c (column->text c)]
                [r (row->text r)]
                [x (- col-width (image-width c))]
                [i (place-image/align c x 0 'left 'top space)]
                [x (+ col-width (quotient (- row-width (image-width r)) 2) 1)]
                [i (place-image/align r x 0 'left 'top i)])
           i)])))
  
  (define (draw-hotel h)
    (define c (hotel->color h))
    (overlay (above (text (hotel->label h) 11 c)
                    (square (+ 22 (image-height (column->text (first COLUMNS)))) 'solid 'white))
             (square (- CELL-SIZE 10) 'solid 'white)
             (square (- CELL-SIZE 2) 'solid c))))

(module tiles+spots racket
  ;; define and export: A1 ... I12 as names for matching tiles 
  
  (require (for-syntax racket/syntax (only-in  (submod ".." tiles) ROWS COLUMNS))
           (only-in (submod ".." tiles) ctile))
  
  (define-syntax (define-all-tiles stx)
    #`(begin 
        #,@(for*/list ((r ROWS) (c COLUMNS))
             (define rc (datum->syntax stx (format-symbol "~a~a" r c)))
             #`(begin (provide #,rc) (define #,rc (ctile #,r #,c))))))
  
  (define-all-tiles))

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION: BOARD

(require (submod "." tiles))

;; ---------------------------------------------------------------------------------------------------
;; DATA

(define FOUNDING 'FOUNDING)
(define GROWING 'GROWING)
(define MERGING 'MERGING)
(define SINGLETON 'SINGLETON)
(define IMPOSSIBLE 'IMPOSSIBLE)

;; no tile is placed in this cell 
(define UNTAKEN 'UNTAKEN)

;; a tile is placed but it does not belong to a hotel 
(define TAKEN-NO-HOTEL 'taken-ho-hotel)

;; Content = Hotel | UNTAKEN | TAKEN-NO-HOTEL

;; Board   = [Hashof Tile Content]
;; if a (c,r) key maps to a hotel, it belongs to this hotel; otherwise it is free 

(define board? hash?)

(define (board) #hash())

(define (board-tiles b) (hash-keys b))

;; ---------------------------------------------------------------------------------------------------
;; FUNCTIONS: internal 

;; Tile *-> Board 
;; place tiles; do not found or create hotels 
(define (board* . t*)  
  (for/fold ((b #hash())) ((t (in-list t*)))
    (place-tile b t)))

(define (*create-board-with-hotels lt lh)
  (for/fold ((b (apply board* lt))) ((h lh))
    (define name (first h))
    (define til* (rest h))
    (for/fold ((b b)) ((t (rest h)))
      (hash-set b t name))))

;; Board Column Row -> Content
(define (board-ref b c r)
  (hash-ref b (tile c r) UNTAKEN))

;; Board Column Row -> Content 
(define (board-set b c r [h TAKEN-NO-HOTEL])
  (hash-set b (tile c r) h))

;; Board Column Row [X X X X ->* Y] ->* Y 
;; produce neighbors in North East South and West 
(define (neighbors b c r f)
  (f (north b c r) (east b c r) (south b c r) (west b c r)))

(define (cardinal-direction n-s e-w)
  (lambda (b c r)
    (define north-south (n-s r))
    (define east-west (e-w c))
    (cond 
      [(boolean? north-south) UNTAKEN]
      [(boolean? east-west) UNTAKEN]
      [else (board-ref b east-west north-south)])))

(define north (cardinal-direction row-^ values))
(define south (cardinal-direction row-v values))
(define east (cardinal-direction values column->))
(define west (cardinal-direction values column-<))

;; Board Hotel -> [Listof Tile] | sorted * tile<=?
(define (tiles-with-specific-label b h)
  (sort (for/list ([(s label) b] #:when (equal? h label)) s) tile<=?))

;; ---------------------------------------------------------------------------------------------------
;; FUNCTIONS: external functions  

(define (free-spot? board tile)
  (eq? (board-ref board (tile-column tile) (tile-row tile)) UNTAKEN))

(define (what-kind-of-spot board tile)
  (define column (tile-column tile))
  (define row (tile-row tile))
  (define surroundings (neighbors board column row list))
  (define hotels (apply set (filter hotel? surroundings)))
  (define hotels# (set-count hotels))
  (define neighbor-taken-no-hotel? (ormap (curry eq? TAKEN-NO-HOTEL) surroundings))
  (cond
    [(= hotels# 0) (if neighbor-taken-no-hotel? FOUNDING SINGLETON)]
    [(= hotels# 1) (if neighbor-taken-no-hotel? IMPOSSIBLE GROWING)]
    [(>= hotels# 2)
     (define any-hotel-safe? (for/or ((h hotels)) (>= (size-of-hotel board h) SAFE#)))
     (if (or neighbor-taken-no-hotel? any-hotel-safe?) IMPOSSIBLE MERGING)]))

(define (growing-which board tile)
  ;; the 'first' is guranateed by contract 
  (first (filter hotel? (neighbors board (tile-column tile) (tile-row tile) list))))

(define (merging-which board tile)
  (define surroundings (neighbors board (tile-column tile) (tile-row tile) list))
  (define hotels (set->list (apply set (filter hotel? surroundings))))
  (define sorted (sort (map (lambda (h) (list h (size-of-hotel board h))) hotels) > #:key second))
  (define partitioned (partition sorted second #:info first))
  (values (first partitioned) (apply append (rest partitioned))))

(define (size-of-hotel board hotel)
  (for/fold ((size 0)) ([(key value) (in-hash board)])
    (if (equal? hotel value) (+ size 1) size)))

(define (grow-hotel board tile)
  (define row (tile-row tile))
  (define column (tile-column tile))
  (define surroundings (neighbors board column row list))
  (define hotel-that-touches (first (filter hotel? surroundings)))
  (board-set board column row hotel-that-touches))

(define (merge-hotels board tile hotel)
  (define row (tile-row tile))
  (define column (tile-column tile))
  (define-values (acquirers acquired) (merging-which board tile))
  (define acquired-hotels (append (remq hotel acquirers) acquired))
  (define relabeled-hotel
    (for/hash (([key current-content] (in-hash board)))
      (if (memq current-content acquired-hotels)
          (values key hotel)
          (values key current-content))))
  (board-set relabeled-hotel column row hotel))

(define (found-hotel board tile hotel)
  (define row (tile-row tile))
  (define column (tile-column tile))
  (board-set (hotel-take-over-neighboring-tiles board column row hotel) column row hotel))

;; Board Column Row Hotel -> Board 
;; mark all TAKEN-NO-HOTEL tiles on board reachable from (column,row) as belong to hotel
;; Global Invariant: This region does not touch any other hotels 
(define (hotel-take-over-neighboring-tiles board column row hotel)
  (let loop ((board board) (to-visit (list (tile column row))) (visited '()))
    (cond
      [(empty? to-visit) board]
      [(member (first to-visit) visited) (loop board (rest to-visit) visited)]
      [else 
       (define column (tile-column (first to-visit)))
       (define row (tile-row (first to-visit)))
       (define-values (n e s w) (neighbors board column row values))
       (loop (board-set board column row hotel)
             (append (if (equal? TAKEN-NO-HOTEL n) (list (tile column (row-^ row))) '())
                     (if (equal? TAKEN-NO-HOTEL e) (list (tile (column-> column) row)) '())
                     (if (equal? TAKEN-NO-HOTEL s) (list (tile column (row-v row))) '())
                     (if (equal? TAKEN-NO-HOTEL w) (list (tile (column-< column) row)) '())
                     (rest to-visit))
             (cons (first to-visit) visited))])))

(define (place-tile board tile)
  (define row (tile-row tile))
  (define column (tile-column tile))
  (board-set board column row))

(define (set-board board tile kind hotel)
  (cond
    [(eq? FOUNDING kind) (if hotel (found-hotel board tile hotel) (place-tile board tile))]
    [(eq? MERGING kind) (merge-hotels board tile hotel)]
    [(eq? SINGLETON kind) (place-tile board tile)]
    [(eq? GROWING kind) (grow-hotel board tile)]))

(define (affordable? board hotels budget)
  (define prices (map (lambda (h) (price-per-share h (size-of-hotel board h))) hotels))
  (if (ormap boolean? prices) #f (<= (apply + prices) budget)))

(define (draw board)
  (draw-grid (lambda (c r) 
               (define content (board-ref board c r))
               (cond
                 [(hotel? content) (draw-hotel content)]
                 [(equal? TAKEN-NO-HOTEL content) (draw-tile c r)]
                 [else (draw-cell c r)]))))

(define xboard? (xml-predicate (board () xtile? ... xboard-hotel? ...)))

(define xboard-hotel? (xml-predicate (hotel ((name string->hotel)) xtile? ...)))

(define (board->xexpr b)
  (define unassociated-tiles (map tile->xexpr #;"is sorted" (tiles-with-specific-label b TAKEN-NO-HOTEL)))
  (define tiles-per-hotel (lambda (h) #;"is sorted" (tiles-with-specific-label b h)))
  (define hotels-of-tiles 
    (foldr (lambda (h others) 
             (define tiles #;"is sorted" (tiles-per-hotel h))
             (if (empty? tiles)
                 others
                 (cons `(hotel ((name ,(hotel->label h))) ,@(map tile->xexpr tiles)) others)))
           '()
           #;"is sorted"
           ALL-HOTELS))
  ;; -- IN -- 
  `(board () ,@unassociated-tiles ,@hotels-of-tiles))

(define xspot? 
  (let ((h? string->hotel))
    (xml-predicate
     (singleton ())
     (founding ())
     (growing ((name h?)))
     (merging ((acquirer h?) (acquired1 h?)))
     (merging ((acquirer h?) (acquired1 h?) (acquired2 h?)))
     (merging ((acquirer h?) (acquired1 h?) (acquired2 h?) (acquired3 h?)))
     (merging ((acquirer h?) (acquired1 h?) (acquired2 h?) (acquired3 h?) (acquired4 h?)))
     (impossible ((msg string?)))
     (error ((msg string?))))))

(define (spot->xexpr b tile kind)
  (cond
    [(eq? SINGLETON kind) '(singleton ())]
    [(eq? FOUNDING kind) '(founding ())]
    [(eq? GROWING kind) `(growing ((name ,(hotel->label (growing-which b tile)))))]
    [(eq? MERGING kind) 
     (define-values (acquirers acquired) (merging-which b tile))
     (if (cons? (rest acquirers))
         '(error ((msg "merger of equals: ignore test case")))
         `(merging ((acquirer ,(hotel->label (first acquirers)))
                    ,@(numbered-attributes "acquired~a" hotel->label acquired))))]
    [(eq? IMPOSSIBLE kind) `(impossible ((msg "safe hotels may not be involved in a merger")))]))

;; ---------------------------------------------------------------------------------------------------
;; tests

(module+ test
  (require (submod ".." tiles))
  
  (check-equal? (column-> (first COLUMNS)) (second COLUMNS))
  (check-equal? (column-< (first COLUMNS)) #f)
  (check-equal? (column-> (last COLUMNS)) #f)
  (check-equal? (column-< (last COLUMNS)) (second (reverse COLUMNS)))
  
  (check-equal? (row-^ (first ROWS)) #f)
  (check-equal? (row-^ (last ROWS))  (second (reverse ROWS)))
  (check-equal? (row-v (first ROWS)) (second ROWS))
  (check-equal? (row-v (last ROWS))  #f)
  
  (check-true (tile<=? (ctile A 1) (ctile A 1)))
  (check-true (tile<=? (ctile A 1) (ctile A 2)))
  (check-true (tile<=? (ctile A 2) (ctile B 1)))
  (check-true (tile<=? (ctile A 2) (ctile B 8)))
  
  (check-false (tile<=? (ctile A 2) (ctile A 1)))
  (check-false (tile<=? (ctile C 2) (ctile B 1)))
  (check-true (tile? (ctile A 1)))
  
  (define (draw-random-board)
    (draw-grid (lambda (c r) 
                 (cond
                   [(< (random 10) 2) (draw-hotel (random-hotel))]
                   [(< (random 10) 2) (draw-cell c r)]
                   [else (draw-tile c r)]))))
  (draw-random-board)
  )

;; ---------------------------------------------------------------------------------------------------
;; tests and test exports 

(require (submod "." tiles+spots))

(define-syntax (define-board stx)
  (syntax-case stx ()
    [(_ name #:inherit name2 action ...)
     (with-syntax ([it (datum->syntax stx 'it)])
       #'(define name 
           (let* ([it name2]
                  [it action]
                  ...)
             it)))]
    [(_ name (tile ...) action ...)
     (with-syntax ([it (datum->syntax stx 'it)])
       #'(define name 
           (let* ([it (board* tile ...)]
                  [it action]
                  ...)
             it)))]))

(define-board board-a1-b2-c6 (A1 B2 C6))

(define-board board-a2-b2-american (B2)
  (found-hotel it A2 AMERICAN))

(define-board board-b2-c2-am-c4-d4-tw-e4  (B2 D4)
  #| A   B   C   D    E
    1 
    2    x   x
    3
    4        x   x    x
  |#
  (found-hotel it C2 AMERICAN)
  (found-hotel it C4 TOWER)
  (grow-hotel  it E4))

(define-board board-safe-american #:inherit board-b2-c2-am-c4-d4-tw-e4
  (foldl (lambda (t b) (grow-hotel b t)) it (list B4 F4 G4 H4 I4 A4 F5 G5 H5 I5)))

(define-board board-b2-c2-am-b4-c4-tw-d2-d4-e3 (B2 B4)
  #| A   B   C   D   E
    1 
    2    x   x   x
    3                x
    4    x   x   x
  |#
  (found-hotel it C2 AMERICAN)
  (found-hotel it C4 TOWER)
  (grow-hotel  it D2)
  (grow-hotel  it D4)
  (let ([s (what-kind-of-spot it E3)])
    (if (eq? s SINGLETON)
        it
        (error 'board-b2-c2-am-b4-c4-tw-d4-a3 "(E,3) bad: ~e" s)))
  (place-tile  it E3))

(define-board board-3way-merger-at-d3 #:inherit board-b2-c2-am-b4-c4-tw-d2-d4-e3
  (found-hotel it F3 WORLDWIDE)
  (let-values ([(acquirer acquired) (merging-which it D3)])
    (if (and (equal? (sort acquirer hotel<=?) `(,AMERICAN ,TOWER)) (equal? acquired `(,WORLDWIDE)))
        it
        (error 'board-3way-merger-at-d3 "(D,3) bad: ~e" (append acquirer acquired)))))

#| Patrick and Deanna's test case on merging when unassociated tiles could be involved 
D5, D6 - hotel           
D8, D9 - hotel
E7 singleton
|#

(define-board d5-d6-tw-d8-d9-am (D5 D8)
  (found-hotel it D6 TOWER)
  (found-hotel it D9 AMERICAN)
  (place-tile it E7))

(define-board d1-should-merge (C1 E1 D2)
  (place-tile it F2)
  (found-hotel it B1 TOWER)
  (found-hotel it D3 AMERICAN)
  (found-hotel it F1 FESTIVAL))

(module+ test
  (check-equal? (what-kind-of-spot d1-should-merge D1) MERGING)
  
  (check-equal? (what-kind-of-spot d5-d6-tw-d8-d9-am D7) IMPOSSIBLE "due to Patrick and Deanna")
  
  (define b (*create-board-with-hotels (list A2) '()))
  (check-equal? (free-spot? b A1) #t)
  (check-equal? (what-kind-of-spot b A1) FOUNDING)
  
  ;; -------------------------------------------------------------------------------------------------
  ;; create empty board and run basic predicates 
  (define empty-board (board))
  (check-true (board? empty-board))
  (check-true (free-spot? empty-board (tile (random-column) (random-row))))
  (check-equal? (what-kind-of-spot empty-board (tile (random-column) (random-row))) SINGLETON)
  (check-equal? (size-of-hotel empty-board (random-hotel)) 0)
  
  (check-equal? (what-kind-of-spot (board* B2) A1) SINGLETON)
  (check-equal? (what-kind-of-spot (board* B2) A2) FOUNDING)
  
  ;; -------------------------------------------------------------------------------------------------
  ;; place some separate tiles, test basic properties 
  
  (check-false (free-spot? board-a1-b2-c6 A1))
  
  (check-equal? (what-kind-of-spot board-a1-b2-c6 A2) FOUNDING) ;; <--- QUESTION?
  
  (check-equal? (what-kind-of-spot board-a1-b2-c6 D6) FOUNDING)
  (check-equal? (what-kind-of-spot board-a1-b2-c6 C5) FOUNDING)
  (check-equal? (what-kind-of-spot board-a1-b2-c6 B6) FOUNDING)
  (check-equal? (what-kind-of-spot board-a1-b2-c6 C4) SINGLETON)
  
  (check-equal? (what-kind-of-spot board-safe-american C3) IMPOSSIBLE)
  (check-equal? (what-kind-of-spot board-safe-american B3) IMPOSSIBLE)
  
  ;; -------------------------------------------------------------------------------------------------
  ;; found and grow a hotel 
  
  (check-true (board? board-a2-b2-american))
  (check-equal? (size-of-hotel board-a2-b2-american AMERICAN) 2)
  
  (check-equal? (what-kind-of-spot board-a2-b2-american C2) GROWING)
  (check-equal? (what-kind-of-spot board-a2-b2-american B1) GROWING)
  (check-equal? (what-kind-of-spot board-a2-b2-american A1) GROWING)
  
  (define board-1-hotel-grown (grow-hotel board-a2-b2-american A1))
  
  (check-true (board? board-1-hotel-grown))
  (check-equal? (what-kind-of-spot board-a2-b2-american B1) GROWING)
  (check-equal? (what-kind-of-spot board-a2-b2-american C2) GROWING)
  
  ;; -------------------------------------------------------------------------------------------------
  ;; found and grow two hotels, then merge 
  
  (check-true (board? board-b2-c2-am-c4-d4-tw-e4))
  
  (check-equal? (size-of-hotel board-b2-c2-am-c4-d4-tw-e4 AMERICAN) 2)
  (check-equal? (size-of-hotel board-b2-c2-am-c4-d4-tw-e4 TOWER) 3)
  
  (check-equal? (what-kind-of-spot board-b2-c2-am-c4-d4-tw-e4 C3) MERGING)
  
  (call-with-values (lambda () (merging-which board-b2-c2-am-c4-d4-tw-e4 C3))
                    (lambda (winners losers)
                      (check-equal? winners (list TOWER))
                      (check-equal? losers  (list AMERICAN))))
  
  (define board-merged (merge-hotels board-b2-c2-am-c4-d4-tw-e4 C3 TOWER))
  (check-true (board? board-merged))
  (check-equal? (size-of-hotel board-merged TOWER) 6)
  
  (define board-merged-3way (merge-hotels board-3way-merger-at-d3 D3 TOWER))
  (check-true (board? board-merged-3way))
  (check-equal? (size-of-hotel board-merged-3way TOWER) 9)
  
  ;; -------------------------------------------------------------------------------------------------
  ;; due to Deanna and Sarah
  
  (check-equal? (what-kind-of-spot board-b2-c2-am-b4-c4-tw-d2-d4-e3 D3) IMPOSSIBLE)
  (define board-b2-c2-am-b4-c4-tw-d4-e3-d3 (merge-hotels board-b2-c2-am-b4-c4-tw-d2-d4-e3 D3 TOWER))
  
  ;; ---------------------------------------------------------------------------------------------------
  ;; externalize board 
  
  (check-equal? (board->xexpr (board)) '(board ()))
  (check-equal? (board->xexpr board-a1-b2-c6) 
                `(board () ,@(map tile->xexpr (list A1 B2 C6))))
  (check-equal? (board->xexpr board-a2-b2-american)
                `(board () (hotel ((name ,(hotel->label AMERICAN))) ,@(map tile->xexpr (list A2 B2)))))
  (check-equal? (board->xexpr board-b2-c2-am-c4-d4-tw-e4 )
                `(board () 
                        (hotel ((name ,(hotel->label AMERICAN))) ,@(map tile->xexpr (list B2 C2)))
                        (hotel ((name ,(hotel->label TOWER))) ,@(map tile->xexpr (list C4 D4 E4)))))
  (check-equal? (board->xexpr board-b2-c2-am-b4-c4-tw-d2-d4-e3)
                `(board () 
                        ,(tile->xexpr E3)
                        (hotel ((name ,(hotel->label AMERICAN))) ,@(map tile->xexpr (list B2 C2 D2)))
                        (hotel ((name ,(hotel->label TOWER))) ,@(map tile->xexpr (list B4 C4 D4)))))
  )

;; ---------------------------------------------------------------------------------------------------
;; notions of contracts for creating a board with hotels specified as lists

;; (1) the hotels have distinct names 
;; (2) each hotel comes with at least two tiles 
;; (3) the tiles of the hotels and the unassociated tiles are distinct 
;; (4) the tiles of each hotel form connected graphs 
;; (5) no two hotel chains touch each other directly
(define ((distinct-and-properly-formed free-tiles) hotels-as-lists)
  (define hotel-tiles (map rest hotels-as-lists))
  (and (or (distinct (map first hotels-as-lists)) (tee/#f "hotel names not distinct"))
       (or (andmap contains-at-least-two hotel-tiles) (tee/#f "hotels don't have 2 tiles"))
       (or (distinct (apply append free-tiles hotel-tiles)) (tee/#f "hotel & free tiles overlap"))
       (or (andmap connected-graph hotel-tiles) (tee/#f "hotels not graphs"))
       (or (no-two-hotels-touch hotel-tiles) (tee/#f "two hotels touch"))))

(define (tee/#f s)
  (displayln s)
  #f)

;; ---------------------------------------------------------------------------------------------------
;; auxiliary notiones 

;; HT = (cons Tile (cons Tile [Listof Tile])) ;; the tiles of a syntactically well-formed hotel 

;; HT -> Boolean 
(define (contains-at-least-two ht)
  (and (cons? ht) (cons? (rest ht))))

;; HT -> Boolean 
;; do the tiles of a hotel form a connected graph? 
(define (connected-graph hotel-tiles)
  (define start (first hotel-tiles))
  (define remaining (rest hotel-tiles))
  (define next (connected-to start remaining))
  (and (cons? next)
       (let loop ((frontier next) (remaining (remove* next remaining)))
         (cond
           [(empty? remaining) #t]
           [else (define one-step (apply append (map (λ (f) (connected-to f remaining)) frontier)))
                 (and (cons? one-step) (loop one-step (remove* one-step remaining)))]))))

;; [Listof HT] -> Boolean 
;; are any pairs of HTs connected? if so, return #f
(define (no-two-hotels-touch hotel-tiles*)
  (and-over-pairs-of-distinct-hotels 
   (lambda (one other) (not (connected-graph (append one other))))
   hotel-tiles*))

;; [HT HT -> Boolean] [Listof HT] -> Boolean
;; apply f to all pairs of distinct HTs on lh
(define (and-over-pairs-of-distinct-hotels f hotel-tiles*)
  (or (empty? hotel-tiles*)
      (let loop ([preceding '()][current (first hotel-tiles*)][remaining (rest hotel-tiles*)])
        (cond
          [(empty? remaining) #t]
          [else (and (for/and ((h (append preceding remaining))) (f current h))
                     (loop (cons current preceding) (first remaining) (rest remaining)))]))))

;; Tile HT -> [Listof Tile]
;; find all (at most four) tiles in lh that are connected to t
(define (connected-to t hotel-tiles*)
  (define r (tile-row t))
  (define c (tile-column t))
  (define (in r c)
    (cond [(and r c (member (tile c r) hotel-tiles*)) => (compose list first)] [else '()]))
  (append (in (row-^ r) c) (in (row-v r) c) (in r (column-> c)) (in r (column-< c))))

(module+ test
  ;; connected-to 
  (check-equal? (connected-to (ctile A 1) (list (ctile A 2) (ctile C 4) (ctile B 1)))
                (list (ctile B 1) (ctile A 2)))
  (check-equal? (connected-to (ctile A 1) '()) '())
  
  ;; connected-spots
  (check-false (connected-graph (list  (ctile A 1) (ctile A 2) (ctile C 4) (ctile B 1))))
  (check-true (connected-graph (list  (ctile A 1) (ctile C 2) (ctile A 2) (ctile B 2))))
  
  ;; distinct-and-properly-formed 
  (define tiles (list (ctile A 4) (ctile A 5) (ctile A 7) (ctile A 8)))
  (define (hotel n) (cons n (list (ctile A 4) (ctile A 5))))
  (define hotel-1 (hotel AMERICAN))
  (define hotel-2 (hotel CONTINENTAL))
  (define american (cons AMERICAN (list (ctile A 7) (ctile A 8))))
  
  (check-true ((distinct-and-properly-formed '()) (list hotel-2 american)))
  
  (check-false ((distinct-and-properly-formed '()) (list (list AMERICAN))))
  (check-false ((distinct-and-properly-formed '()) (list (list* AMERICAN (list (ctile A 1))))))
  (check-false ((distinct-and-properly-formed '()) (list hotel-1 american)))
  (check-false ((distinct-and-properly-formed tiles) (list hotel-2 american)))
  (check-true ((distinct-and-properly-formed (list (ctile A 6))) (list hotel-2 american)))
  (check-false ((distinct-and-properly-formed '()) (list hotel-1 hotel-2))))
