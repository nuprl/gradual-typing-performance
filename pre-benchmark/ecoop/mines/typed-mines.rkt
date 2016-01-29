
;; An example implementation of the ever-popular Minesweeper game.

;;;;;;;;;;;;;;;;; Configuration ;;;;;;;;;;;;;;;;;;

#lang typed/racket

(require typed/racket/gui/base)
 
;; Layout constants
(define TILE-HW 24)        ; height/width of a tile
(define B-WIDTH 16)        ; number of tiles across
(define B-HEIGHT 16)       ; number of tiles down
(define THE-BOMB-COUNT 30) ; number of bombs to hide

;; Bitmap constants
(define tile-bm (make-object bitmap% "images/tile.png"))
(define lclick-bm (make-object bitmap% "images/lclick-tile.png"))
(define rclick-bm (make-object bitmap% "images/rclick-tile.png"))
(define local-bm (make-object bitmap% "images/local-tile.png"))
(define near-bm (make-object bitmap% "images/near-tile.png"))
(define bomb-bm (make-object bitmap% "images/bomb.png"))
(define explode-bm (make-object bitmap% "images/explode.png"))
(define flag-bm (make-object bitmap% "images/flag.png"))

(define DIGIT-COLOR-NAMES
  ;; 0th is background; 8th is foreground
  (vector "WHITE"  "BLUE"  "FORESTGREEN"  "RED"  "PURPLE" 
          "ORANGE"  "YELLOW"  "BROWN"  "BLACK"))

(define DIGIT-COLORS
  (build-vector 9 (lambda: ([i : Integer])
                    (assert (send the-color-database find-color
                                  (vector-ref DIGIT-COLOR-NAMES i))))))

(define BG-COLOR (vector-ref DIGIT-COLORS 0)) 
(define FG-COLOR (vector-ref DIGIT-COLORS 8))

(define BLACK-COLOR (send the-color-database find-color "BLACK"))

(define BG-PEN (make-object pen% BG-COLOR 1 'solid))
(define FG-PEN (make-object pen% FG-COLOR 1 'solid))

;; A function for looping over numbers:
(: step-while 
   (All (A B) (Integer (Integer Integer -> Boolean) Integer (Integer -> B) (A B -> A) A -> A)))
(define (step-while first test until  f accum init)
  (let loop ([n first][a init])
    (if (test n until)
      (loop (add1 n) (accum a (f n)))
      a)))

;; ;;;;;;;;;;;;;;; Tiles ;;;;;;;;;;;;;;;;;;
(define-type State (U 'covered 'flagged 'semi-flagged 'uncovered))
(define-type HiLite (U 'none 'local 'near 'left 'right))
(define-type Tile:Plain% 
  (Class [set-state (State -> Void)]
         [get-state (-> State)]
         [set-neighbor-bomb-count (Integer -> Void)]
         [get-neighbor-bomb-count (-> Integer)]
         [set-area-hilite (HiLite -> Void)]
         [draw-text-tile ((Instance DC<%>) 
                          Real Real 
                          Nonnegative-Real Nonnegative-Real 
                          (Option HiLite) Boolean 
                          (Option (U String (Instance Bitmap%))) (Option (U String (Instance Color%))) -> Any)]
         [draw ((Instance DC<%>) Real Real Nonnegative-Real Nonnegative-Real (Option HiLite) -> Any)]))

;; Class for a tile object
(: tile:plain% Tile:Plain%) 
(define tile:plain%
  (class object%
    (define: state : State 'covered)     ; 'covered, 'flagged, 'semi-flagged, or 'uncovered
    (define neighbor-bomb-count 0) ; 0 to 8
    (define: area-hilite : HiLite 'none)  ; 'none, 'local, 'near

    (public*
     [set-state (lambda: ([newstate : State]) (set! state newstate))]
     [get-state (lambda () state)]
     [set-neighbor-bomb-count (lambda: ([c : Integer]) (set! neighbor-bomb-count c))]
     [get-neighbor-bomb-count (lambda () neighbor-bomb-count)]
     [set-area-hilite (lambda: ([mode : HiLite]) (set! area-hilite mode))]
     [draw-text-tile
      (lambda (dc x y w h hilite border? str color)
        (if border?
          (send dc draw-bitmap
                (case hilite
                  [(left) lclick-bm]
                  [(right) rclick-bm]
                  [else (case area-hilite
                          [(near) near-bm]
                          [(local) local-bm]
                          [else tile-bm])])
                x y)
          (begin (send dc set-pen BG-PEN)
                 (send dc draw-rectangle x y w h)))
        (when str
          (cond [(string? str)
                 (send dc set-text-foreground (assert (or color FG-COLOR)))
                 ;; Draw text centered in the tile's box:
                 (let-values ([(tw th d a) (send dc get-text-extent str)])
                   (send dc draw-text str
                         (+ x (/ (- w tw) 2))
                         (+ y (/ (- h (- th d)) 2))))]
                [else
                 (send dc draw-bitmap str x y 'solid (assert BLACK-COLOR)
                       (send str get-loaded-mask))])))]
     [draw
      (lambda (dc x y w h hilite)
        (case state
          [(covered) (draw-text-tile dc x y w h hilite #t #f #f)]
          [(flagged) (draw-text-tile dc x y w h hilite #t flag-bm #f)]
          [(semi-flagged) (draw-text-tile dc x y w h hilite #t "?" #f)]
          [(uncovered)
           (draw-text-tile
            dc x y w h #f #f
            (if (zero? neighbor-bomb-count)
              #f
              (number->string neighbor-bomb-count))
            (vector-ref DIGIT-COLORS neighbor-bomb-count))]))])

    (super-instantiate ())))

(define-type Tile:Bomb%
  (Class #:implements Tile:Plain%
         [set-explode-source (Boolean -> Void)]))

;; Class for a tile with a bomb underneath
(: tile:bomb% Tile:Bomb%)
(define tile:bomb%
  (class tile:plain%
    (inherit get-state draw-text-tile)
    (define: explode-source? : Boolean #f) ; draw this bomb as the one that exploded?

    (public*
     [set-explode-source (lambda: ([s? : Boolean]) (set! explode-source? s?))])

    (override*
     [draw
      (lambda (dc x y w h hilite)
        (if (eq? (get-state) 'uncovered)
          (draw-text-tile dc x y w h #f #f
                          (if explode-source? explode-bm bomb-bm) #f)
          (super draw dc x y w h hilite)))])

    (super-instantiate ())))

(define (is-bomb? x)
  (is-a? x tile:bomb%))

;; ;;;;;;;;;;;;;;; Board Operations ;;;;;;;;;;;;;;;;;;
;; A board is a vector of vectors of tiles
(define-type Board (Vectorof (Vectorof (Instance Tile:Plain%))))

(: board (Option Board))
(define board #f) ; initialized by calling make-board!

(: get-tile (Integer Integer -> (Instance Tile:Plain%)))
(define (get-tile x y)
  (vector-ref (vector-ref (assert board) x) y))

(: set-tile! (Integer Integer (Instance Tile:Plain%) -> Void))
(define (set-tile! x y t)
  (vector-set! (vector-ref (assert board) x) y t))

(: do-surrounding 
   (All (A)
        (Integer Integer (A A -> A )A A (Integer Integer -> A) -> A)))
(define (do-surrounding x y accum start default f)
  (step-while -1 <= 1
              (lambda: ([dx : Integer])
                (step-while -1 <= 1
                            (lambda: ([dy : Integer])
                              (if (and (not (and (zero? dx) (zero? dy)))
                                       (< -1 (+ x dx) B-WIDTH)
                                       (< -1 (+ y dy) B-HEIGHT))
                                (f dx dy)
                                default))
                            accum start))
              accum start))


(: count-surrounding-bombs (Integer Integer -> Integer))
(define (count-surrounding-bombs x y)
  (do-surrounding
   x y + 0 0
   (lambda: ([dx : Integer] [dy : Integer]) (if (is-bomb? (get-tile (+ x dx) (+ y dy))) 1 0))))

(: for-each-tile (All (A) (((Instance Tile:Plain%) Integer Integer -> A) -> Void)))
(define (for-each-tile f)
  (step-while 0 < B-WIDTH
              (lambda: ([x : Integer])
                (step-while 0 < B-HEIGHT (lambda: ([y : Integer]) (f (get-tile x y) x y))
                            void (void)))
              void (void)))

(define (make-board!)
  ;; Create the board
  (set! board
        (build-vector B-WIDTH
                      (lambda (i)
                        (build-vector B-HEIGHT
                                      (lambda (j) (make-object tile:plain%))))))
  ;; Randomly insert bombs
  (let loop ([n THE-BOMB-COUNT])
    (unless (zero? n)
      (let rloop ()
        (let* ([x (random B-WIDTH)]
               [y (random B-HEIGHT)]
               [t (get-tile x y)])
          (if (is-a? t tile:bomb%)
            (rloop)
            (begin
              (set-tile! x y (make-object tile:bomb%))
              (loop (sub1 n))))))))
  ;; Set surrounding-bomb counts for each tile:
  (for-each-tile (lambda: ([t : (Instance Tile:Plain%)] [x : Integer] [y : Integer])
                   (send t
                         set-neighbor-bomb-count
                         (count-surrounding-bombs x y)))))

;; ;;;;;;;;;;;;;;; Graphic Interface ;;;;;;;;;;;;;;;;;;

;; Make a frame:
(: frame (Instance (Class #:implements Frame%
                          (augment [on-close (-> Void)]))))
(define frame
  (instantiate
   (class frame%
     (augment*
      [on-close ; stop the timer, in case it's running
       (lambda ()
         (send board-canvas stop-timer)
         (inner (void) on-close))]) ; Note: fixed type bug
     (super-instantiate ()))
   ("Minesweeper")
   [style '(no-resize-border metal)]))

;; Make the row of controls at the top of the frame:
(: panel (Instance Horizontal-Panel%))
(define panel (make-object horizontal-panel% frame))
(send panel stretchable-height #f)
(: make-centering-pane ((Instance Area-Container<%>) -> (Instance Vertical-Pane%)))
(define (make-centering-pane parent)
  (let ([p (make-object vertical-pane% parent)])
    (send p set-alignment 'center 'center)
    p))

(define time-display
  (make-object message% "Time: 00000" (make-centering-pane panel)))
(new button%
     [label "Reset"]
     [parent (make-centering-pane panel)]
     [callback (lambda (b e) (send board-canvas reset))])
(define count-display
  (make-object message% "Count: 000" (make-centering-pane panel)))

(: set-time (Number -> Void))
(define (set-time t)
  (send time-display set-label (string-append "Time: " (number->string t))))
(: set-count (Number -> Void))
(define (set-count c)
  (send count-display set-label (string-append "Bombs: " (number->string c))))

;; Most of the work is in this class, which extends the basic canvas
;;  class for drawing the Minesweeper board and handling clicks.
(define-type Board-Canvas%
  (Class #:implements/inits Canvas%
         [stop-timer (-> Void)]
         [start-timer (-> Void)]
         [end-of-game (Boolean -> Void)]
         [explode (-> Void)]
         [win (-> Void)]
         [reset (-> Void)]
         [show-all-bombs (-> Void)]
         [autoclick-surrounding (Integer Integer -> Void)]
         [change-state ((Instance Tile:Plain%) State State Boolean -> Void)]
         [do-select (Integer Integer Boolean -> Void)]
         [paint-one ((Instance Tile:Plain%) Integer Integer -> Void)]
         [set-near-hilite ((Instance Tile:Plain%) Integer Integer -> Void)]
         [clear-area-hilite (-> Void)]))

(: board-canvas% Board-Canvas%)
(define board-canvas% 
  (class canvas%
    (init parent)
    (inherit get-dc min-client-width min-client-height 
             stretchable-width stretchable-height)
    (define: clicking : (Option (Instance Tile:Plain%)) #f)        ; #t => click in progress
    (define clicking-x 0)       ; x position of click in progress
    (define clicking-y 0)       ; y position of click in progress
    (define: clicking-right? : Boolean #f) ; #t => right-click in progress
    (define: area-hilite : (Option (Instance Tile:Plain%)) #f)     ; tile with mouse pointer over it
    (: area-hilites (Listof (List (Instance Tile:Plain%) Integer Integer)))
    (define area-hilites null)  ; tiles+locs hilited due to mouse-over
    (define: ready? : Boolean #t)          ; #t => accept clicks
    (define: start-time : (Option Integer) #f)      ; time of first click
    (define elapsed-time 0)     ; seconds since first click
    (define: timer : (Option (Instance Timer%)) #f)           ; a timer that updates elapsed-time
    (define bomb-count THE-BOMB-COUNT) ; number of bombs minus the number of flags
    (define cover-count (* B-HEIGHT B-WIDTH)) ; number of uncovered tiles

    (public*
     [stop-timer      ; stop the clock
      (lambda ()
        (when timer
          (let ([timer timer])
            (send (assert timer) stop))
          (set! timer #f)))]
     [start-timer     ; start the clock
      (lambda ()
        (set! start-time (current-seconds))
        (set! timer
              (make-object
               (class timer% '()
                 (super-new)
                 (override*
                  [notify
                   (lambda ()
                     (let* ([start-time start-time]
                            [e (- (current-seconds) (assert start-time))])
                       (when (> e elapsed-time)
                         (set! elapsed-time e)
                         (set-time e))))]))))
        (let ([timer timer])
          (send (assert timer) start 100 #f)))] ; check time roughly every .1 secs
     [end-of-game     ; stop the game
      (lambda (win?)
        (stop-timer)
        (set! ready? #f)
        (set! start-time #f)
        (unless win? (show-all-bombs))
        (set-count THE-BOMB-COUNT))]
     [explode         ; stop the game because the player hit a bomb
      (lambda () (end-of-game #f))]
     [win             ; stop the game because the player won
      (lambda () (end-of-game #t))]
     [reset           ; quit the current game and reset the board
      (lambda ()
        (stop-timer)
        (set! ready? #t)
        (set! start-time #f)
        (set! elapsed-time 0)
        (set! cover-count (* B-HEIGHT B-WIDTH))
        (send dc clear)
        (set-time 0)
        (set! bomb-count THE-BOMB-COUNT)
        (set-count THE-BOMB-COUNT)
        (make-board!)
        (on-paint))]
     [show-all-bombs  ; show the location of each bomb (after end-of-game)
      (lambda ()
        (for-each-tile (lambda: ([t : (Instance Tile:Plain%)] [x : Integer] [y : Integer])
                         (when (is-bomb? t)
                           (change-state t (send t get-state) 'uncovered #f)
                           (paint-one t x y)))))]
     [autoclick-surrounding   ; autoclick tiles (after a 0 tile is uncovered)
      (lambda: ([x : Integer] [y : Integer])
        (do-surrounding
         x y void (void) (void)
         (lambda: ([dx : Integer] [dy : Integer])
           (let* ([x2 (+ x dx)]
                  [y2 (+ y dy)]
                  [t (get-tile x2 y2)]
                  [state (send t get-state)]
                  [nc (send t get-neighbor-bomb-count)])
             (unless (eq? state 'uncovered)
               (change-state t state 'uncovered #t)
               (paint-one t x2 y2)
               (when (zero? nc) (autoclick-surrounding x2 y2)))))))]
     [change-state         ; update counters after a tile changes
      (lambda: ([t : (Instance Tile:Plain%)] [old-state : State] [new-state : State] [update-count? : Boolean])
        (send t set-state new-state)
        (when (and update-count? (not (eq? new-state old-state)))
          (when (eq? new-state 'uncovered)
            (set! cover-count (sub1 cover-count)))
          (when (eq? old-state 'uncovered)
            (set! cover-count (add1 cover-count)))
          (when (eq? new-state 'flagged)
            (set! bomb-count (sub1 bomb-count))
            (set-count bomb-count))
          (when (eq? old-state 'flagged)
            (set! bomb-count (add1 bomb-count))
            (set-count bomb-count))))]
     [do-select            ; handle a click on a tile
      (lambda (x y flag?)
        (let* ([t (get-tile x y)]
               [state (send t get-state)]
               [new-state (case state
                            [(covered) (if flag? 'flagged 'uncovered)]
                            [(flagged) (if flag? 'semi-flagged state)]
                            [(semi-flagged) (if flag? 'covered 'uncovered)]
                            [else state])]
               [nc (send t get-neighbor-bomb-count)]
               [new-uncover? (and (eq? new-state 'uncovered)
                                  (not (eq? state 'uncovered)))]
               [bomb? (is-bomb? t)])
          (change-state t state new-state #t)
          (when (and new-uncover? bomb?) (send (cast t (Instance Tile:Bomb%)) set-explode-source #t))
          (paint-one t x y)
          (when new-uncover?
            (if bomb?
              (explode)
              (begin
                (if (zero? nc)
                  (autoclick-surrounding x y)
                  (set-near-hilite t x y))))
            (when (and ready? (= cover-count THE-BOMB-COUNT)) (win)))))]
     [paint-one        ; draw one tile
      (lambda: ([t : (Instance Tile:Plain%)] [x : Integer] [y : Integer])
        (let ([xloc (* x TILE-HW)]
              [yloc (* y TILE-HW)])
          (send t draw dc xloc yloc TILE-HW TILE-HW
                ;; below should return an (Option HiLite)
                ;; but 'right and 'left are not HiLites
                (and (eq? t clicking) (if clicking-right? 'right 'left)))
          (void)))]
     [set-near-hilite
      (lambda: ([t : (Instance Tile:Plain%)] [x : Integer] [y : Integer])
        (set! area-hilite t)
        (set! area-hilites
              ((inst do-surrounding (Listof (List (Instance Tile:Plain%) Integer Integer)))
               x y append null null
               (lambda: ([dx : Integer] [dy : Integer])
                 (let* ([x (+ x dx)]
                        [y (+ y dy)]
                        [t (get-tile x y)])
                   (if (not (eq? (send t get-state) 'uncovered))
                     (begin
                       (send t set-area-hilite 'near)
                       (paint-one t x y)
                       (list (list t x y)))
                     null))))))]
     [clear-area-hilite
      (lambda ()
        (when area-hilite
          (set! area-hilite #f)
          (for-each (lambda: ([p : (List (Instance Tile:Plain%) Integer Integer)])
                      (send (car p) set-area-hilite 'none)
                      (paint-one (car p) (cadr p) (caddr p)))
                    area-hilites)
          (set! area-hilites null)))])
    (override*
     [on-event        ; handle a click
      (lambda: ([e : (Instance Mouse-Event%)])
        (when ready?
          (unless start-time    ; if the timer's not running, start it
            (when (send e button-down?)
              (start-timer)))
          ;; Find the time for an (x,y) pixel position in the canvas
          (let* ([x (quotient (inexact->exact (floor (send e get-x))) TILE-HW)]
                 [y (quotient (inexact->exact (floor (send e get-y))) TILE-HW)]
                 [t (if (and (< -1 x B-WIDTH) (< -1 y B-HEIGHT))
                      (get-tile x y)
                      #f)]) ; not a tile
            (cond
              [(and clicking (or (not (eq? t clicking))
                                 (not (or (send e button-up?)
                                          (send e dragging?)))))
               ;; We're already in the middle of a click, and the mouse
               ;;   was moved. Paint the tile to show whether releasing the
               ;;   mouse button selects the tile.
               (let ([old clicking])
                 (set! clicking #f)
                 (paint-one (assert old) clicking-x clicking-y))]
              [(and t
                    (not (eq? (send t get-state) 'uncovered))
                    (or (send e button-down?)
                        (and (send e dragging?)
                             (= x clicking-x)
                             (= y clicking-y))))
               ;; Start a click on a covered tile
               (clear-area-hilite)
               (set! clicking t)
               (set! clicking-x x)
               (set! clicking-y y)
               (when (send e button-down?)
                 (set! clicking-right?
                       (or (send e button-down? 'right)
                           (send e get-control-down)
                           (send e get-alt-down)
                           (send e get-meta-down))))
               (paint-one t x y)]
              [(and clicking (send e button-up?))
               ;; User released the button
               (set! clicking #f)
               (do-select x y clicking-right?)]
              [(and (not (send e leaving?))
                    t
                    (eq? (send t get-state) 'uncovered)
                    (positive? (send t get-neighbor-bomb-count)))
               ;; Moving over uncovered number
               (unless (eq? t area-hilite)
                 (clear-area-hilite)
                 (set-near-hilite t x y))]
              [(and (not (send e leaving?))
                    t
                    (not (eq? (send t get-state) 'uncovered)))
               ;; Moving over tile
               (unless (eq? t area-hilite)
                 (clear-area-hilite)
                 (set! area-hilite t)
                 (set! area-hilites (list (list t x y)))
                 (send t set-area-hilite 'local)
                 (paint-one t x y))]
              [else (clear-area-hilite)]))))]
     [on-paint       ; refresh the board
      (lambda () 
        (for-each-tile 
         (lambda: ([tile : (Instance Tile:Plain%)] [x : Integer] [y : Integer])
           (paint-one tile x y))))])

    (super-instantiate (parent))

    ;; Make canvas size always match the board size:
    (min-client-width (* TILE-HW B-WIDTH))
    (min-client-height (* TILE-HW B-HEIGHT))
    (stretchable-width #f)
    (stretchable-height #f)
    (: dc (Instance DC<%>))
    (define dc (get-dc))

    (reset)  ; initialize the game
    (send dc set-font (make-object font% 16 'swiss 'normal 'bold #f 'default #t))
    (send dc set-text-background BG-COLOR)
    (send dc set-brush (assert (send the-brush-list find-or-create-brush
                                     BG-COLOR 'solid)))))

;; Make the board canvas:
(define board-canvas (make-object board-canvas% frame))

;; Show the frame (and handle events):
(send frame show #t)
