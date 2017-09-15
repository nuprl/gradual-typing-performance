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
 require-typed-check
 "../base/types.rkt"
 "board-adapted.rkt"
 "state-adapted.rkt"
 "tree-adapted.rkt"
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
                (send tree nothing-to-place?))
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

