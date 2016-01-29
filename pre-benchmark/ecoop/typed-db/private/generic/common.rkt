#lang typed/racket/base
(require typed/racket/class
         racket/string
         "typed-interfaces.rkt")
(require/typed ffi/unsafe/atomic [start-atomic (-> Void)] [end-atomic (-> Void)])
(provide define-type-table
         dbsystem-base%
         locking%
         debugging%
         transactions%
         statement-cache%
         isolation-symbol->string
         make-sql-classifier
         sql-skip-comments
         make-handler
         guess-socket-path/paths)

;; Common connection-implementation code

;; ----------------------------------------

;; Defining type tables

(define-syntax-rule (define-type-table (type-list
                                        typeid->type
                                        describe-typeid)
                      (typeid type since-version) ...)
  ;; since-version is #f is this library does not support it,
  ;; *DBMS* version number of introduction (0 for "virtually forever")
  (begin
    (define type-list '((type since-version) ...))
    (define (typeid->type x)
      (case x
        ((typeid) 'type) ...
        (else #f)))
    (define (describe-typeid x)
      (let ([t (typeid->type x)]
            [ok? (case x ((typeid) (and since-version #t)) ... (else #f))])
        (list ok? t x)))))

;; ----------------------------------------

(define-type DBSystem-Base%
  (Class [get-known-types (Real -> (Listof String))]
         [get-type-list (-> (Listof (List String Real)))]))

(: dbsystem-base% DBSystem-Base%)
(define dbsystem-base%
  (class object%
    (super-new)
    (define/public (get-known-types version)
      (let* ([all-types (get-type-list)]
             [supported-types
              (filter (lambda ([type+version : (List String Real)])
                        (let ([since-version (cadr type+version)])
                          (and since-version
                               (>= version since-version))))
                      all-types)])
        ((inst sort String String) ((inst map String (List String Real)) car supported-types)
              string<?
              #:key symbol->string
              #:cache-keys? #t)))
    (define/public (get-type-list) null)))

;; ----------------------------------------

;; Notice/notification handler maker

(: make-handler ((U Output-Port 'output 'error (String String -> Void)) String -> (String String -> Void)))
(define (make-handler out header)
  (if (procedure? out)
      (cast out (String String -> Void))
      (lambda (code message)
        (fprintf (case out
                   ((output) (current-output-port))
                   ((error) (current-error-port))
                   (else (assert out output-port?)))
                 "~a: ~a (SQLSTATE ~a)\n" header message code))))

;; ----------------------------------------

;; Socket paths
(: guess-socket-path/paths (Symbol (Listof Path-String) -> (Option Path-String)))
(define (guess-socket-path/paths function paths)
  (or (for/or : (Option Path-String) ([path : Path-String (in-list paths)])
        (and (file-exists? path) path))
      (error function "could not find socket path")))

;; ----------------------------------------
(define-type Debugging%
  (Class (field [DEBUG? Boolean])
         [debug (Boolean -> Void)]
         [dprintf (String Any * -> Void)]))

(: debugging% Debugging%)
(define debugging%
  (class object%
    (super-new)

    (field [DEBUG? #f])

    (define/public (debug debug?)
      (set! DEBUG? debug?))

    (define/public (dprintf fmt . args)
      (when DEBUG? (apply eprintf fmt args)))
    ))

;; ----------------------------------------

(define-type Locking%
  (Class #:implements Debugging%
         [call-with-lock (All (X) (Symbol (-> X) -> X))]
         [call-with-lock* (All (X) (Symbol (-> X) (Option (-> X)) Boolean -> X))]
         [connected? (-> Boolean)]
         [add-delayed-call! ((-> Any) -> Void)]
         [on-break-within-lock (-> Void)]))
(: locking% Locking%)
(define locking%
  (class debugging%

    ;; == Communication locking

    ;; Goal: we would like to be able to detect if a thread has
    ;; acquired the lock and then died, leaving the connection
    ;; permanently locked.
    ;;
    ;; lock-holder=(thread-dead-evt thd) iff thd has acquired inner-lock
    ;;  - lock-holder, inner-lock always modified together within
    ;;    atomic block
    ;;
    ;; Thus if (thread-dead-evt thd) is ready, thd died holding
    ;; inner-lock, so hopelessly locked.
    ;;
    ;; outer-sema = inner-lock
    ;;  - outer-sema, inner-lock always modified together within atomic
    ;;
    ;; The outer-lock just prevents threads from spinning polling
    ;; inner-lock. If a thread gets past outer-lock and dies before
    ;; acquiring inner-lock, ok, because outer-lock still open at that
    ;; point, so other threads can enter outer-lock and acquire inner-lock.

    (define: outer-sema : Semaphore (make-semaphore 1))
    (define: outer-lock : (Rec x (Evtof x)) (semaphore-peek-evt outer-sema))
    (define: inner-lock : Semaphore (make-semaphore 1))
    (define: lock-holder : (Evtof Any) never-evt)

    ;; Delay async calls (eg, notice handler) until unlock
    (define: delayed-async-calls : (Listof (-> Any)) null)

    ;; ----

    ;; LOCKING: requires unlocked
    (define/public (call-with-lock who proc)
      (call-with-lock* who proc #f #t))

    ;; LOCKING: requires unlocked
    (define/public (call-with-lock* who proc hopeless require-connected?) ; formerly define/public-final
      (let ([me (thread-dead-evt (current-thread))]
            [eb? (break-enabled)]
            [result (sync outer-lock lock-holder)])
        (cond [(eq? result outer-lock)
               ;; Got past outer stage
               (break-enabled #f)
               (let ([proceed?
                      (begin (start-atomic)
                             (let ([proceed? (semaphore-try-wait? inner-lock)])
                               (when proceed?
                                 (set! lock-holder me)
                                 (semaphore-wait outer-sema))
                               (end-atomic)
                               proceed?))])
                 (cond [proceed?
                        ;; Acquired lock
                        ;;  - lock-holder = me, and outer-lock is closed again
                        (when (and require-connected? (not (connected?)))
                          (break-enabled eb?)
                          (unlock #f)
                          (error/not-connected who))
                        (with-handlers ([(lambda (e) #t)
                                         (lambda (e)
                                           (when (exn:break? e) (on-break-within-lock))
                                           (unlock #f)
                                           (raise (assert e exn?)))]) ; FIXME: does this cover it?
                          (break-enabled eb?)
                          (begin0 (proc) (unlock #t)))]
                       [else
                        ;; Didn't acquire lock; retry
                        (break-enabled eb?)
                        (call-with-lock* who proc hopeless require-connected?)]))]
              [(eq? result lock-holder)
               ;; Thread holding lock is dead
               (if hopeless (hopeless) (error/hopeless who))]
              [(eq? me lock-holder)
               (error/internal who "attempted to recursively acquire lock")]
              [else
               ;; lock-holder was stale; retry
               (call-with-lock* who proc hopeless require-connected?)])))

    (: unlock (Boolean -> Void))
    (define/private (unlock run-async-calls?)
      (let ([async-calls (reverse delayed-async-calls)])
        (set! delayed-async-calls null)
        (start-atomic)
        (set! lock-holder never-evt)
        (semaphore-post inner-lock)
        (semaphore-post outer-sema)
        (end-atomic)
        (when run-async-calls?
          (for ([ac : (-> Any) (in-list async-calls)]) (call-with-continuation-barrier ac)))))

    ;; needs overriding
    ;; LOCKING: must not block, must not acquire lock
    (define/public (connected?) #f)

    ;; LOCKING: requires locked
    (define/public (add-delayed-call! proc)
      (set! delayed-async-calls (cons proc delayed-async-calls)))

    ;; Called before unlock; makes it easy to disconnect on any break
    ;; within lock.
    ;; LOCKING: called within lock
    (define/public (on-break-within-lock)
      (void))

    (super-new)))

;; ----------------------------------------

(define-type Disconnect%
  (Class #:implements Locking%
         [disconnect (-> Void)]
         [disconnect* (Boolean -> Void)]))

(: disconnect% Disconnect%)
(define disconnect%
  (class locking%
    (inherit dprintf
             call-with-lock*
             connected?)
    (super-new)

    ;; LOCKING: requires unlocked
    (define/public (disconnect)
      (when (connected?)
        (call-with-lock* 'disconnect
                         (lambda () (disconnect* #t))
                         (lambda () (disconnect* #f))
                         #f))
      (void)) ; possible bug fix

    ;; LOCKING: requires locked
    (define/public (disconnect* politely?)
      (dprintf "  ** disconnecting~a\n" (if politely? " politely" ""))
      (void))

    (define/override (on-break-within-lock)
      (dprintf "  ** break occurred within lock\n")
      (disconnect* #f))))

;; ----------------------------------------

(define-type Transactions%
  (Class #:implements Disconnect%
         [get-tx-status (-> (U Boolean 'invalid))]
         [set-tx-status! (Symbol (U Boolean 'invalid) -> Void)]
         [check-valid-tx-status (Symbol -> Void)]
         [transaction-status (Symbol -> Any)]
         [tx-state->string (-> String)]
         [start-transaction (Symbol (Option Symbol) Any Boolean -> Void)]
         [start-transaction* (Symbol (Option Symbol) Any -> (Option String))]
         [end-transaction (Symbol Symbol Boolean -> Any)]
         [end-transaction* (Symbol Symbol (Option String) -> Void)]
         [check-statement/tx (Symbol (Option Symbol) -> Void)]))

(: transactions% Transactions%)
(define transactions%
  (class disconnect%
    (inherit dprintf)
    (inherit-field DEBUG?)

    #|
    A transaction created via SQL is "unmanaged".
    A transaction created via start-tx, call-with-tx is "managed".

    Indicates whether in a transaction (managed or unmanaged) and if
    transaction is valid or invalid.

    Represents the "managed" transaction stack.

    If tx-status = #f, then tx-stack = null (except temporarily,
    within lock). But it is possible for tx-status != #f and
    tx-stack = null; that indicates an unmanaged tx.
    |#

    (define: tx-status : (U 'invalid Boolean) #f)
    (define: tx-stack : (Listof (Pairof (Option String) Boolean)) null)

    (define/public (get-tx-status) tx-status)
    (define/public (set-tx-status! fsym s)
      (set! tx-status s))

    (define/public (check-valid-tx-status fsym)
      (when (eq? tx-status 'invalid)
        (error fsym "current transaction is invalid")))

    ;; ----

    (define/override (call-with-lock fsym proc)
      (super call-with-lock fsym
             (lambda ()
               (begin0 (proc)
                 (when DEBUG? (dprintf "  ** ~a\n" (tx-state->string)))
                 (when (and (eq? tx-status #f) (not (null? tx-stack)))
                   (error/internal fsym "managed transaction unexpectedly closed"))))))

    ;; ----

    (define/public (transaction-status fsym)
      (call-with-lock fsym (lambda () tx-status)))

    (define/public (tx-state->string)
      (string-append (case (transaction-nesting)
                       ((#f) "not in transaction")
                       ((unmanaged) "in unmanaged transaction")
                       ((top-level nested) "in managed transaction"))
                     (let ([savepoints (filter string? ((inst map (Option String) (Pairof (Option String) Boolean)) car tx-stack))])
                       (if (pair? savepoints)
                           (string-append "; savepoints: "
                                          (string-join savepoints ", "))
                           ""))))

    (: transaction-nesting (-> (Option (U 'unmanaged 'top-level 'nested))))
    (define/private (transaction-nesting)
      (cond [(eq? tx-status #f) #f]
            [(null? tx-stack) 'unmanaged]
            [(null? (cdr tx-stack)) 'top-level]
            [else 'nested]))

    ;; ----

    (define/public (start-transaction fsym isolation option cwt?)
      (call-with-lock fsym
        (lambda ()
          (check-valid-tx-status fsym)
          (cond [(not tx-status)
                 (start-transaction* fsym isolation option)
                 (set! tx-stack (list (cons #f cwt?)))]
                [else ;; in transaction
                 (unless (eq? isolation #f)
                   (error/invalid-nested-isolation fsym isolation))
                 (when option
                   (error/nested-tx-option fsym option))
                 (let ([savepoint (start-transaction* fsym 'nested #f)])
                   (set! tx-stack (cons (cons savepoint cwt?) tx-stack)))])))
      (void))

    (define/public (start-transaction* fsym isolation option)
      ;; returns string (savepoint name) if isolation = 'nested, #f otherwise
      (error/internal fsym "not implemented"))

    (define/public (end-transaction fsym mode cwt?)
      (call-with-lock fsym
        (lambda ()
          (unless (eq? mode 'rollback)
            ;; PostgreSQL: otherwise COMMIT statement would cause silent ROLLBACK!
            (check-valid-tx-status fsym))
          (: tx-stack* (Listof (Pairof (Option String) Boolean)))
          (define tx-stack*
            (cond [(and (eq? mode 'rollback) cwt?)
                   ;; Need to rollback any open start-tx transactions within call-with-tx.
                   ;; No need to complain, because cwt/rollback means exn already raised,
                   ;; either by thunk or commit attempt.
                   (let: loop : (Listof (Pairof (Option String) Boolean)) ([tx-stack* tx-stack])
                     (cond [(pair? tx-stack*)
                            (if (cdar tx-stack*)
                                tx-stack*
                                (loop (cdr tx-stack*)))]
                           [else ; previously missing fsym below added
                            (error/internal fsym "unmatched end of call-with-transaction")]))]
                  [else tx-stack]))
          (cond [(pair? tx-stack*)
                 (let ([savepoint (caar tx-stack*)]
                       [saved-cwt? (cdar tx-stack*)])
                   (unless (eq? saved-cwt? cwt?)
                     (case saved-cwt?
                       ((#f) ;; saved-cwt = #f, cwt = #t
                        (error/unclosed-tx fsym mode #t))
                       ((#t) ;; saved-cwt = #t, cwt = #f: possible
                        (error/unbalanced-tx fsym mode #t))))
                   (end-transaction* fsym mode savepoint)
                   (set! tx-stack (cdr tx-stack*)))]
                [else  ;; not in managed transaction
                 (when #f  ;; DISABLED!
                   #|
                   FIXME: Unmatched {commit,rollback}-transaction should
                   probably be illegal outside of transaction for consistency
                   with requirements within call-with-tx. But that would break
                   backwards compatibility, so disabled.
                   |#
                   (error/unbalanced-tx fsym mode #f))
                 (when tx-status
                   ;; Allow closing unmanaged transaction
                   (end-transaction* fsym mode #f))])
          (void))))

    (define/public (end-transaction* fsym mode savepoint)
      (error/internal fsym "not implemented"))

    ;; Used to check whether SQL command is allowed given managed tx status.
    (define/public (check-statement/tx fsym stmt-type)
      #|
      Nested transaction safety

      For simplicity, we put rules for all statement types here, including
      non-standard statements. FIXME: need to decouple eventually.

      if in "unmanaged" top-level transaction
       - allow all SQL commands (but restrict tx functions)
       - yes, even implicit-commit

      if in "managed" top-level transaction (no "managed" savepoints):
       - START not allowed
       - COMMIT, ROLLBACK not allowed (for now!)
       - SAVEPOINT not allowed (for consistency, for ease of stmt cache)
       - RELEASE TO, ROLLBACK TO not allowed (for consistency, for ease of stmt cache)
       - implicit-commit not allowed

      if in nested "managed" transaction (impl as "managed" savepoint):
       - START not allowed
       - COMMIT, ROLLBACK not allowed
       - SAVEPOINT not allowed -- because it could not be used; see next
       - RELEASE TO, ROLLBACK TO not allowed -- because it may cross nesting levels
       - implicit-commit now allowed
      |#

      (define (no! tx-state)
        (error/tx-bad-stmt fsym
                           (or (statement-type->string stmt-type)
                               (case stmt-type
                                 ((implicit-commit) "statement with implicit commit")
                                 (else #f)))
                           tx-state))

      (case (transaction-nesting)
        ((#f)
         (void))
        ((unmanaged)
         (void))
        ((top-level nested)
         (case stmt-type
           ((start)
            (no! "within transaction"))
           ((commit rollback
             savepoint prepare-transaction
             release-savepoint rollback-savepoint
             implicit-commit)
            (no! "within managed transaction"))
           (else (void))))))

    (super-new)))

;; ----------------------------------------

(define-type Statement-Cache%
  (Class #:implements Transactions%
         (init [cache-statements Symbol #:optional])
         (field [cache-statements Symbol]
                [pst-cache (HashTable String (Instance Prepared-Statement<%>))]
                [cache-mode (U 'in-transaction 'always 'never)]
                [cache-flush-next? Boolean]
                [max-cache-size Real])
         [stmt-cache-ctl (Symbol (U 'get 'flush 'in-transaction 'always 'never) -> Any)]
         [get-cached-statement (String -> (Option (Instance Prepared-Statement<%>)))]
         [safe-statement-type? ((Option Symbol) -> (Option (Listof Symbol)))]
         [cache-statement! ((Instance Prepared-Statement<%>) -> Void)]
         [check/invalidate-cache ((U String statement-binding (Instance Prepared-Statement<%>))
                                  -> (Option (HashTable String (Instance Prepared-Statement<%>))))]
         [prepare (Symbol String Boolean -> (Instance Prepared-Statement<%>))]
         [prepare1 (Symbol String Boolean -> (Instance Prepared-Statement<%>))]
         [prepare1* (Symbol String Boolean (Option Symbol) -> (Instance Prepared-Statement<%>))]
         [classify-stmt (String -> (Option Symbol))]))

(: statement-cache% Statement-Cache%)
(define statement-cache%
  (class transactions%
    (init-field [cache-statements 'in-transaction])
    (inherit call-with-lock
             get-tx-status
             check-valid-tx-status
             dprintf)
    (super-new)

    ;; Statement Cache
    ;; updated by prepare; potentially invalidated by query (via check/invalidate-cache)

    (field [pst-cache (ann '#hash() (HashTable String (Instance Prepared-Statement<%>)))]
           [cache-mode 'in-transaction]
           [cache-flush-next? #f]  ;; flush cache on next query
           [max-cache-size 20])

    (: use-cache? (-> Boolean))
    (define/private (use-cache?)
      (and (not cache-flush-next?)
           (case cache-mode
             ((always) #t)
             ((never) #f)
             ((in-transaction) (eq? (get-tx-status) #t)))))

    (define/public (stmt-cache-ctl who mode)
      (cond [(eq? mode 'get) cache-mode]
            [(eq? mode 'flush) (begin (set! cache-flush-next? #t) cache-mode)]
            [(not (eq? mode cache-mode))
             (call-with-lock who
               (lambda ()
                 (set! cache-mode mode)
                 (set! cache-flush-next? #t)
                 cache-mode))]))

    ;; --

    (define/public (get-cached-statement stmt)
      (let ([cached-pst (hash-ref pst-cache stmt (λ () #f))]) ; add thunk to false arg..
        (cond [cached-pst
               (dprintf "  ** using cached statement\n")
               cached-pst]
              [else
               (dprintf "  ** statement not in cache\n")
               #f])))

    (define/public (safe-statement-type? stmt-type)
      (memq stmt-type '(select insert update delete with)))

    (define/public (cache-statement! pst)
      (when (and (use-cache?) (safe-statement-type? (send pst get-stmt-type)))
        (let ([sql (send pst get-stmt)])
          (when sql
            (dprintf "  ** caching statement\n")
            (set! pst-cache (hash-set pst-cache sql pst))))))

    ;; Returns old cache on invalidation, or #f if stmt is safe.
    ;; May also return part of old cache (excluding pst) when cache gets too big.
    (define/public (check/invalidate-cache x)
      #|
      Sufficient to check on every query execution whether statement type is safe
      (ie, SELECT, INSERT, etc). All statements sent as strings are considered
      unsafe, because they're usually transactional SQL.
      |#
      (: invalidate! ((Option (Instance Prepared-Statement<%>)) -> (HashTable String (Instance Prepared-Statement<%>))))
      (define (invalidate! except)
        ;; FIXME: smarter cache ejection (LRU?)
        (dprintf "  ** invalidating statement cache~a\n"
                 (cond [except " (too big)"]
                       [cache-flush-next? " (mode changed)"]
                       [else ""]))
        (let ([cache pst-cache])
          (set! pst-cache (ann '#hash() (HashTable String (Instance Prepared-Statement<%>))))
          (cond [except
                 (cache-statement! except)
                 (hash-remove cache (send except get-stmt))]
                [else
                 cache])))
      (cond [cache-flush-next?
             (invalidate! #f)]
            [(statement-binding? x)
             (check/invalidate-cache (statement-binding-pst x))]
            [(prepared-statement? x)
             (let ([stmt-type (send (cast x (Instance Prepared-Statement<%>)) get-stmt-type)])
               (cond [(safe-statement-type? stmt-type)
                      (if (< (hash-count pst-cache) max-cache-size)
                          #f
                          (invalidate! (cast x (Instance Prepared-Statement<%>))))]
                     [else
                      (invalidate! #f)]))]
            [else (invalidate! #f)]))

    ;; Prepare

    (define/public (prepare fsym stmt close-on-exec?)
      (call-with-lock fsym
        (lambda ()
          (check-valid-tx-status fsym)
          (prepare1 fsym stmt close-on-exec?))))

    (define/public (prepare1 fsym stmt close-on-exec?)
      (cond [(and close-on-exec? (use-cache?))
             (or (get-cached-statement stmt)
                 (let* ([stmt-type (classify-stmt stmt)]
                        [safe? (safe-statement-type? stmt-type)]
                        [pst (prepare1* fsym stmt (if safe? #f close-on-exec?) stmt-type)])
                   (when safe? (cache-statement! pst))
                   pst))]
            [else
             (dprintf "  ** not using statement cache\n")
             (prepare1* fsym stmt close-on-exec? (classify-stmt stmt))]))

    (define/public (prepare1* fsym stmt close-on-exec? stmt-type) ; added stmt-type argument to fix bug
      (error/internal 'prepare1* "not implemented"))

    (define/public (classify-stmt stmt)
      (error/internal 'classify-stmt "not implemented"))

    ))

;; ----------------------------------------

;; Isolation levels

(define (isolation-symbol->string isolation)
  (case isolation
    ((serializable)     "SERIALIZABLE")
    ((repeatable-read)  "REPEATABLE READ")
    ((read-committed)   "READ COMMITTED")
    ((read-uncommitted) "READ UNCOMMITTED")
    (else #f)))

;; ----------------------------------------

;; Simple SQL "parsing" (just classification)

(: make-sql-classifier (-> (Listof (List String Symbol)) [#:hash-comments? Boolean]
                           (->* [String] [Integer] Any)))
(define (make-sql-classifier table-spec
                             #:hash-comments? [hash-comments? #f])
  (: make-sql-regexp (String -> PRegexp))
  (define (make-sql-regexp stmt-str)
    ;; eg, turns "alter table" into #px"^[[:space:]]*(?i:alter)[[:space:]](?i:table)"
    ;; FIXME/TODO: comments (need real tokenizer; keep regexps as fast path?)
    (pregexp
     (apply string-append
            "^"
            (for/list : (Listof String) ([piece : String (in-list (regexp-split #rx" " stmt-str))])
              (format "[[:space:]]*(?i:~a)(?i:[[:space:]]|$)" piece)))))
  (define classifier-table
    (for/list : (Listof (Pairof PRegexp Any)) ([rule-spec (in-list table-spec)])
      (cons (make-sql-regexp (car rule-spec)) (cadr rule-spec))))
  (lambda ([str : String] [start : Integer 0])
    (let ([start (sql-skip-comments str start #:hash-comments? hash-comments?)])
      (let: loop : (Option Any) ([rules : (Listof (Pairof PRegexp Any)) classifier-table])
        (cond [(null? rules) #f]
              [else (let ([rule (car rules)])
                      (if (regexp-match? (car rule) str start)
                          (cdr rule)
                          (loop (cdr rules))))])))))

(: sql-skip-comments (String Integer [#:hash-comments? Boolean] -> Integer)) ; type of regexp-match-positions requires an integer
(define (sql-skip-comments str start #:hash-comments? [hash-comments? #f])
  (define dash-rx    #px"^[[:space:]]*-- [^\n\r]*(?:[\n\r]|$)")
  (define sh-like-rx #px"^[[:space:]]*#[^\n\r]*(?:[\n\r]|$)")
  (define c-like-rx  #px"^[[:space:]]*/\\*(?:[^\\*]|\\*[^/])*\\*/")
  (let: loop : Integer ([start : Integer start])
    (cond [(or (regexp-match-positions dash-rx str start)
               (regexp-match-positions c-like-rx str start)
               (and hash-comments?
                    (regexp-match-positions sh-like-rx str start)))
           => (lambda (pl) (loop (cdar pl)))]
          [else start])))

(define (statement-type->string stmt-type)
  (case stmt-type
    ;; standard
    ((start) "START TRANSACTION")
    ((commit) "COMMIT")
    ((rollback) "ROLLBACK")
    ((savepoint) "SAVEPOINT")
    ((release-savepoint) "RELEASE SAVEPOINT")
    ((rollback-savepoint) "ROLLBACK TO SAVEPOINT")
    ;; postgresql extensions
    ((prepare-transaction) "PREPARE TRANSACTION")
    ;; unknown
    (else #f)))
