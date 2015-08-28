#lang typed/racket/base

;; User Interface to `ai.rkt`

(require
  benchmark-util
  racket/set
  (only-in racket/string string-join)
)
(define-type Exp (U exp Ref Lam Call))
(define-type Label Symbol)
(define-type Var Symbol)

(require/typed/check "structs.rkt"
  [#:struct Stx ([label : Label])]
  [#:struct (exp Stx) ()]
  [#:struct (Ref exp) ([var : Var])]
  [#:struct (Lam exp) ([formals : (Listof Var)] [call : Exp])]
  [#:struct (Call Stx) ([fun : Exp] [args : (Listof Exp)])]
)
(require/typed/check "benv.rkt"
  [#:struct Closure
    ([lam : Lam]
     [benv : BEnv])]
  [#:struct Binding
    ([var : Var]
     [time : Time])]
  (empty-benv BEnv)
  (benv-lookup (-> BEnv Var Addr))
  (benv-extend (-> BEnv Var Addr BEnv))
  (benv-extend* (-> BEnv (Listof Var) (Listof Addr) BEnv))
)
(define-type BEnv (HashTable Var Addr))
(define-type Addr Binding)
(define-type Time (Listof Label))
(define-type Denotable (Setof Value))
(define-type Store (HashTable Addr Denotable))
(define-type Value Closure)
(require/typed/check "time.rkt"
  [time-zero Time]
  [k (Parameterof Natural)]
  [tick (-> Stx Time Time)]
  [alloc (-> Time (-> Var Addr))]
)
(require/typed/check "denotable.rkt"
  [#:struct State
    ([call : Exp]
     [benv : BEnv]
     [store : Store]
     [time : Time])]
   [d-bot Denotable]
   [d-join (-> Denotable Denotable Denotable)]
   [empty-store Store]
   [store-lookup (-> Store Addr Denotable)]
   [store-update (-> Store Addr Denotable Store)]
   [store-update* (-> Store (Listof Addr) (Listof Denotable) Store)]
   [store-join (-> Store Store Store)]
)

(require/typed/check "ai.rkt"
  (atom-eval (-> BEnv Store (-> Exp Denotable)))
  (next (-> State (Setof State)))
  (explore (-> (Setof State) (Listof State) (Setof State)))
)
;; ---

(provide
  summarize
  empty-mono-store
  monovariant-value
  monovariant-store
  analyze
  format-mono-store
)

;; =============================================================================

;; -- ui.rkt
(define-type MonoStore (HashTable Var (Setof Exp)))

(: summarize (-> (Setof State) Store))
(define (summarize states)
  (for/fold ([store empty-store])
    ([state (in-set states)])
    (store-join (State-store state) store)))

(: empty-mono-store MonoStore)
(define empty-mono-store (make-immutable-hasheq '()))

(: monovariant-value (-> Value Lam))
(define (monovariant-value v)
  (Closure-lam v))

(: monovariant-store (-> Store MonoStore))
(define (monovariant-store store)
  (: update-lam (-> (Setof Value) (-> (Setof Exp) (Setof Exp))))
  (define ((update-lam vs) b-vs)
    (: v-vs (Setof Lam))
    (define v-vs (list->set (set-map vs monovariant-value)))
    (set-union b-vs v-vs))
  (: default-lam (-> (Setof Exp)))
  (define (default-lam) (set))
  (for/fold ([mono-store empty-mono-store])
    ([(b vs) (in-hash store)])
    (hash-update mono-store
                 (Binding-var b)
                 (update-lam vs)
                 default-lam)))

(: analyze (-> Exp MonoStore))
(define (analyze exp)
  (define init-state (State exp empty-benv empty-store time-zero))
  (define states (explore (set) (list init-state)))
  (define summary (summarize states))
  (define mono-store (monovariant-store summary))
  mono-store)

(: format-mono-store (-> MonoStore String))
(define (format-mono-store ms)
  (: res (Listof String))
  (define res
    (for/list ([(i vs) (in-hash ms)])
      (format "~a:\n~a"
              i
              (string-join
                (for/list : (Listof String) ([v (in-set vs)])
                  (format "\t~S" v))
                "\n"))))
  (string-join res "\n"))

