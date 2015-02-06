#lang typed/racket/base

;; Access the fields of a struct by name at runtime.

;; Uses predicates to guess what struct its argument is,
;; then compares strings with statically-known field names.
;; Functions that end with '->' are the specific transition function
;; for a type of zo struct.

(provide
 ;; (-> zo String (values (U zo (Listof zo)) Boolean)))
 ;; Access "structName-fieldName myStruct" at runtime.
 zo-transition
 )

(require racket/match
         (only-in racket/list empty? empty)
         (only-in "zo-string.rkt" zo zo?))

(require/typed compiler/zo-structs
               [#:struct (compilation-top zo) ([max-let-depth : Exact-Nonnegative-Integer]
                                          [prefix : prefix]
                                          [code : (U form Any)])]
               [#:struct (prefix zo) ([num-lifts : Exact-Nonnegative-Integer] 
                                 [toplevels : (Listof (U #f Symbol global-bucket module-variable))] 
                                 [stxs : (Listof stx)])]
               [#:struct (global-bucket zo) ([name : Symbol])]
               [#:struct (module-variable zo) ([modidx : Module-Path-Index] 
                                          [sym : Symbol] 
                                          [pos : Integer] 
                                          [phase : Exact-Nonnegative-Integer]
                                          [constantness : (U #f 'constant 'fixed 
                                                              function-shape
                                                              struct-shape)])]
               [#:struct function-shape ([arity : (U Natural arity-at-least (Listof (U Natural arity-at-least)))]
                                         [preserves-marks? : Boolean])] ;; bennn: got type from (:print-type procedure-arity)
               [#:struct struct-shape ()]
               [#:struct (struct-type-shape struct-shape) ([field-count : Exact-Nonnegative-Integer])]
               [#:struct (constructor-shape struct-shape) ([arity : Exact-Nonnegative-Integer])]
               [#:struct (predicate-shape struct-shape) ()]
               [#:struct (accessor-shape struct-shape) ([field-count : Exact-Nonnegative-Integer])]
               [#:struct (mutator-shape struct-shape) ([field-count : Exact-Nonnegative-Integer])]
               [#:struct (struct-other-shape struct-shape) ()]
               [#:struct (stx zo) ([encoded : wrapped])]
               [#:struct (form zo) ()]
               [#:struct (expr form) ()]
               [#:struct (wrapped zo) ([datum : Any] 
                                  [wraps : (Listof wrap)] 
                                  [tamper-status : (U 'clean 'armed 'tainted)])]
               [#:struct (wrap zo) ()]
               [#:struct (free-id-info zo) ([path0 : Module-Path-Index]
                                       [symbol0 : Symbol]
                                       [path1 : Module-Path-Index]
                                       [symbol1 : Symbol]
                                       [phase0 : (U Integer #f)]
                                       [phase1 : (U Integer #f)]
                                       [phase2 : (U Integer #f)]
                                       [use-current-inspector? : Boolean])]
               [#:struct (all-from-module zo) ([path : Module-Path-Index] 
                                          [phase : (U Integer #f)] 
                                          [src-phase : (U Integer #f)]
                                          [exceptions : (Listof Symbol)]
                                          [prefix : (U Symbol #f)]
                                          [context : (U (Listof Integer) 
                                                         (Vector (Listof Integer) Any))])]
               [#:struct (module-binding zo) ()]
               [#:struct (nominal-path zo) ()]
               [#:struct (provided zo) ([name : Symbol] 
                                   [src : (U Module-Path-Index #f)] 
                                   [src-name : Symbol] 
                                   [nom-src : Any] ; mflatt?: should be (or/c module-path-index? #f)
                                   [src-phase : Exact-Nonnegative-Integer] 
                                   [protected? : Boolean])]
               [#:struct (def-values form) ([ids : (Listof (U toplevel Symbol))]
                                     [rhs : (U expr seq inline-variant Any)])]
               [#:struct (def-syntaxes form) ([ids : (Listof (U toplevel Symbol))]
                                       [rhs : (U expr seq Any)] 
                                       [prefix : prefix] 
                                       [max-let-depth : Exact-Nonnegative-Integer]
                                       [dummy : (U toplevel #f)])]
               [#:struct (seq-for-syntax form) ([forms : (Listof (U form Any))] ; `begin-for-syntax'
                                         [prefix : prefix] 
                                         [max-let-depth : Exact-Nonnegative-Integer]
                                         [dummy : (U toplevel #f)])]
               [#:struct (req form) ([reqs : stx]
                              [dummy : toplevel])]
               [#:struct (seq form) ([forms : (Listof (U form Any))])]
               [#:struct (splice form) ([forms : (Listof (U form Any))])]
               [#:struct (inline-variant form) ([direct : expr]
                                         [inline : expr])]
               [#:struct (mod form) ([name : (U Symbol (Listof Symbol))]
                              [srcname : Symbol]
                              [self-modidx : Module-Path-Index] 
                              [prefix : prefix] 
                              [provides : (Listof (List (U Integer #f)
                                                        (Listof provided)
                                                        (Listof provided)))] 
                              [requires : (Listof (Pair (U Integer #f)
                                                        (Listof Module-Path-Index)))]
                              [body : (Listof (U form Any))]
                              [syntax-bodies : (Listof (Pair Exact-Positive-Integer
                                                             (Listof (U def-syntaxes seq-for-syntax))))]
                              [unexported : (Listof (List Exact-Nonnegative-Integer
                                                          (Listof Symbol)
                                                          (Listof Symbol)))]
                              [max-let-depth : Exact-Nonnegative-Integer]
                              [dummy : toplevel]
                              [lang-info : (U #f (Vector Module-Path Symbol Any))]
                              [internal-context : (U #f #t stx (Vectorof stx))]
                              [flags : (Listof (U 'cross-phase))]
                              [pre-submodules : (Listof mod)]
                              [post-submodules : (Listof mod)])]
               [#:struct (lam expr) ([name : (U Symbol (Vectorof Any) (List ))] ;empty list
                              [flags : (Listof (U 'preserves-marks 'is-method 'single-result
                                                   'only-rest-arg-not-used 'sfs-clear-rest-args))]
                              [num-params : Exact-Nonnegative-Integer]
                              [param-types : (Listof (U 'val 'ref 'flonum 'fixnum 'extflonum))]
                              [rest? : Boolean]
                              [closure-map : (Vectorof Exact-Nonnegative-Integer)]
                              [closure-types : (Listof (U 'val/ref 'flonum 'fixnum 'extflonum))]
                              [toplevel-map : (U #f (Setof Exact-Nonnegative-Integer))]
                              [max-let-depth : Exact-Nonnegative-Integer]
                              [body : (U expr seq Any)])]
               [#:struct (closure expr) ([code : lam]
                                  [gen-id : Symbol])]
               [#:struct (case-lam expr) ([name : (U Symbol (Vectorof Any) (List ))]
                                   [clauses : (Listof (U lam closure))])]
               [#:struct (let-one expr) ([rhs : (U expr seq Any)]  ; pushes one value onto stack
                                  [body : (U expr seq Any)] 
                                  [type : (U #f 'flonum 'fixnum 'extflonum)]
                                  [unused? : Boolean])]
               [#:struct (let-void expr) ([count : Exact-Nonnegative-Integer]
                                   [boxes? : Boolean]
                                   [body : (U expr seq Any)])]
               [#:struct (install-value expr) ([count : Exact-Nonnegative-Integer]
                                        [pos : Exact-Nonnegative-Integer] 
                                        [boxes? : Boolean] 
                                        [rhs : (U expr seq Any)] 
                                        [body : (U expr seq Any)])]
               [#:struct (let-rec expr) ([procs : (Listof lam)]
                                  [body : (U expr seq Any)])]
               [#:struct (boxenv expr) ([pos : Exact-Nonnegative-Integer]
                                 [body : (U expr seq Any)])]
               [#:struct (localref expr) ([unbox? : Boolean] 
                                   [pos : Exact-Nonnegative-Integer] 
                                   [clear? : Boolean] 
                                   [other-clears? : Boolean] 
                                   [type : (U #f 'flonum 'fixnum 'extflonum)])]
               [#:struct (toplevel expr) ([depth : Exact-Nonnegative-Integer] 
                                   [pos : Exact-Nonnegative-Integer] 
                                   [const? : Boolean] 
                                   [ready? : Boolean])]
               [#:struct (topsyntax expr) ([depth : Exact-Nonnegative-Integer]
                                    [pos : Exact-Nonnegative-Integer]
                                    [midpt : Exact-Nonnegative-Integer])]
               [#:struct (application expr) ([rator : (U expr seq Any)]
                                      [rands : (Listof (U expr seq Any))])]
               [#:struct (branch expr) ([test : (U expr seq Any)]
                                 [then : (U expr seq Any)]
                                 [else : (U expr seq Any)])]
               [#:struct (with-cont-mark expr) ([key : (U expr seq Any)] 
                                         [val : (U expr seq Any)] 
                                         [body : (U expr seq Any)])]
               [#:struct (beg0 expr) ([seq : (Listof (U expr seq Any))])]
               [#:struct (varref expr) ([toplevel : (U toplevel #t)]
                                 [dummy : (U toplevel #f)])]
               [#:struct (assign expr) ([id : toplevel]
                                 [rhs : (U expr seq Any)]
                                 [undef-ok? : Boolean])]
               [#:struct (apply-values expr) ([proc : (U expr seq Any)]
                                       [args-expr : (U expr seq Any)])]
               [#:struct (primval expr) ([id : Exact-Nonnegative-Integer])]
               [#:struct (top-level-rename wrap) ([flag : Boolean])]
               [#:struct (mark-barrier wrap) ([value : Symbol])]
               [#:struct (lexical-rename wrap) ([has-free-id-renames? : Boolean]
                                         [bool2 : Boolean] ; this needs a name
                                         [alist : (Listof 
                                                 (Pair Symbol
                                                         (U
                                                          Symbol
                                                          (Pair
                                                           Symbol
                                                           (U
                                                            (Pair Symbol (U Symbol #f))
                                                            free-id-info)))))])]
               [#:struct (phase-shift wrap) ([amt : (U Integer #f)] 
                                      [src : (U Module-Path-Index #f)] 
                                      [dest : (U Module-Path-Index #f)]
                                      [cancel-id : (U Integer #f)])]
               [#:struct (module-rename wrap) ([phase : (U Integer #f)] 
                                          [kind : (U 'marked 'normal)] 
                                          [set-id : Any] 
                                          [unmarshals : (Listof all-from-module)]
                                          [renames : (Listof (Pair Symbol module-binding))] 
                                          [mark-renames : Any] 
                                          [plus-kern? : Boolean])]
               [#:struct (wrap-mark wrap) ([val : Integer])]
               [#:struct (prune wrap) ([sym : Any])]
               [#:struct (simple-module-binding module-binding) ([path : Module-Path-Index])]
               [#:struct (phased-module-binding module-binding) ([path : Module-Path-Index]
                                                [phase : Integer]
                                                [export-name : Any]
                                                [nominal-path : nominal-path]
                                                [nominal-export-name : Any])]
               [#:struct (exported-nominal-module-binding module-binding) ([path : Module-Path-Index]
                                                          [export-name : Any]
                                                          [nominal-path : nominal-path]
                                                          [nominal-export-name : Any])]
               [#:struct (nominal-module-binding module-binding) ([path : Module-Path-Index]
                                                 [nominal-path : nominal-path])]
               [#:struct (exported-module-binding module-binding) ([path : Module-Path-Index]
                                                  [export-name : Any])]
               [#:struct (simple-nominal-path nominal-path) ([value : Module-Path-Index])]
               [#:struct (imported-nominal-path nominal-path) ([value : Module-Path-Index] 
                                                [import-phase : Integer])]
               [#:struct (phased-nominal-path nominal-path) ([value : Module-Path-Index]
                                              [import-phase : (U #f Integer)]
                                              [phase : Integer])]
               )

;; -----------------------------------------------------------------------------

;; --- API functions

;; Look up the field name `field-name` in the struct `z`.
;; First use predicates to decide what type of struct `z` is,
;; then use string equality to check if `field-name` matches any
;; statically-known name.
;; Return two values.
;; - First is a zo struct or list of zo structs, depending on the
;;   value stored in the field denoted by `field-name`
;; - Second is a boolean indicating success or failure.
;;   On failure, the returned zo struct is `z`.
(: zo-transition (-> zo String (values (U zo (Listof zo)) Boolean)))
(define (zo-transition z field-name)
  ;; (-> zo? string? (values (or/c zo? (listof zo?)) boolean?))
  ;; Check if transition failed or returned a list without any zo, pack result values.
  (match (try-transition z field-name)
    [(? zo? nxt)
     (values nxt #t)]
    [(? list? nxt)
     (match (filter zo? nxt)
       ['() (values z #f)]
       [zs  (values zs #t)])]
    [_
     (values z #f)]))

;; --- dispatch
(: try-transition (-> zo String (U zo (Listof zo) #f)))
(define (try-transition z str)
  (match z
   [(? compilation-top?) (compilation-top-> z str)]
   [(? prefix?) (prefix-> z str)]
   [(? global-bucket?) (global-bucket-> z str)]
   [(? module-variable?) (module-variable-> z str)]
   [(? stx?) (stx-> z str)]
   [(? form?) (form-> z str)]
   [(? wrapped?) (wrapped-> z str)]
   [(? wrap?) (wrap-> z str)]
   [(? free-id-info?) (free-id-info-> z str)]
   [(? all-from-module?) (all-from-module-> z str)]
   [(? module-binding?) (module-binding-> z str)]
   [(? nominal-path?) (nominal-path-> z str)]
   [(? provided?) (provided-> z str)]
   [x #f]
))
(: form-> (-> zo String (U zo (Listof zo) #f)))
(define (form-> z str)
  (match z
   [(? def-values?) (def-values-> z str)]
   [(? def-syntaxes?) (def-syntaxes-> z str)]
   [(? seq-for-syntax?) (seq-for-syntax-> z str)]
   [(? req?) (req-> z str)]
   [(? seq?) (seq-> z str)]
   [(? splice?) (splice-> z str)]
   [(? inline-variant?) (inline-variant-> z str)]
   [(? mod?) (mod-> z str)]
   [(? provided?) (provided-> z str)]
   [(? expr?) (expr-> z str)]
   [x #f]
))
(: expr-> (-> zo String (U zo (Listof zo) #f)))
(define (expr-> z str)
  (match z
   [(? lam?) (lam-> z str)]
   [(? closure?) (closure-> z str)]
   [(? case-lam?) (case-lam-> z str)]
   [(? let-one?) (let-one-> z str)]
   [(? let-void?) (let-void-> z str)]
   [(? install-value?) (install-value-> z str)]
   [(? let-rec?) (let-rec-> z str)]
   [(? boxenv?) (boxenv-> z str)]
   [(? localref?) (localref-> z str)]
   [(? toplevel?) (toplevel-> z str)]
   [(? topsyntax?) (topsyntax-> z str)]
   [(? application?) (application-> z str)]
   [(? branch?) (branch-> z str)]
   [(? with-cont-mark?) (with-cont-mark-> z str)]
   [(? beg0?) (beg0-> z str)]
   [(? varref?) (varref-> z str)]
   [(? assign?) (assign-> z str)]
   [(? apply-values?) (apply-values-> z str)]
   [(? primval?) (primval-> z str)]
   [x #f]
))
(: wrap-> (-> zo String (U zo (Listof zo) #f)))
(define (wrap-> z str)
  (match z
   [(? top-level-rename?) (top-level-rename-> z str)]
   [(? mark-barrier?) (mark-barrier-> z str)]
   [(? lexical-rename?) (lexical-rename-> z str)]
   [(? phase-shift?) (phase-shift-> z str)]
   [(? module-rename?) (module-rename-> z str)]
   [(? wrap-mark?) (wrap-mark-> z str)]
   [(? prune?) (prune-> z str)]
   [x #f]
))
(: module-binding-> (-> zo String (U zo (Listof zo) #f)))
(define (module-binding-> z str)
  (match z
   [(? simple-module-binding?) (simple-module-binding-> z str)]
   [(? phased-module-binding?) (phased-module-binding-> z str)]
   [(? exported-nominal-module-binding?) (exported-nominal-module-binding-> z str)]
   [(? nominal-module-binding?) (nominal-module-binding-> z str)]
   [(? exported-module-binding?) (exported-module-binding-> z str)]
   [x #f]
))
(: nominal-path-> (-> zo String (U zo (Listof zo) #f)))
(define (nominal-path-> z str)
  (match z
   [(? simple-nominal-path?) (simple-nominal-path-> z str)]
   [(? imported-nominal-path?) (imported-nominal-path-> z str)]
   [(? phased-nominal-path?) (phased-nominal-path-> z str)]
   [x #f]
))

;; --- getters
(: compilation-top-> (-> compilation-top String (U zo (Listof zo) #f)))
(define (compilation-top-> z field-name)
  (match field-name
    ["prefix"
     (compilation-top-prefix z)]
    ["code"
     (: res (U form Any))
     (define res (compilation-top-code   z))
     (if (form? res) res #f)]
    [_ #f]))

(: prefix-> (-> prefix String (U zo (Listof zo) #f)))
(define (prefix-> z field-name)
  (define-predicate gb-or-mv? (U global-bucket module-variable))
  (match field-name
    ["toplevels"
     (filter gb-or-mv? (prefix-toplevels z))]
    ["stxs"
     (prefix-stxs z)]
    [_ #f]))

(: global-bucket-> (-> global-bucket String (U zo (Listof zo) #f)))
(define (global-bucket-> z field-name)
  #f)

(: module-variable-> (-> module-variable String (U zo (Listof zo) #f)))
(define (module-variable-> z field-name)
  #f)

(: stx-> (-> stx String (U zo (Listof zo) #f)))
(define (stx-> z field-name)
  (match field-name
    ["encoded"
     (stx-encoded z)]
    [_  #f]))

(: wrapped-> (-> wrapped String (U zo (Listof zo) #f)))
(define (wrapped-> z field-name)
  (match field-name
    ["wraps"
     (wrapped-wraps z)]
    [_ #f]))

(: free-id-info-> (-> free-id-info String (U zo (Listof zo) #f)))
(define (free-id-info-> z field-name)
  #f)

(: all-from-module-> (-> all-from-module String (U zo (Listof zo) #f)))
(define (all-from-module-> z field-name)
  #f)

;; --- form

(: def-values-> (-> def-values String (U zo (Listof zo) #f)))
(define (def-values-> z field-name)
  (match field-name
    ["ids"
     (filter toplevel? (def-values-ids z))]
    ["rhs"
     (match (def-values-rhs z)
       [(or (? expr? rhs) (? seq? rhs) (? inline-variant? rhs))
        rhs]
       [_ #f])]
  [_ #f]))

(: def-syntaxes-> (-> def-syntaxes String (U zo (Listof zo) #f)))
(define (def-syntaxes-> z field-name)
  (match field-name
    ["ids"
     (filter toplevel? (def-syntaxes-ids z))]
    ["rhs"
     (match (def-syntaxes-rhs z)
       [(or (? expr? rhs) (? seq? rhs)) rhs]
       [_ #f])]
    ["prefix"
     (def-syntaxes-prefix z)]
    ["dummy"
     (match (def-syntaxes-dummy z)
       [(? toplevel? dm) dm]
       [_ #f])]
    [_ #f]))

(: seq-for-syntax-> (-> seq-for-syntax String (U zo (Listof zo) #f)))
(define (seq-for-syntax-> z field-name)
  (match field-name
    ["forms"
     (filter form? (seq-for-syntax-forms z))]
    ["prefix"
     (seq-for-syntax-prefix z)]
    ["dummy"
     (match (seq-for-syntax-dummy z)
       [(? toplevel? dm) dm]
       [_ #f])]
    [_ #f]))

(: req-> (-> req String (U zo (Listof zo) #f)))
(define (req-> z field-name)
  (match field-name
    ["reqs"
     (req-reqs z)]
    ["dummy"
     (req-dummy z)]
    [_ #f]))

(: seq-> (-> seq String (U zo (Listof zo) #f)))
(define (seq-> z field-name)
  (match field-name
    ["forms"
     (filter form? (seq-forms z))]
    [_ #f]))

(: splice-> (-> splice String (U zo (Listof zo) #f)))
(define (splice-> z field-name)
  (match field-name
    ["forms"
     (filter form? (splice-forms z))]
    [_ #f]))

(: inline-variant-> (-> inline-variant String (U zo (Listof zo) #f)))
(define (inline-variant-> z field-name)
  (match field-name
    ["direct"
     (inline-variant-direct z)]
    ["inline"
     (inline-variant-inline z)]
    [_ #f]))

(: mod-> (-> mod String (U zo (Listof zo) #f)))
(define (mod-> z field-name)
  (: get-provided (-> (Listof (List (U Integer #f) (Listof provided) (Listof provided))) (Listof provided)))
  (define (get-provided pds)
    (cond [(empty? pds) empty]
          [else (append (cadar pds)
                        (caddar pds)
                        (get-provided (cdr pds)))]))
  (: get-syntaxes (-> (Listof (Pair Exact-Positive-Integer (Listof (U def-syntaxes seq-for-syntax)))) (Listof (U def-syntaxes seq-for-syntax))))
  (define (get-syntaxes sxs)
    (cond [(empty? sxs) empty]
          [else (append (cdar sxs)
                        (get-syntaxes (cdr sxs)))]))
  (match field-name
    ["prefix"
     (mod-prefix z)]
    ["provides"
     (get-provided (mod-provides z))]
    ["body"
     (filter form? (mod-body z))]
    ["syntax-bodies"
     (get-syntaxes (mod-syntax-bodies z))]
    ["dummy"
     (mod-dummy z)]
    ["internal-context"
     (match (mod-internal-context z)
       [(? stx? ic) ic]
       [(? vector? ic) (vector->list ic)]
       [_ #f])]
    ["pre-submodules"
     (mod-pre-submodules z)]
    ["post-submodules"
     (mod-post-submodules z)]
    [_ #f]))

(: provided-> (-> provided String (U zo (Listof zo) #f)))
(define (provided-> z field-name)
  #f)

;; --- expr

(: lam-> (-> lam String (U zo (Listof zo) #f)))
(define (lam-> z field-name)
  (match field-name
    ["body"
     (match (lam-body z)
       [(? expr-or-seq? bd) bd]
       [_x #f])]
    [_ #f]))

(: closure-> (-> closure String (U zo (Listof zo) #f)))
(define (closure-> z field-name)
  (match field-name
    ["code"
     (closure-code z)]
    [_ #f]))

(: case-lam-> (-> case-lam String (U zo (Listof zo) #f)))
(define (case-lam-> z field-name)
  (match field-name
    ["clauses"
     (case-lam-clauses z)]
    [_ #f]))

(: let-one-> (-> let-one String (U zo (Listof zo) #f)))
(define (let-one-> z field-name)
  (match field-name
    ["rhs"
     (match (let-one-rhs z)
       [(? expr-or-seq? rhs) rhs]
       [_ #f])]
    ["body"
     (match (let-one-body z)
       [(? expr-or-seq? body) body]
       [_ #f])]
    [_ #f]))

(: let-void-> (-> let-void String (U zo (Listof zo) #f)))
(define (let-void-> z field-name)
  (match field-name
    ["body"
     (match (let-void-body z)
       [(? expr-or-seq? body) body]
       [_ #f])]
    [_ #f]))

(: install-value-> (-> install-value String (U zo (Listof zo) #f)))
(define (install-value-> z field-name)
  (match field-name
    ["rhs"
     (match (install-value-rhs z)
       [(? expr-or-seq? rhs) rhs]
       [_ #f])]
    ["body"
     (match (install-value-body z)
       [(? expr-or-seq? body) body]
       [_ #f])]
    [_ #f]))

(: let-rec-> (-> let-rec String (U zo (Listof zo) #f)))
(define (let-rec-> z field-name)
  (match field-name
    ["procs"
     (let-rec-procs z)]
    ["body"
     (match (let-rec-body z)
       [(? expr-or-seq? body) body]
       [_ #f])]
    [_ #f]))

(: boxenv-> (-> boxenv String (U zo (Listof zo) #f)))
(define (boxenv-> z field-name)
  (match field-name
    ["body"
     (match (boxenv-body z)
       [(? expr-or-seq? body) body]
       [_ #f])]
    [_ #f]))

(: localref-> (-> localref String (U zo (Listof zo) #f)))
(define (localref-> z field-name)
  #f)

(: toplevel-> (-> toplevel String (U zo (Listof zo) #f)))
(define (toplevel-> z field-name)
  #f)

(: topsyntax-> (-> topsyntax String (U zo (Listof zo) #f)))
(define (topsyntax-> z field-name)
  #f)

(: application-> (-> application String (U zo (Listof zo) #f)))
(define (application-> z field-name)
  (match field-name
    ["rator"
     (match (application-rator z)
       [(? expr-or-seq? rator) rator]
       [_ #f])]
    ["rands"
     (filter expr-or-seq? (application-rands z))]
    [_ #f]))

(: branch-> (-> branch String (U zo (Listof zo) #f)))
(define (branch-> z field-name)
  (match field-name
    ["test"
     (match (branch-test z)
       [(? expr-or-seq? test) test]
       [_ #f])]
    ["then"
     (match (branch-then z)
       [(? expr-or-seq? then) then]
       [_ #f])]
    ["else"
     (match (branch-else z)
       [(? expr-or-seq? el) el]
       [_ #f])]
    [_ #f]))

(: with-cont-mark-> (-> with-cont-mark String (U zo (Listof zo) #f)))
(define (with-cont-mark-> z field-name)
  (match field-name
    ["key"
     (match (with-cont-mark-key z)
       [(? expr-or-seq? key)  key]
       [_ #f])]
    ["val"
     (match (with-cont-mark-val z)
       [(? expr-or-seq? val) val]
       [_ #f])]
    ["body"
     (match (with-cont-mark-body z)
       [(? expr-or-seq? body) body]
       [_ #f])]
    [_ #f]))

(: beg0-> (-> beg0 String (U zo (Listof zo) #f)))
(define (beg0-> z field-name)
  (match field-name
    ["seq" (filter expr-or-seq? (beg0-seq z))]
    [_ #f]))

(: varref-> (-> varref String (U zo (Listof zo) #f)))
(define (varref-> z field-name)
  (match field-name
    ["toplevel"
     (match (varref-toplevel z)
       [(? toplevel? tl) tl]
       [_ #f])]
    ["dummy"
     (match (varref-dummy z)
       [(? toplevel? dm) dm]
       [_ #f])]
    [_ #f]))

(: assign-> (-> assign String (U zo (Listof zo) #f)))
(define (assign-> z field-name)
  (match field-name
    ["id" (assign-id z)]
    ["rhs" (match (assign-rhs z)
             [(? expr-or-seq? rhs) rhs]
             [_ #f])]
    [_ #f]))

(: apply-values-> (-> apply-values String (U zo (Listof zo) #f)))
(define (apply-values-> z field-name)
  (match field-name
    ["proc"
     (match (apply-values-proc z)
       [(? expr-or-seq? proc) proc]
       [_ #f])]
    ["args-expr"
     (match (apply-values-args-expr z)
       [(? expr-or-seq? args-expr) args-expr]
       [_ #f])]
    [_ #f]))

(: primval-> (-> primval String (U zo (Listof zo) #f)))
(define (primval-> z field-name)
  #f)

;; --- wrap

(: top-level-rename-> (-> top-level-rename String (U zo (Listof zo) #f)))
(define (top-level-rename-> z field-name)
  #f)

(: mark-barrier-> (-> mark-barrier String (U zo (Listof zo) #f)))
(define (mark-barrier-> z field-name)
  #f)

(: lexical-rename-> (-> lexical-rename String (U zo (Listof zo) #f)))
(define (lexical-rename-> z field-name)
  (: get-free-id-info (-> (Listof (Pair Symbol (U Symbol (Pair Symbol (U (Pair Symbol (U Symbol #f)) free-id-info))))) (Listof free-id-info)))
  (define (get-free-id-info als)
    (for/list : (Listof free-id-info) ([blah : (Pair Symbol (U Symbol (Pair Symbol (U (Pair Symbol (U Symbol #f)) free-id-info)))) als]
               #:when (and (pair? (cdr blah))
                           (free-id-info? (cddr blah))))
      (cddr blah)))
  (match field-name
    ["alist"
     (get-free-id-info (lexical-rename-alist z))]
    [_ #f]))

(: phase-shift-> (-> phase-shift String (U zo (Listof zo) #f)))
(define (phase-shift-> z field-name)
  #f)

(: module-rename-> (-> module-rename String (U zo (Listof zo) #f)))
(define (module-rename-> z field-name)
  (match field-name
    ["unmarshals" (module-rename-unmarshals z)]
    ["renames"    (for/list : (Listof module-binding) ([mbpair : (Pair Symbol module-binding) (module-rename-renames z)])
                    (cdr mbpair))]
    [_ #f]))

(: wrap-mark-> (-> wrap-mark String (U zo (Listof zo) #f)))
(define (wrap-mark-> z field-name)
  #f)

(: prune-> (-> prune String (U zo (Listof zo) #f)))
(define (prune-> z field-name)
  #f)

;; --- module-binding

(: simple-module-binding-> (-> simple-module-binding String (U zo (Listof zo) #f)))
(define (simple-module-binding-> z field-name)
  #f)

(: phased-module-binding-> (-> phased-module-binding String (U zo (Listof zo) #f)))
(define (phased-module-binding-> z field-name)
  (match field-name
    ["nominal-path" (phased-module-binding-nominal-path z)]
    [_ #f]))

(: exported-nominal-module-binding-> (-> exported-nominal-module-binding String (U zo (Listof zo) #f)))
(define (exported-nominal-module-binding-> z field-name)
  (match field-name
    ["nominal-path" (exported-nominal-module-binding-nominal-path z)]
    [_ #f]))

(: nominal-module-binding-> (-> nominal-module-binding String (U zo (Listof zo) #f)))
(define (nominal-module-binding-> z field-name)
  (match field-name
    ["nominal-path" (nominal-module-binding-nominal-path z)]
    [_ #f]))

(: exported-module-binding-> (-> exported-module-binding String (U zo (Listof zo) #f)))
(define (exported-module-binding-> z field-name)
  #f)

;; --- nominal-path

(: simple-nominal-path-> (-> simple-nominal-path String (U zo (Listof zo) #f)))
(define (simple-nominal-path-> z field-name)
  #f)

(: imported-nominal-path-> (-> imported-nominal-path String (U zo (Listof zo) #f)))
(define (imported-nominal-path-> z field-name)
  #f)

(: phased-nominal-path-> (-> phased-nominal-path String (U zo (Listof zo) #f)))
(define (phased-nominal-path-> z field-name)
  #f)

;; --- helpers

;; True if the argument is an 'expr' or a 'seq' zo struct.
(define-predicate expr-or-seq? (U expr seq))

;; ;; -----------------------------------------------------------------------------
;; ;; --- testing

;; (module+ test
;;   (require rackunit)
;;            ;(only-in syntax/modresolve module-path-index-join))

;;   ;; compilation-top->
;;   (let* ([px (prefix 0 '() '())]
;;          [cd (form)]
;;          [z  (compilation-top 0 px cd)])
;;     (begin (check-equal? (compilation-top-> z "prefix") px)
;;            (check-equal? (compilation-top-> z "code")   cd)
;;            (check-equal? (compilation-top-> z "")       #f)))

;;   ;; prefix->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [gb (global-bucket 'NAME)]
;;          [mv (module-variable mpi 'SYM 0 0 #f)]
;;          [sx (stx (wrapped (void) '() 'tainted))]
;;          [z  (prefix 0
;;                     (list gb mv)
;;                     (list sx))])
;;     (begin (check-equal? (prefix-> z "toplevels") (list gb mv))
;;            (check-equal? (prefix-> z "stxs")      (list sx))
;;            (check-equal? (prefix-> z "num-lifts") #f)
;;            (check-equal? (prefix-> z "")          #f)))

;;   ;; global-bucket->
;;   (let* ([z (global-bucket 'arbitrary-symbol)])
;;     (check-equal? (global-bucket-> z "name") #f))

;;   ;; module-variable->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [fs  (function-shape 1 #f)]
;;          [ss  (struct-shape)] 
;;          [z   (module-variable mpi 'arbitrary 999 9001 fs)]
;;          ;; Testing when 'constantness' is a struct shape (still #f, it's not a zo)
;;          [z*  (module-variable mpi 'arbitrary 999 9001 ss)])
;;     (begin (check-equal? (module-variable-> z "modidx") #f)
;;            (check-equal? (module-variable-> z "sym") #f)
;;            (check-equal? (module-variable-> z "pos") #f)
;;            (check-equal? (module-variable-> z "phase") #f)
;;            (check-equal? (module-variable-> z "constantness") #f)
;;            (check-equal? (module-variable-> z* "constantness") #f)))

;;   ;; stx->
;;   (let* ([wp (wrapped (void) '() 'tainted)]
;;          [z (stx wp)])
;;     (begin (check-equal? (stx-> z "encoded") wp)
;;            (check-equal? (stx-> z "")        #f)))

;;   ;; form-> (this is better tested by the specific tests for 'def-values->', 'req->', ...)
;;   (let* ([z (form)])
;;     (check-equal? (form-> z "") #f))

;;   ;; expr-> (see tests for specific expressions below
;;   (let* ([z (expr)])
;;     (check-equal? (expr-> z "") #f))

;;   ;; wrapped->
;;   (let* ([wps (list (wrap) (wrap) (wrap))]
;;          [z   (wrapped (void) wps 'tainted)])
;;     (begin (check-equal? (wrapped-> z "wraps")        wps)
;;            (check-equal? (wrapped-> z "datum")         #f)
;;            (check-equal? (wrapped-> z "tamper-status") #f)
;;            (check-equal? (wrapped-> z "")              #f)))

;;   ;; wrap-> (see below)
;;   (let* ([z (wrap)])
;;     (check-equal? (wrap-> z "") #f))
  
;;   ;; free-id-info->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z   (free-id-info mpi 'A mpi 'B #f 101 #f #f)])
;;     (begin (check-equal? (free-id-info-> z "path0") #f)
;;            (check-equal? (free-id-info-> z "symbol0") #f)
;;            (check-equal? (free-id-info-> z "path1") #f)
;;            (check-equal? (free-id-info-> z "symbol1") #f)
;;            (check-equal? (free-id-info-> z "phase0") #f)
;;            (check-equal? (free-id-info-> z "phase1") #f)
;;            (check-equal? (free-id-info-> z "phase2") #f)
;;            (check-equal? (free-id-info-> z "use-current-inspector?") #f)
;;            (check-equal? (free-id-info-> z "") #f)))

;;   ;; all-from-module->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z (all-from-module mpi #f #f '() #f '())])
;;     (begin (check-equal? (all-from-module-> z "path") #f)
;;            (check-equal? (all-from-module-> z "phase") #f)
;;            (check-equal? (all-from-module-> z "src-phase") #f)
;;            (check-equal? (all-from-module-> z "exceptions") #f)
;;            (check-equal? (all-from-module-> z "prefix") #f)
;;            (check-equal? (all-from-module-> z "context") #f)
;;            (check-equal? (all-from-module-> z "") #f)))
  
;;   ;; module-binding-> (see below)
;;   (let* ([z (module-binding)])
;;     (check-equal? (module-binding-> z "") #f))
  
;;   ;; nominal-path-> (see below)
;;   (let* ([z (nominal-path)])
;;     (check-equal? (nominal-path-> z "") #f))

;;   ;; def-values->
;;   (let* ([ids (list (toplevel 1 2 #t #f))]
;;          [rhs (expr)]
;;          [z (def-values ids rhs)])
;;     (begin (check-equal? (def-values-> z "ids") ids)
;;            (check-equal? (def-values-> z "rhs") rhs)
;;            (check-equal? (def-values-> z "") #f)))

;;   ;; def-syntaxes->
;;   (let* ([ids (list (toplevel 1 2 #t #f))]
;;          [rhs (expr)]
;;          [px  (prefix 0 '() '())]
;;          [dm  (toplevel 1 1 #t #t)]
;;          [z   (def-syntaxes ids rhs px 42 dm)]
;;          ;; If dummy is false, transition fails
;;          [z*  (def-syntaxes ids rhs px 42 #f)])
;;     (begin (check-equal? (def-syntaxes-> z "ids") ids)
;;            (check-equal? (def-syntaxes-> z "rhs") rhs)
;;            (check-equal? (def-syntaxes-> z "prefix") px)
;;            (check-equal? (def-syntaxes-> z "dummy") dm)
;;            (check-equal? (def-syntaxes-> z "max-let-depth") #f)
;;            (check-equal? (def-syntaxes-> z "") #f)
;;            (check-equal? (def-syntaxes-> z* "dummy") #f)))

;;   ;; seq-for-syntax->
;;   (let* ([fms (list (form))]
;;          [px  (prefix 0 '() '())]
;;          [dm  (toplevel 9 9 #t #t)]
;;          [z   (seq-for-syntax fms px 8 dm)]
;;          ;; should filter non-zo from the forms list
;;          [z*  (seq-for-syntax '(A B C) px 9 dm)])
;;     (begin (check-equal? (seq-for-syntax-> z "forms") fms)
;;            (check-equal? (seq-for-syntax-> z "prefix") px)
;;            (check-equal? (seq-for-syntax-> z "max-let-depth") #f)
;;            (check-equal? (seq-for-syntax-> z "dummy") dm)
;;            (check-equal? (seq-for-syntax-> z "") #f)
;;            (check-equal? (seq-for-syntax-> z* "forms") '())
;;            ;; empty list filtered at toplevel
;;            (let-values ([(ctx* pass?) (zo-transition z* "forms")])
;;              (begin (check-equal? ctx* z*)
;;                     (check-false pass?)))))

;;   ;; req->
;;   (let* ([sx (stx (wrapped 'XXX '() 'clean))]
;;          [dm (toplevel 1 1 #t #t)]
;;          [z  (req sx dm)])
;;     (begin (check-equal? (req-> z "reqs") sx)
;;            (check-equal? (req-> z "dummy") dm)
;;            (check-equal? (req-> z "") #f)))

;;   ;; seq->
;;   (let* ([fms (list (form) (form) (form))]
;;          [z   (seq fms)]
;;          [z*  (seq '(N O T F O R M S))])
;;     (begin (check-equal? (seq-> z "forms") fms)
;;            (check-equal? (seq-> z "") #f)
;;            (check-equal? (seq-> z* "forms") '())
;;            (let-values ([(ctx* pass?) (zo-transition z* "forms")])
;;              (begin (check-equal? ctx* z*)
;;                     (check-false pass?)))))

  
;;   ;; splice->
;;   (let* ([fms (list (form) (form))]
;;          [z   (splice fms)]
;;          [z*  (splice '(X X X))])
;;     (begin (check-equal? (splice-> z "forms") fms)
;;            (check-equal? (splice-> z "") #f)
;;            (check-equal? (splice-> z* "forms") '())
;;            (let-values ([(ctx* pass?) (zo-transition z* "forms")])
;;              (begin (check-equal? ctx* z*)
;;                     (check-false pass?)))))
    
;;   ;; inline-variant->
;;   (let* ([dr (expr)]
;;          [il (expr)]
;;          [z  (inline-variant dr il)])
;;     (begin (check-equal? (inline-variant-> z "direct") dr)
;;            (check-equal? (inline-variant-> z "inline") il)
;;            (check-equal? (inline-variant-> z "") #f)))
         
;;   ;; mod->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [px  (prefix 0 '() '())]
;;          [pd1 (provided 'p1 #f 'B 'C 13 #t)]
;;          [pd2 (provided 'p2 #f 'asdf 'C 6 #f)]
;;          [pd3 (provided 'p3 #f 'B 'lol 832 #t)]
;;          [pd4 (provided 'p4 #f 'R 'xx 1 #t)]
;;          [pvs (list (list #f (list pd1 pd2) (list pd3))
;;                     (list #f (list pd4) '()))]
;;          [bd  (list (form) 'any)]
;;          [ds  (def-syntaxes '() (expr) (prefix 0 '() '()) 1 #f)]
;;          [sfs (seq-for-syntax '() (prefix 0 '() '()) 999 (toplevel 9 9 #t #t))]
;;          [sb  (list (cons 7 (list ds))
;;                     (cons 8 (list sfs)))]
;;          [dm  (toplevel 1 1 #f #f)]
;;          [ic  (stx (wrapped 'dirty '() 'clean))]
;;          [m1  (mod 'm1 'm1src mpi px pvs '() bd sb '() 0 dm #f ic '() '() '())]
;;          [m2  (mod 'm2 'm2src mpi px pvs '() bd sb '() 0 dm #f ic '() '() '())]
;;          [m3  (mod 'm3 'm3src mpi px pvs '() bd sb '() 0 dm #f ic '() '() '())]
;;          [prs (list m1 m2)]
;;          [pts (list m3)]
;;          [z   (mod 'name 'srcname mpi px pvs '() bd sb '() 0 dm #f ic '() prs pts)])
;;     (begin (check-equal? (mod-> z "prefix") px)
;;            (check-equal? (mod-> z "provides") (list pd1 pd2 pd3 pd4))
;;            (check-equal? (mod-> z "body") (list (form)))
;;            (check-equal? (mod-> z "syntax-bodies") (list ds sfs))
;;            (check-equal? (mod-> z "dummy") dm)
;;            (check-equal? (mod-> z "internal-context") ic)
;;            (check-equal? (mod-> z "pre-submodules") prs)
;;            (check-equal? (mod-> z "post-submodules") pts)
;;            (check-equal? (mod-> z "name") #f)
;;            (check-equal? (mod-> z "srcname") #f)
;;            (check-equal? (mod-> z "self-modidx") #f)
;;            (check-equal? (mod-> z "requires") #f)
;;            (check-equal? (mod-> z "unexported") #f)
;;            (check-equal? (mod-> z "max-let-depth") #f)
;;            (check-equal? (mod-> z "lang-info") #f)
;;            (check-equal? (mod-> z "flags") #f)
;;            (check-equal? (mod-> z "") #f)))

;;   ;; provided->
;;   (let* ([z (provided 'name #f 'srcname 'nomnom 12 #t)])
;;     (begin (check-equal? (provided-> z "name") #f)
;;            (check-equal? (provided-> z "src") #f)
;;            (check-equal? (provided-> z "src-name") #f)
;;            (check-equal? (provided-> z "nom-src") #f)
;;            (check-equal? (provided-> z "src-phase") #f)
;;            (check-equal? (provided-> z "protected?") #f)
;;            (check-equal? (provided-> z "") #f)))

;;   ;; lam->
;;   (let* ([bd (expr)]
;;          [z  (lam 'name '() 3 '() #f '#() '() #f 1 bd)])
;;     (begin (check-equal? (lam-> z "body") bd)
;;            (check-equal? (lam-> z "name") #f)
;;            (check-equal? (lam-> z "flags") #f)
;;            (check-equal? (lam-> z "num-params") #f)
;;            (check-equal? (lam-> z "param-types") #f)
;;            (check-equal? (lam-> z "rest?") #f)
;;            (check-equal? (lam-> z "closure-map") #f)
;;            (check-equal? (lam-> z "closure-types") #f)
;;            (check-equal? (lam-> z "toplevel-map") #f)
;;            (check-equal? (lam-> z "max-let-depth") #f)
;;            (check-equal? (lam-> z "") #f)))

;;   ;; closure->
;;   (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
;;          [z  (closure lm 'genid)])
;;     (begin (check-equal? (closure-> z "code") lm)
;;            (check-equal? (closure-> z "gen-id") #f)
;;            (check-equal? (closure-> z "") #f)))

;;   ;; case-lam->
;;   (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
;;          [cl (closure lm 'id)]
;;          [cls (list lm cl lm)]
;;          [z   (case-lam 'name cls)])
;;     (begin (check-equal? (case-lam-> z "clauses") cls)
;;            (check-equal? (case-lam-> z "name") #f)
;;            (check-equal? (case-lam-> z "") #f)))

;;   ;; let-one->
;;   (let* ([rhs (expr)]
;;          [bdy (expr)]
;;          [z   (let-one rhs bdy #f #f)]
;;          ;; Testing any/c rhs and body
;;          [z*  (let-one #f #f #f #f)])
;;     (begin (check-equal? (let-one-> z "rhs") rhs)
;;            (check-equal? (let-one-> z "body") bdy)
;;            (check-equal? (let-one-> z "type") #f)
;;            (check-equal? (let-one-> z "unused?") #f)
;;            (check-equal? (let-one-> z "") #f)
;;            (check-equal? (let-one-> z* "rhs") #f)
;;            (check-equal? (let-one-> z* "body") #f)))

;;   ;; let-void->
;;   (let* ([bdy (expr)]
;;          [z   (let-void 1 #f bdy)]
;;          [z*  (let-void 1 #f #f)])
;;     (begin (check-equal? (let-void-> z "body") bdy)
;;            (check-equal? (let-void-> z "count") #f)
;;            (check-equal? (let-void-> z "boxes") #f)
;;            (check-equal? (let-void-> z "") #f)
;;            (check-equal? (let-void-> z* "body") #f)))

;;   ;; install-value->
;;   (let* ([rhs (expr)]
;;          [bdy (expr)]
;;          [z   (install-value 2 3 #f rhs bdy)]
;;          [z*  (install-value 0 0 #f #f #f)])
;;     (begin (check-equal? (install-value-> z "rhs") rhs)
;;            (check-equal? (install-value-> z "body") bdy)
;;            (check-equal? (install-value-> z "count") #f)
;;            (check-equal? (install-value-> z "pos") #f)
;;            (check-equal? (install-value-> z "boxes?") #f)
;;            (check-equal? (install-value-> z "") #f)
;;            (check-equal? (install-value-> z* "rhs") #f)
;;            (check-equal? (install-value-> z* "body") #f)))

;;   ;; let-rec->
;;   (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
;;          [pcs (list lm lm)]
;;          [bdy (expr)]
;;          [z   (let-rec pcs bdy)]
;;          [z*  (let-rec '() '())])
;;     (begin (check-equal? (let-rec-> z "procs") pcs)
;;            (check-equal? (let-rec-> z "body") bdy)
;;            (check-equal? (let-rec-> z "") #f)
;;            (check-equal? (let-rec-> z* "procs") '())
;;            (check-equal? (let-rec-> z* "body") #f)))

;;   ;; boxenv->
;;   (let* ([bdy (expr)]
;;          [z   (boxenv 2 bdy)]
;;          [z*  (boxenv 3 4)])
;;     (begin (check-equal? (boxenv-> z "body") bdy)
;;            (check-equal? (boxenv-> z "pos") #f)
;;            (check-equal? (boxenv-> z "") #f)
;;            (check-equal? (boxenv-> z* "body") #f)))

;;   ;; localref->
;;   (let ([z (localref #t 1 #t #t #f)])
;;     (begin (check-equal? (localref-> z "unbox?") #f)
;;            (check-equal? (localref-> z "pos") #f)
;;            (check-equal? (localref-> z "clear?") #f)
;;            (check-equal? (localref-> z "other-clears?") #f)
;;            (check-equal? (localref-> z "type") #f)
;;            (check-equal? (localref-> z "") #f)))

;;   ;; toplevel->
;;   (let ([z (toplevel 1 2 #f #f)])
;;     (begin (check-equal? (toplevel-> z "depth") #f)
;;            (check-equal? (toplevel-> z "pos") #f)
;;            (check-equal? (toplevel-> z "const?") #f)
;;            (check-equal? (toplevel-> z "ready?") #f)
;;            (check-equal? (toplevel-> z "") #f)))

;;   ;; topsyntax->
;;   (let ([z (topsyntax 1 2 3)])
;;     (begin (check-equal? (topsyntax-> z "depth") #f)
;;            (check-equal? (topsyntax-> z "pos") #f)
;;            (check-equal? (topsyntax-> z "midpt") #f)
;;            (check-equal? (topsyntax-> z "") #f)))

;;   ;; application->
;;   (let* ([e (expr)]
;;          [s (seq '())]
;;          [z (application s (list e s s '() 'any 54 e))])
;;     (begin (check-equal? (application-> z "rator") s)
;;            (check-equal? (application-> z "rands") (list e s s e))
;;            (check-equal? (application-> z "") #f)))

;;   ;; branch->
;;   (let* ([z (branch (expr) (expr) (expr))]
;;          [z* (branch #f #f #f)])
;;     (begin (check-equal? (branch-> z "test") (expr))
;;            (check-equal? (branch-> z "then") (expr))
;;            (check-equal? (branch-> z "else") (expr))
;;            (check-equal? (branch-> z "") #f)
;;            (check-equal? (branch-> z* "test") #f)
;;            (check-equal? (branch-> z* "then") #f)
;;            (check-equal? (branch-> z* "else") #f)))

;;   ;; with-cont-mark->
;;   (let* ([z (with-cont-mark (expr) (expr) (expr))]
;;          [z* (with-cont-mark #f #f #f)])
;;     (begin (check-equal? (with-cont-mark-> z "key") (expr))
;;            (check-equal? (with-cont-mark-> z "val") (expr))
;;            (check-equal? (with-cont-mark-> z "body") (expr))
;;            (check-equal? (with-cont-mark-> z "") #f)
;;            (check-equal? (with-cont-mark-> z* "key") #f)
;;            (check-equal? (with-cont-mark-> z* "val") #f)
;;            (check-equal? (with-cont-mark-> z* "body") #f)))

;;   ;; beg0->
;;   (let ([z (beg0 (list (expr) 'asdf (expr)))])
;;     (begin (check-equal? (beg0-> z "seq") (list (expr) (expr)))
;;            (check-equal? (beg0-> z "")    #f)))

;;   ;; varref->
;;   (let* ([tl (toplevel 1 1 #f #f)]
;;          [z  (varref tl tl)]
;;          [z* (varref #t #f)])
;;     (begin (check-equal? (varref-> z "toplevel") tl)
;;            (check-equal? (varref-> z "dummy") tl)
;;            (check-equal? (varref-> z "") #f)
;;            (check-equal? (varref-> z* "dummy") #f)
;;            (check-equal? (varref-> z* "toplevel") #f)))

;;   ;; assign->
;;   (let* ([id  (toplevel 1 1 #f #f)]
;;          [rhs (expr)]
;;          [z   (assign id rhs #t)]
;;          [z*  (assign id #f #t)])
;;     (begin (check-equal? (assign-> z "id") id)
;;            (check-equal? (assign-> z "rhs") rhs)
;;            (check-equal? (assign-> z "undef-ok?") #f)
;;            (check-equal? (assign-> z "") #f)
;;            (check-equal? (assign-> z* "rhs") #f)))

;;   ;; apply-values->
;;   (let* ([z (apply-values (expr) (expr))]
;;          [z* (apply-values #f #f)])
;;     (begin (check-equal? (apply-values-> z "proc") (expr))
;;            (check-equal? (apply-values-> z "args-expr") (expr))
;;            (check-equal? (apply-values-> z "") #f)
;;            (check-equal? (apply-values-> z* "proc") #f)
;;            (check-equal? (apply-values-> z* "args-expr") #f)))

;;   ;; primval->
;;   (let ([z (primval 420)])
;;     (begin (check-equal? (primval-> z "id") #f)
;;            (check-equal? (primval-> z "") #f)))

;;   ;; top-level-rename->
;;   (let* ([z (top-level-rename #t)])
;;     (begin (check-equal? (top-level-rename-> z "flag") #f)
;;            (check-equal? (top-level-rename-> z "") #f)))

;;   ;; mark-barrier->
;;   (let* ([z (mark-barrier 'val)])
;;     (begin (check-equal? (mark-barrier-> z "value") #f)
;;            (check-equal? (mark-barrier-> z "") #f)))

;;   ;; lexical-rename->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [fii (free-id-info mpi 'A mpi 'B #f 101 #f #f)]
;;          [alist (list (cons 'A 'B)
;;                       (cons 'C (cons 'D fii))
;;                       (cons 'F (cons 'G (cons 'H 'I))))]
;;          [z (lexical-rename #f #f alist)])
;;     (begin (check-equal? (lexical-rename-> z "alist") (list fii))
;;            (check-equal? (lexical-rename-> z "bool2") #f)
;;            (check-equal? (lexical-rename-> z "has-free-id-renames?") #f)
;;            (check-equal? (lexical-rename-> z "") #f)))

;;   ;; phase-shift->
;;   (let ([z (phase-shift #f #f #f #f)])
;;     (begin (check-equal? (phase-shift-> z "amt") #f)
;;            (check-equal? (phase-shift-> z "src") #f)
;;            (check-equal? (phase-shift-> z "dest") #f)
;;            (check-equal? (phase-shift-> z "cancel-id") #f)
;;            (check-equal? (phase-shift-> z "") #f)))

;;   ;; module-rename->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [ums (list (all-from-module mpi #f #f '() #f '()))]
;;          [bds (list (cons 'A (module-binding)))]
;;          [z (module-rename #f 'marked 'setid ums bds 'any #f)])
;;     (begin (check-equal? (module-rename-> z "unmarshals") ums)
;;            (check-equal? (module-rename-> z "renames") (list (module-binding)))
;;            (check-equal? (module-rename-> z "phase") #f)
;;            (check-equal? (module-rename-> z "kind") #f)
;;            (check-equal? (module-rename-> z "set-id") #f)
;;            (check-equal? (module-rename-> z "mark-renames") #f)
;;            (check-equal? (module-rename-> z "plus-kern") #f)
;;            (check-equal? (module-rename-> z "") #f)))

;;   ;; wrap-mark->
;;   (let ([z (wrap-mark 121)])
;;     (begin (check-equal? (wrap-mark-> z "val") #f)
;;            (check-equal? (wrap-mark-> z "") #f)))

;;   ;; prune->
;;   (let ([z (prune 'anything-at-all)])
;;     (begin (check-equal? (prune-> z "sym") #f)
;;            (check-equal? (prune-> z "") #f)))

;;   ;; simple-module-binding->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z (simple-module-binding mpi)])
;;     (begin (check-equal? (simple-module-binding-> z "path") #f)
;;            (check-equal? (simple-module-binding-> z "") #f)))

;;   ;; phased-module-binding->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [np (nominal-path)]
;;          [z (phased-module-binding mpi 1 'any np 'any2)])
;;     (begin (check-equal? (phased-module-binding-> z "path") #f)
;;            (check-equal? (phased-module-binding-> z "phase") #f)
;;            (check-equal? (phased-module-binding-> z "export-name") #f)
;;            (check-equal? (phased-module-binding-> z "nominal-path") np)
;;            (check-equal? (phased-module-binding-> z "nominal-export-name") #f)
;;            (check-equal? (phased-module-binding-> z "") #f)))

;;   ;; exported-nominal-module-binding->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [np (nominal-path)]
;;          [z (exported-nominal-module-binding mpi 'any np 'any)])
;;     (begin (check-equal? (exported-nominal-module-binding-> z "path") #f)
;;            (check-equal? (exported-nominal-module-binding-> z "export-name") #f)
;;            (check-equal? (exported-nominal-module-binding-> z "nominal-path") np)
;;            (check-equal? (exported-nominal-module-binding-> z "nominal-export-name") #f)
;;            (check-equal? (exported-nominal-module-binding-> z "") #f)))

;;   ;; nominal-module-binding->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [np (nominal-path)]
;;          [z (nominal-module-binding mpi np)])
;;     (begin (check-equal? (nominal-module-binding-> z "path") #f)
;;            (check-equal? (nominal-module-binding-> z "nominal-path") np)
;;            (check-equal? (nominal-module-binding-> z "") #f)))

;;   ;; exported-module-binding->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z (exported-module-binding mpi 'any)])
;;     (begin (check-equal? (exported-module-binding-> z "path") #f)
;;            (check-equal? (exported-module-binding-> z "export-name") #f)
;;            (check-equal? (exported-module-binding-> z "") #f)))

;;   ;; simple-nominal-path->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z (simple-nominal-path mpi)])
;;     (begin (check-equal? (simple-nominal-path-> z "value") #f)
;;            (check-equal? (simple-nominal-path-> z "") #f)))

;;   ;; imported-nominal-path->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z (imported-nominal-path mpi 12423)])
;;     (begin (check-equal? (imported-nominal-path-> z "value") #f)
;;            (check-equal? (imported-nominal-path-> z "import-phase") #f)
;;            (check-equal? (imported-nominal-path-> z "") #f)))

;;   ;; phased-nominal-path->
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z (phased-nominal-path mpi #f 8)])
;;     (begin (check-equal? (phased-nominal-path-> z "value") #f)
;;            (check-equal? (phased-nominal-path-> z "import-phase") #f)
;;            (check-equal? (phased-nominal-path-> z "phase") #f)
;;            (check-equal? (phased-nominal-path-> z "") #f)))

;;   ;; expr-or-seq?
;;   (check-true (expr-or-seq? (expr)))
;;   (check-true (expr-or-seq? (branch #t #t #t)))
;;   (check-true (expr-or-seq? (application (expr) (list expr))))
;;   (check-true (expr-or-seq? (seq '())))

;;   (check-false (expr-or-seq? 'asdf))
;;   (check-false (expr-or-seq? "yolo"))
;;   (check-false (expr-or-seq? (nominal-path)))
;;   (check-false (expr-or-seq? (form)))
;; )
