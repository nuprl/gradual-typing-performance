#lang typed/racket/base

;; Convert a zo struct to a more readable string representation.

;; Uses predicates to guess which struct we have, then convert the known
;; fields to strings.
;; Printing a field recursively is potentially expensive,
;; so we wrap the computation in a thunk.
;; The macro `lcons` makes thunk creation a little prettier.
;; The function `format-spec` forces these thunks.

;; Documentation for zo structs is online:
;; http://docs.racket-lang.org/raco/decompile.html

(provide
 ;; (: zo->string (->* (zo) (#:deep? Boolean) String))
 ;; Return a string representation of a zo struct
 zo->string
 ;; (: zo->spec (-> zo Spec))
 ;; Return a list-of-strings representation of a zo struct.
 ;; The structure of the list mirrors the structure of the original zo struct.
 zo->spec
 Spec
 zo zo?)

;; --- string specifications

;; A Spec is the name of a zo struct and a list of pairs representing its fields:
;; - The car of each field is the name of that field
;; - The cdr of each field is a thunk for building a representation of the field's value.
;;   If the value is a zo-struct, the thunk should build a Spec
;;   Otherwise, the thunk should build a string
(define-type Spec
  (Rec Spec
   (Pair String (Listof (Pair String (-> (U Spec String)))))))

(require racket/match
         (only-in racket/list   empty?)
         (only-in racket/string string-join)
         (for-syntax racket/base racket/syntax))

(require/typed compiler/zo-structs
               [#:struct zo ()]
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

;; Convert any zo struct to a spec/c representation.
(: zo->spec (-> zo Spec))
(define
  (zo->spec z)
  (: z* (U Spec #f))
  (define z* (try-spec z))
  (if z*
      z*
      (error (format "Cannot format unknown struct ~e" z))))

;; Convert any zo struct to a string.
;; First builds a spec, then forces the thunks in that spec to build a string.
;; If `deep` is `#f`, only formats the name of the struct `z`.
(: zo->string (->* (zo) (#:deep? Boolean) String))
(define
  (zo->string z #:deep? [deep? #t])
  (format-spec deep? (zo->spec z)))

;; --- syntax: lazy cons to delay evaluation of tail

;; Introduces syntax (lcons a:any b:any).
;; Wraps second argument in a thunk.
(define-syntax (lcons stx)
  (syntax-case stx ()
    [(_)       (raise-syntax-error #f "[lcons] Expected two arguments.")]
    [(_ _)     (raise-syntax-error #f "[lcons] Expected two arguments.")]
    [(_ hd tl) #'(cons hd (lambda () tl))]))

;; --- dispatch tables

(: try-spec (-> zo Spec))
(define (try-spec z)
  (match z
   [(? compilation-top?) (compilation-top->spec z)]
   [(? prefix?) (prefix->spec z)]
   [(? global-bucket?) (global-bucket->spec z)]
   [(? module-variable?) (module-variable->spec z)]
   [(? stx?) (stx->spec z)]
   [(? form?) (form->spec z)]
   [(? expr?) (expr->spec z)]
   [(? wrapped?) (wrapped->spec z)]
   [(? wrap?) (wrap->spec z)]
   [(? free-id-info?) (free-id-info->spec z)]
   [(? all-from-module?) (all-from-module->spec z)]
   [(? module-binding?) (module-binding->spec z)]
   [(? nominal-path?) (nominal-path->spec z)]
   [(? provided?) (provided->spec z)]
   [x (error (format "unknown struct ~e" z))]
))
(: form->spec (-> form Spec))
(define (form->spec z)
  (match z
   [(? def-values?) (def-values->spec z)]
   [(? def-syntaxes?) (def-syntaxes->spec z)]
   [(? seq-for-syntax?) (seq-for-syntax->spec z)]
   [(? req?) (req->spec z)]
   [(? seq?) (seq->spec z)]
   [(? splice?) (splice->spec z)]
   [(? inline-variant?) (inline-variant->spec z)]
   [(? mod?) (mod->spec z)]
   [(? provided?) (provided->spec z)]
   [(? expr?) (expr->spec z)]
   [x (error (format "unknown struct ~e" z))]
))
(: expr->spec (-> expr Spec))
(define (expr->spec z)
  (match z
   [(? lam?) (lam->spec z)]
   [(? closure?) (closure->spec z)]
   [(? case-lam?) (case-lam->spec z)]
   [(? let-one?) (let-one->spec z)]
   [(? let-void?) (let-void->spec z)]
   [(? install-value?) (install-value->spec z)]
   [(? let-rec?) (let-rec->spec z)]
   [(? boxenv?) (boxenv->spec z)]
   [(? localref?) (localref->spec z)]
   [(? toplevel?) (toplevel->spec z)]
   [(? topsyntax?) (topsyntax->spec z)]
   [(? application?) (application->spec z)]
   [(? branch?) (branch->spec z)]
   [(? with-cont-mark?) (with-cont-mark->spec z)]
   [(? beg0?) (beg0->spec z)]
   [(? varref?) (varref->spec z)]
   [(? assign?) (assign->spec z)]
   [(? apply-values?) (apply-values->spec z)]
   [(? primval?) (primval->spec z)]
   [x (error (format "unknown struct ~e" z))]
))
(: wrap->spec (-> wrap Spec))
(define (wrap->spec z)
  (match z
   [(? top-level-rename?) (top-level-rename->spec z)]
   [(? mark-barrier?) (mark-barrier->spec z)]
   [(? lexical-rename?) (lexical-rename->spec z)]
   [(? phase-shift?) (phase-shift->spec z)]
   [(? module-rename?) (module-rename->spec z)]
   [(? wrap-mark?) (wrap-mark->spec z)]
   [(? prune?) (prune->spec z)]
   [x (error (format "unknown struct ~e" z))]
))
(: module-binding->spec (-> module-binding Spec))
(define (module-binding->spec z)
  (match z
   [(? simple-module-binding?) (simple-module-binding->spec z)]
   [(? phased-module-binding?) (phased-module-binding->spec z)]
   [(? exported-nominal-module-binding?) (exported-nominal-module-binding->spec z)]
   [(? nominal-module-binding?) (nominal-module-binding->spec z)]
   [(? exported-module-binding?) (exported-module-binding->spec z)]
   [x (error (format "unknown struct ~e" z))]
))
(: nominal-path->spec (-> nominal-path Spec))
(define (nominal-path->spec z)
  (match z
   [(? simple-nominal-path?) (simple-nominal-path->spec z)]
   [(? imported-nominal-path?) (imported-nominal-path->spec z)]
   [(? phased-nominal-path?) (phased-nominal-path->spec z)]
   [x (error (format "unknown struct ~e" z))]
))
;; --- private functions

(: compilation-top->spec (-> compilation-top Spec))
(define
  (compilation-top->spec z)
  (list "compilation-top"
        (lcons "max-let-depth" (number->string     (compilation-top-max-let-depth z)))
        (lcons "prefix"        (prefix->spec      (compilation-top-prefix z)))
        (lcons "code"          (form-or-any->string (compilation-top-code z)))))

(: prefix->spec (-> prefix Spec))
(define
  (prefix->spec z)
  (: tl->spec (-> (U #f Symbol global-bucket module-variable) String))
  (define (tl->spec tl)
    (match tl
      [(? module-variable?)
       (format-spec #f (module-variable->spec tl))]
      [(? global-bucket?)
       (format-spec #f (global-bucket->spec tl))]
      [(? symbol?)
       (symbol->string tl)]
      [#f "#f"]))
  (list "prefix"
        (lcons "num-lifts" (number->string                (prefix-num-lifts z)))
        (lcons "toplevels" (list->string      tl->spec  (prefix-toplevels z)))
        (lcons "stxs"      (listof-zo->string stx->spec (prefix-stxs z)))))

(: global-bucket->spec (-> global-bucket Spec))
(define
  (global-bucket->spec  z)
  (list "global-bucket"
        (lcons "name" (symbol->string (global-bucket-name z)))))

(: module-variable->spec (-> module-variable Spec))
(define
  (module-variable->spec z)
  (: constantness->spec (-> (U #f 'constant 'fixed function-shape struct-shape) String))
  (define (constantness->spec cs)
    (cond [(symbol? cs)         (symbol->string         cs)]
          [(function-shape? cs) (function-shape->spec cs)]
          [(struct-shape? cs)   (struct-shape->spec   cs)]
          [else          "#f"]))
  (list "module-variable"
        (lcons "modidx"       (module-path-index->string (module-variable-modidx z)))
        (lcons "sym"          (symbol->string            (module-variable-sym z)))
        (lcons "pos"          (number->string            (module-variable-pos z)))
        (lcons "phase"        (number->string            (module-variable-phase z)))
        (lcons "constantness" (constantness->spec      (module-variable-constantness z)))))

(: stx->spec (-> stx Spec))
(define
  (stx->spec z)
  (list "stx"
        (lcons "encoded" (wrapped->spec (stx-encoded z)))))

(: wrapped->spec (-> wrapped Spec))
(define
  (wrapped->spec z)
  (list "wrapped"
        (lcons "datum"         (any->string                    (wrapped-datum z)))
        (lcons "wraps"         (listof-zo->string wrap->spec (wrapped-wraps z)))
        (lcons "tamper-status" (symbol->string                 (wrapped-tamper-status z)))))

;; Helper for `free-id-info` and `all-from-module`
(: phase->spec (-> (U Integer #f) String))
(define (phase->spec ph)
  (cond [(number? ph) (number->string ph)]
        [else  (boolean->string ph)]))

(: free-id-info->spec (-> free-id-info Spec))
(define
  (free-id-info->spec z)
  (list "free-id-info"
        (lcons "path0"                  (module-path-index->string (free-id-info-path0 z)))
        (lcons "symbol0"                (symbol->string            (free-id-info-symbol0 z)))
        (lcons "path1"                  (module-path-index->string (free-id-info-path1 z)))
        (lcons "symbol1"                (symbol->string            (free-id-info-symbol1 z)))
        (lcons "phase0"                 (phase->spec             (free-id-info-phase0 z)))
        (lcons "phase1"                 (phase->spec             (free-id-info-phase1 z)))
        (lcons "phase2"                 (phase->spec             (free-id-info-phase2 z)))
        (lcons "use-current-inspector?" (boolean->string           (free-id-info-use-current-inspector? z)))))

(: all-from-module->spec (-> all-from-module Spec))
(define
  (all-from-module->spec z)
  (: prefix->spec (-> (U Symbol #f) String))
  (define (prefix->spec px)
    (if (symbol? px)
        (symbol->string px)
        "#f"))
  (: context->spec (-> (U (Listof Integer) (Vector (Listof Integer) Any)) String))
  (define (context->spec ctx)
    (cond [(eq? #f ctx)  "#f"]
          [(list? ctx)   (list->string number->string ctx)]
          [(vector? ctx) (format-list #:sep " "
                                      (list (list->string number->string (vector-ref ctx 0))
                                            (any->string                 (vector-ref ctx 1))))]))
  (list "all-from-module"
        (lcons "path"      (module-path-index->string (all-from-module-path z)))
        (lcons "phase"     (phase->spec             (all-from-module-phase z)))
        (lcons "src-phase" (phase->spec             (all-from-module-src-phase z)))
        (lcons "exceptions" (list->string symbol->string (all-from-module-exceptions z)))
        (lcons "prefix"    (prefix->spec            (all-from-module-prefix z)))
        (lcons "context"   (context->spec           (all-from-module-context z)))))

;; --- form

(: def-values->spec (-> def-values Spec))
(define
  (def-values->spec z)
  (: toplevel-or-symbol->string (-> (U toplevel Symbol) String))
  (define (toplevel-or-symbol->string tl)
    (match tl
      [(? toplevel?)
       (format-spec #f (toplevel->spec tl))]
      [(? symbol?)
       (symbol->string tl)]))
  (list "def-values"
        (lcons "ids" (list->string toplevel-or-symbol->string (def-values-ids z)))
        (lcons "rhs" (let ([rhs (def-values-rhs z)])
                       (cond [(inline-variant? rhs) (inline-variant->spec rhs)]
                             [else (expr-seq-any->string rhs)])))))

(: def-syntaxes->spec (-> def-syntaxes Spec))
(define
  (def-syntaxes->spec z)
  (: toplevel-or-symbol->string (-> (U toplevel Symbol) String))
  (define (toplevel-or-symbol->string tl)
    (match tl
      [(? toplevel?)
       (format-spec #f (toplevel->spec tl))]
      [(? symbol?)
       (symbol->string tl)]))
  (list "def-syntaxes"
        (lcons "ids"           (list->string toplevel-or-symbol->string (def-syntaxes-ids z)))
        (lcons "rhs"           (expr-seq-any->string                    (def-syntaxes-rhs z)))
        (lcons "prefix"        (prefix->spec                            (def-syntaxes-prefix z)))
        (lcons "max-let-depth" (number->string                          (def-syntaxes-max-let-depth z)))
        (lcons "dummy"         (toplevel-or-any->string                 (def-syntaxes-dummy z)))))

(: seq-for-syntax->spec (-> seq-for-syntax Spec))
(define
  (seq-for-syntax->spec z)
  (list "seq-for-syntax"
        (lcons "forms"         (listof-form-or-any->string (seq-for-syntax-forms z)))
        (lcons "prefix"        (prefix->spec             (seq-for-syntax-prefix z)))
        (lcons "max-let-depth" (number->string             (seq-for-syntax-max-let-depth z)))
        (lcons "dummy"         (toplevel-or-any->string    (seq-for-syntax-dummy z)))))

(: req->spec (-> req Spec))
(define
  (req->spec z)
  (list "req"
        (lcons "reqs"  (stx->spec      (req-reqs z)))
        (lcons "dummy" (toplevel->spec (req-dummy z)))))

(: seq->spec (-> seq Spec))
(define
  (seq->spec z)
  (list "seq"
        (lcons "forms" (listof-form-or-any->string (seq-forms z)))))

(: splice->spec (-> splice Spec))
(define
  (splice->spec z)
  (list "splice"
        (lcons "forms" (listof-form-or-any->string (splice-forms z)))))

(: inline-variant->spec (-> inline-variant Spec))
(define
  (inline-variant->spec z)
  (list "inline-variant"
        (lcons "direct" (expr->spec (inline-variant-direct z)))
        (lcons "inline" (expr->spec (inline-variant-inline z)))))

(: mod->spec (-> mod Spec))
(define
  (mod->spec z)
  (: name->spec (-> (U Symbol (Listof Symbol)) String))
  (define (name->spec nm)
    (match nm
      [(? list?)
       (list->string  symbol->string nm)]
      [(? symbol?)
       (symbol->string nm)]))
  (: unexported->spec (-> (Listof (List Exact-Nonnegative-Integer (Listof Symbol) (Listof Symbol))) String))
  (define (unexported->spec ux)
    (: elem->spec (-> (List Exact-Nonnegative-Integer (Listof Symbol) (Listof Symbol)) String))
    (define (elem->spec e)
      (format-list
       #:sep " "
       (list (number->string              (car e))
             (list->string symbol->string (cadr e))
             (list->string symbol->string (caddr e)))))
    (list->string elem->spec ux))
  (: lang-info->spec (-> (U #f (Vector Module-Path Symbol Any)) String))
  (define (lang-info->spec li)
    (match li
      [(vector mp sym any)
        (format-list
         #:sep " "
         (list (module-path->spec mp)
               (symbol->string    sym)
               (any->string       any)))]
      [#f "#f"]))
  (: provides->spec (-> (Listof (List (U Integer #f) (Listof provided) (Listof provided))) String))
  (define
    (provides->spec pds)
    (: elem->spec (-> (List (U Integer #f) (Listof provided) (Listof provided)) String))
    (define (elem->spec e)
      (format-list
       #:sep " "
       (list (if (number? (car e))
                 (number->string (car e))
                 "#f")
             (listof-zo->string provided->spec (cadr e))
             (listof-zo->string provided->spec (caddr e)))))
    (list->string elem->spec pds))
  (: requires->spec (-> (Listof (Pair (U Integer #f) (Listof Module-Path-Index))) String))
  (define
    (requires->spec rqs)
    (: elem->spec (-> (Pair (U Integer #f) (Listof Module-Path-Index)) String))
    (define (elem->spec e)
      (format-list
       #:sep " "
       (list (if (number? (car e))
                 (number->string (car e))
                 "#f")
             (list->string module-path-index->string (cdr e)))))
    (list->string elem->spec rqs))
  (: syntax-bodies->spec (-> (Listof (Pair Exact-Positive-Integer (Listof (U def-syntaxes seq-for-syntax)))) String))
  (define
    (syntax-bodies->spec sbs)
    (: ds-or-sfs->spec (-> (U def-syntaxes seq-for-syntax) String))
    (define (ds-or-sfs->spec d)
      (cond [(def-syntaxes?   d) (format-spec #f (def-syntaxes->spec d))]
            [(seq-for-syntax? d) (format-spec #f (seq-for-syntax->spec d))]))
    (: elem->spec (-> (Pair Exact-Positive-Integer (Listof (U def-syntaxes seq-for-syntax))) String))
    (define (elem->spec e)
      (format-list
       #:sep " "
       (list (number->string                 (car e))
             (list->string ds-or-sfs->spec (cdr e)))))
    (list->string elem->spec sbs))
  (: internal-context->string (-> (U #f #t stx (Vectorof stx)) (U Spec String)))
  (define (internal-context->string ic)
    (match ic
      [(? stx? ic)
       (stx->spec ic)]
      [(? vector? ic)
       (listof-zo->string stx->spec (vector->list ic))]
      [(? boolean? ic)
       (boolean->string ic)]))
  (list "mod"
        (lcons "name"             (name->spec               (mod-name z)))
        (lcons "srcname"          (symbol->string             (mod-srcname z)))
        (lcons "self-modidx"      (module-path-index->string  (mod-self-modidx z)))
        (lcons "prefix"           (prefix->spec             (mod-prefix z)))
        (lcons "provides"         (provides->spec       (mod-provides z)))
        (lcons "requires"         (requires->spec       (mod-requires z)))
        (lcons "body"             (listof-form-or-any->string (mod-body z)))
        (lcons "syntax-bodies"    (syntax-bodies->spec  (mod-syntax-bodies z)))
        (lcons "unexported"       (unexported->spec         (mod-unexported z)))
        (lcons "max-let-depth"    (number->string             (mod-max-let-depth z)))
        (lcons "dummy"            (toplevel->spec           (mod-dummy z)))
        (lcons "lang-info"        (lang-info->spec          (mod-lang-info z)))
        (lcons "internal-context" (internal-context->string (mod-internal-context z)))
        (lcons "flags"            (list->string   symbol->string (mod-flags z)))
        (lcons "pre-submodules"   (listof-zo->string mod->spec (mod-pre-submodules z)))
        (lcons "post-submodules"  (listof-zo->string mod->spec (mod-post-submodules z)))))

(: provided->spec (-> provided Spec))
(define
  (provided->spec z)
  (: mpi-or-f->string (-> (U Module-Path-Index #f) String))
  (define (mpi-or-f->string x)
    (if (eq? #f x)
        "#f"
        (module-path-index->string x)))
  (list "provided"
        (lcons "name"      (symbol->string (provided-name z)))
        (lcons "src"       (mpi-or-f->string (provided-src  z)))
        (lcons "src-name"  (symbol->string (provided-src-name z)))
        (lcons "nom-src"   (any->string (provided-nom-src z)))
        (lcons "src-phase" (number->string (provided-src-phase z)))
        (lcons "protected?" (boolean->string (provided-protected? z)))))

;; --- expr

;; Helper for `lam` and `case-lam`.
(: lam-name->spec (-> (U Symbol (Vectorof Any) (List )) String))
(define (lam-name->spec nm)
  (match nm
    [(? vector?)
     (any->string nm)]
    [(? empty?)
     "()"]
    [(? symbol?)
     (symbol->string nm)]))

(: lam->spec (-> lam Spec))
(define
  (lam->spec z)
  (: closure-map->spec (-> (Vectorof Exact-Nonnegative-Integer) String))
  (define (closure-map->spec cm)
    (list->string number->string (vector->list cm)))
  (: toplevel-map->spec (-> (U #f (Setof Exact-Nonnegative-Integer)) String))
  (define (toplevel-map->spec tm)
    (cond [(eq? #f tm) "#f"]
          [else (format-list
                 #:sep " "
                 (for/list : (Listof String) ([n : Exact-Nonnegative-Integer tm]) (number->string n)))]))
  (list "lam"
        (lcons "name"          (lam-name->spec                  (lam-name z)))
        (lcons "flags"         (list->string symbol->string (lam-flags z)))
        (lcons "num-params"    (number->string              (lam-num-params z)))
        (lcons "param-types"   (list->string symbol->string (lam-param-types z)))
        (lcons "rest?"         (boolean->string             (lam-rest? z)))
        (lcons "closure-map"   (closure-map->spec           (lam-closure-map z)))
        (lcons "closure-types" (list->string symbol->string (lam-closure-types z)))
        (lcons "toplevel-map"  (toplevel-map->spec          (lam-toplevel-map z)))
        (lcons "max-let-depth" (number->string              (lam-max-let-depth z)))
        (lcons "body"          (expr-seq-any->string        (lam-body z)))))

(: closure->spec (-> closure Spec))
(define
  (closure->spec z)
  (list "closure"
        (lcons "code"   (lam->spec    (closure-code z)))
        (lcons "gen-id" (symbol->string (closure-gen-id z)))))

(: case-lam->spec (-> case-lam Spec))
(define
  (case-lam->spec z)
  (list "case-lam"
        (lcons "name"    (lam-name->spec              (case-lam-name z)))
        (lcons "clauses" (list->string (lambda ([x : (U lam closure)]) (format-spec #f (expr->spec x))) (case-lam-clauses z)))))

(: let-one->spec (-> let-one Spec))
(define
  (let-one->spec z)
  (list "let-one"
        (lcons "rhs"    (expr-seq-any->string (let-one-rhs  z)))
        (lcons "body"   (expr-seq-any->string (let-one-body z)))
        (lcons "type"   (symbol-or-f->spec  (let-one-type z)))
        (lcons "unused?" (boolean->string      (let-one-unused? z)))))

(: let-void->spec (-> let-void Spec))
(define
  (let-void->spec z)
  (list "let-void"
        (lcons "count" (number->string       (let-void-count z)))
        (lcons "boxes" (boolean->string      (let-void-boxes? z)))
        (lcons "body"  (expr-seq-any->string (let-void-body z)))))

(: install-value->spec (-> install-value Spec))
(define
  (install-value->spec z)
  (list "install-value"
        (lcons "count"  (number->string       (install-value-count z)))
        (lcons "pos"    (number->string       (install-value-pos z)))
        (lcons "boxes?" (boolean->string      (install-value-boxes? z)))
        (lcons "rhs"    (expr-seq-any->string (install-value-rhs z)))
        (lcons "body"   (expr-seq-any->string (install-value-body z)))))

(: let-rec->spec (-> let-rec Spec))
(define
  (let-rec->spec z)
  (list "let-rec"
        (lcons "procs" (list->string (lambda ([lm : lam]) (format-spec #f (lam->spec lm))) (let-rec-procs z)))
        (lcons "body"  (expr-seq-any->string          (let-rec-body z)))))

(: boxenv->spec (-> boxenv Spec))
(define
  (boxenv->spec z)
  (list "boxenv"
        (lcons "pos"  (number->string       (boxenv-pos z)))
        (lcons "body" (expr-seq-any->string (boxenv-body z)))))

(: localref->spec (-> localref Spec))
(define
  (localref->spec z)
  (list "localref"
        (lcons "unbox?"        (boolean->string     (localref-unbox? z)))
        (lcons "pos"           (number->string      (localref-pos z)))
        (lcons "clear?"        (boolean->string     (localref-clear? z)))
        (lcons "other-clears?" (boolean->string     (localref-other-clears? z)))
        (lcons "type"          (symbol-or-f->spec (localref-type z)))))

(: toplevel->spec (-> toplevel Spec))
(define
  (toplevel->spec z)
  (list
        "toplevel"
        (lcons "depth"  (number->string  (toplevel-depth z)))
        (lcons "pos"    (number->string  (toplevel-pos z)))
        (lcons "const?" (boolean->string (toplevel-const? z)))
        (lcons "ready?" (boolean->string (toplevel-ready? z)))))

(: topsyntax->spec (-> topsyntax Spec))
(define
  (topsyntax->spec z)
  (list "topsyntax"
        (lcons "depth" (number->string (topsyntax-depth z)))
        (lcons "pos"   (number->string (topsyntax-pos z)))
        (lcons "midpt" (number->string (topsyntax-midpt z)))))

(: application->spec (-> application Spec))
(define
  (application->spec z)
  (list "application"
        (lcons "rator" (expr-seq-any->string              (application-rator z)))
        (lcons "rands" (list->string expr-seq-any->string (application-rands z)))))

(: branch->spec (-> branch Spec))
(define
  (branch->spec z)
  (list "branch"
        (lcons "test" (expr-seq-any->string (branch-test z)))
        (lcons "then" (expr-seq-any->string (branch-then z)))
        (lcons "else" (expr-seq-any->string (branch-else z)))))

(: with-cont-mark->spec (-> with-cont-mark Spec))
(define
  (with-cont-mark->spec z)
  (list "with-cont-mark"
        (lcons "key"  (expr-seq-any->string (with-cont-mark-key  z)))
        (lcons "val"  (expr-seq-any->string (with-cont-mark-val  z)))
        (lcons "body" (expr-seq-any->string (with-cont-mark-body z)))))

(: beg0->spec (-> beg0 Spec))
(define
  (beg0->spec z)
  (list "beg0"
        (lcons "seq" (list->string expr-seq-any->string (beg0-seq z)))))

(: varref->spec (-> varref Spec))
(define
  (varref->spec z)
  (list "varref"
        (lcons "toplevel" (match (varref-toplevel z)
                            [(? toplevel? tl) (toplevel->spec tl)]
                            [#t    "#t"]))
        (lcons "dummy"    (match (varref-dummy z)
                            [(? toplevel? dm) (toplevel->spec dm)]
                            [#f "#f"]))))

(: assign->spec (-> assign Spec))
(define
  (assign->spec z)
  (list "assign"
        (lcons "id"        (toplevel->spec     (assign-id z)))
        (lcons "rhs"       (expr-seq-any->string (assign-rhs z)))
        (lcons "undef-ok?" (boolean->string      (assign-undef-ok? z)))))

(: apply-values->spec (-> apply-values Spec))
(define
  (apply-values->spec z)
  (list "apply-values"
        (lcons "proc"      (expr-seq-any->string (apply-values-proc z)))
        (lcons "args-expr" (expr-seq-any->string (apply-values-args-expr z)))))

(: primval->spec (-> primval Spec))
(define
  (primval->spec z)
  (list "primval"
        (lcons "id" (number->string (primval-id z)))))

;; --- wrap

(: top-level-rename->spec (-> top-level-rename Spec))
(define
  (top-level-rename->spec z)
  (list "top-level-rename"
        (lcons "flag" (boolean->string (top-level-rename-flag z)))))

(: mark-barrier->spec (-> mark-barrier Spec))
(define
  (mark-barrier->spec z)
  (list "mark-barrier"
        (lcons "value" (symbol->string (mark-barrier-value z)))))

(: lexical-rename->spec (-> lexical-rename Spec))
(define
  (lexical-rename->spec z)
  (: lexical-rename-alist->string (-> (Listof (Pair Symbol (U Symbol (Pair Symbol (U (Pair Symbol (U Symbol #f)) free-id-info))))) String))
  (define
    (lexical-rename-alist->string alst)
    (list->string (lambda ([x : String]) x)
                  (for/list : (Listof String) ([a : (Pair Symbol (U Symbol (Pair Symbol (U (Pair Symbol (U Symbol #f)) free-id-info)))) alst])
                    (format "(~a . ~a)"
                            (car a)
                            (cond [(symbol? (cdr a)) (cdr a)]
                                  [else
                                   (: a* (Pair Symbol (U (Pair Symbol (U Symbol #f)) free-id-info)))
                                   (define a* (cdr a))
                                   (format "(~a . ~a)"
                                           (car a*)
                                           (cond [(free-id-info? (cdr a*)) (free-id-info->spec (cdr a*))]
                                                 [else                     (cdr a*)]))])))))
  (list "lexical-rename"
        (lcons "has-free-id-renames?" (boolean->string              (lexical-rename-has-free-id-renames? z)))
        (lcons "bool2"                (boolean->string              (lexical-rename-bool2 z)))
        (lcons "alist"                (lexical-rename-alist->string (lexical-rename-alist z)))))

(: phase-shift->spec (-> phase-shift Spec))
(define
  (phase-shift->spec z)
  (: mpi-or-f->string (-> (U Module-Path-Index #f) String))
  (define (mpi-or-f->string x)
    (if (module-path-index? x)
        (module-path-index->string x)
        "#f"))
  (list "phase-shift"
        (lcons "amt"       (number-or-f->string (phase-shift-amt z)))
        (lcons "src"       (mpi-or-f->string    (phase-shift-src z)))
        (lcons "dest"      (mpi-or-f->string    (phase-shift-dest z)))
        (lcons "cancel-id" (number-or-f->string (phase-shift-cancel-id z)))))

(: module-rename->spec (-> module-rename Spec))
(define
  (module-rename->spec z)
  (: rename->string (-> (Pair Symbol module-binding) String))
  (define (rename->string rm)
    (format "(~a ~a)"
            (symbol->string (car rm))
            (format-spec #f (module-binding->spec (cdr rm)))))
  (list "module-rename"
        (lcons "phase"        (number-or-f->string                     (module-rename-phase z)))
        (lcons "kind"         (symbol->string                          (module-rename-kind z)))
        (lcons "set-id"       (any->string                             (module-rename-set-id z)))
        (lcons "unmarshals"   (list->string (lambda ([afm : all-from-module]) (format-spec #f (all-from-module->spec afm))) (module-rename-unmarshals z)))
        (lcons "renames"      (list->string rename->string  (module-rename-renames z)))
        (lcons "mark-renames" (any->string                             (module-rename-mark-renames z)))
        (lcons "plus-kern?"   (boolean->string                         (module-rename-plus-kern? z)))))

(: wrap-mark->spec (-> wrap-mark Spec))
(define
  (wrap-mark->spec z)
  (list "wrap-mark"
        (lcons "val" (number->string (wrap-mark-val z)))))

(: prune->spec (-> prune Spec))
(define
  (prune->spec z)
  (list "prune"
        (lcons "sym" (any->string (prune-sym z)))))

;; --- module-binding

(: simple-module-binding->spec (-> simple-module-binding Spec))
(define
  (simple-module-binding->spec z)
  (list "simple-module-binding"
        (lcons "path" (module-path-index->string (simple-module-binding-path z)))))

(: phased-module-binding->spec (-> phased-module-binding Spec))
(define
  (phased-module-binding->spec z)
  (list "phased-module-binding"
        (lcons "path"                (module-path-index->string (phased-module-binding-path z)))
        (lcons "phase"               (number->string            (phased-module-binding-phase z)))
        (lcons "export-name"         (any->string               (phased-module-binding-export-name z)))
        (lcons "nominal-path"        (nominal-path->spec      (phased-module-binding-nominal-path z)))
        (lcons "nominal-export-name" (any->string               (phased-module-binding-nominal-export-name z)))))

(: exported-nominal-module-binding->spec (-> exported-nominal-module-binding Spec))
(define
  (exported-nominal-module-binding->spec z)
  (list "exported-nominal-module-binding"
        (lcons "path"                (module-path-index->string (exported-nominal-module-binding-path z)))
        (lcons "export-name"         (any->string               (exported-nominal-module-binding-export-name z)))
        (lcons "nominal-path"        (nominal-path->spec      (exported-nominal-module-binding-nominal-path z)))
        (lcons "nominal-export-name" (any->string               (exported-nominal-module-binding-nominal-export-name z)))))

(: nominal-module-binding->spec (-> nominal-module-binding Spec))
(define
  (nominal-module-binding->spec z)
  (list "nominal-module-binding"
        (lcons "path"         (module-path-index->string (nominal-module-binding-path z)))
        (lcons "nominal-path" (nominal-path->spec      (nominal-module-binding-nominal-path z)))))

(: exported-module-binding->spec (-> exported-module-binding Spec))
(define
  (exported-module-binding->spec z)
  (list "exported-module-binding"
        (lcons "path"        (module-path-index->string (exported-module-binding-path z)))
        (lcons "export-name" (any->string               (exported-module-binding-export-name z)))))

;; --- nominal-path

(: simple-nominal-path->spec (-> simple-nominal-path Spec))
(define
  (simple-nominal-path->spec z)
  (list "simple-nominal-path"
        (lcons "value" (module-path-index->string (simple-nominal-path-value z)))))

(: imported-nominal-path->spec (-> imported-nominal-path Spec))
(define
  (imported-nominal-path->spec z)
  (list "imported-nominal-path"
        (lcons "value"        (module-path-index->string (imported-nominal-path-value z)))
        (lcons "import-phase" (number->string            (imported-nominal-path-import-phase z)))))

(: phased-nominal-path->spec (-> phased-nominal-path Spec))
(define
  (phased-nominal-path->spec z)
  (list "phased-nominal-path"
        (lcons "value"        (module-path-index->string (phased-nominal-path-value z)))
        (lcons "import-phase" (number-or-f->string       (phased-nominal-path-import-phase z)))
        (lcons "phase"        (number->string            (phased-nominal-path-phase z)))))

;; --- Shapes

;; Shapes are not zo structs per se, but they are documented in the
;; decompile guide and do not seem to have a nice formatting method.

(: function-shape->spec (-> function-shape String))
(define
  (function-shape->spec fs)
  (format-list #:sep " "
               (list "function-shape"
                     (format "arity : ~a"            (function-shape-arity fs))
                     (format "preserves-marks? : ~a" (function-shape-preserves-marks? fs)))))

(: struct-shape->spec (-> struct-shape String))
(define
  (struct-shape->spec ss)
  (cond [(struct-type-shape?  ss) (struct-type-shape->spec  ss)]
        [(constructor-shape?  ss) (constructor-shape->spec  ss)]
        [(predicate-shape?    ss) (predicate-shape->spec    ss)]
        [(accessor-shape?     ss) (accessor-shape->spec     ss)]
        [(mutator-shape?      ss) (mutator-shape->spec      ss)]
        [(struct-other-shape? ss) (struct-other-shape->spec ss)]
        [else (error (format "unknown struct shape ~a" ss))]))

(: struct-type-shape->spec (-> struct-type-shape String))
(define
  (struct-type-shape->spec sts)
  (format-list #:sep " "
               (list "struct-type-shape"
                     (format "field-count : ~a" (struct-type-shape-field-count sts)))))

(: constructor-shape->spec (-> constructor-shape String))
(define
  (constructor-shape->spec cs)
  (format-list #:sep " "
               (list "constructor-shape"
                     (format "arity : ~a" (constructor-shape-arity cs)))))

(: predicate-shape->spec (-> predicate-shape String))
(define
  (predicate-shape->spec ps)
  (format-list (list "predicate-shape")))

(: accessor-shape->spec (-> accessor-shape String))
(define
  (accessor-shape->spec sts)
  (format-list #:sep " "
               (list "accessor-shape"
                     (format "field-count : ~a" (accessor-shape-field-count sts)))))

(: mutator-shape->spec (-> mutator-shape String))
(define
  (mutator-shape->spec sts)
  (format-list #:sep " "
               (list "mutator-shape"
                     (format "field-count : ~a" (mutator-shape-field-count sts)))))

(: struct-other-shape->spec (-> struct-other-shape String))
(define
  (struct-other-shape->spec ps)
  (format-list (list "struct-other-shape")))

;; --- helpers

;; Turn any value into a string.
(: any->string (-> Any String))
(define
  (any->string z)
  (format "~a" z))

;; Turn a boolean value into a string.
(: boolean->string (-> Boolean String))
(define
  (boolean->string b)
  (any->string b))

;; Turn an 'expr' struct or a 'seq' struct or any other value into a string.
(: expr-seq-any->string (-> (U expr seq Any) String))
(define
  (expr-seq-any->string z)
  (cond [(expr? z) (format-spec #f (expr->spec z))]
        [(seq?  z) (format-spec #f (seq->spec z))]
        [else      (any->string z)]))

;; Turn a 'form' struct or anything else into a string.
(: form-or-any->string (-> (U form Any) String))
(define
  (form-or-any->string fm)
  (cond [(form? fm) (format-spec #f (form->spec fm))]
        [else       (any->string   fm)]))

;; Alternate syntax for `string-join` -- the `sep` argument appears as a label
;; and defaults to a newline character.
(: format-list (->* ((Listof String)) (#:sep String) String))
(define
  (format-list xs #:sep [sep "\n"])
  (string-join xs sep))

;; Turn a spec into a string.
;; If `deep?` is false, only format the title (ignore the field names + thunks).
(: format-spec (-> Boolean Spec String))
(define
  (format-spec deep? struct-spec)
  (: fields (Listof (Pair String (-> (U Spec String)))))
  (define fields (cdr struct-spec))
  (: title String)
  (define title (format "<struct:~a>" (car struct-spec)))
  (: field-name-lengths (Listof Index))
  (define field-name-lengths
    (for/list ([fd fields]) (string-length (car fd))))
  (: w Nonnegative-Fixnum)
  (define w ;; width of longest struct field name
    (if (empty? fields) 0 (apply max field-name-lengths)))
  (if (not deep?)
      title
      (format-list
       (cons title
             (for/list : (Listof String) ([fd : (Pair String (-> (U Spec String))) fields])
               (: forced (U String Spec))
               (define forced ((cdr fd)))
               (: rest String)
               (define rest   (if (string? forced)
                                  forced
                                  (format-spec #f forced)))
               (format "  ~a : ~a" (pad (car fd) w) rest))))))

;; Turn a list into a string.
(: list->string (All (A) (-> (-> A String) (Listof A) String)))
(define
  (list->string f xs)
  (format "[~a]"
          (format-list #:sep " "
                       (for/list : (Listof String) ([x : A xs]) (f x)))))

;; Turn a list of things that might be 'form' structs into a list of strings.
(: listof-form-or-any->string (-> (Listof (U form Any)) String))
(define
  (listof-form-or-any->string xs)
  (list->string form-or-any->string xs))

;; Turn a list of zo structs into a list of strings using the helper function
;; `z->spec`.
;; TODO should not be polymorphic -- want to bind to subtypes
(: listof-zo->string (All (A) (-> (-> A Spec) (Listof A) String)))
(define
  (listof-zo->string z->spec zs)
  (cond [(empty? zs) "[]"]
        [else        (format "~a[~a]" (format-spec #f (z->spec (car zs))) (length zs))]))

;; Turn a module-path-index into a string
;; TODO I think we can do better than ~a
;; http://docs.racket-lang.org/reference/Module_Names_and_Loading.html
(: module-path-index->string (-> Module-Path-Index String))
(define
  (module-path-index->string mpi)
  (any->string mpi))

;; Turn a module path into a string
;; TODO can probably improve on ~a
(: module-path->spec (-> Module-Path String))
(define
  (module-path->spec mp)
  (any->string mp))

;; Turn a number or #f into a string.
(: number-or-f->string (-> (U Number #f) String))
(define
  (number-or-f->string nf)
  (if (eq? #f nf)
      "#f"
      (number->string nf)))

;; Turn a symbol or #f into a string.
(: symbol-or-f->spec (-> (U Symbol #f) String))
(define
  (symbol-or-f->spec sf)
  (if (eq? #f sf)
      "#f"
      (symbol->string sf)))

;; Turn something that might be a 'toplevel' struct into a string.
(: toplevel-or-any->string (-> (U toplevel Any) String))
(define
  (toplevel-or-any->string tl)
  (cond [(toplevel? tl) (format-spec #f (toplevel->spec tl))]
        [else           (any->string tl)]))

;; --- misc

;; If `str` has fewer than `w` characters,
;; append `(w - (len str))` characters to its right end.
(: pad (-> String Natural [#:char Char] String))
(define
  (pad str w #:char [c #\space])
  (: l Index)
  (define l (string-length str))
  (cond [(< l w) (format "~a~a" str (make-string (- w l) c))]
        [else    str]))

;; -----------------------------------------------------------------------------
;; --- testing

;; (module+ test
;;   (require rackunit
;;            compiler/zo-structs)

;;   ; Helper: force lazy tails so we can compare them.
;;   (: force-spec (-> Spec String))
;;   (define (force-spec sp)
;;     (cons (car sp) (for/list : (Listof (Pair String String)) ([xy : (Pair String (-> (U Spec String))) (cdr sp)])
;;                      (cons (car xy)
;;                            (let: ([tl : (U Spec String) ((cdr xy))])
;;                              (if (string? tl)
;;                                  tl
;;                                  (format-spec #f tl)))))))

;;   ;; --- API functions
;;   ;; zo->spec
;;   (check-exn exn:fail? (lambda () (zo->spec (zo))))
;;   (check-equal? (force-spec (zo->spec (branch #t #f #t)))
;;                 (list "branch"
;;                       (cons "test" "#t")
;;                       (cons "then" "#f")
;;                       (cons "else" "#t")))

;;   ;; zo->string
;;   (check-exn exn:fail? (lambda () (zo->string (zo))))
;;   (check-equal? (zo->string (toplevel 1 1 #t #t)) "<struct:toplevel>\n  depth  : 1\n  pos    : 1\n  const? : #t\n  ready? : #t")
;;   (check-equal? (zo->string #:deep? #t (toplevel 1 1 #t #t)) "<struct:toplevel>\n  depth  : 1\n  pos    : 1\n  const? : #t\n  ready? : #t")
;;   (check-equal? (zo->string #:deep? #f (toplevel 1 1 #t #t)) "<struct:toplevel>")

;;   ;; --- private

;;   ;; compilation-top->spec
;;   (let* ([px (prefix 0 '() '())]
;;          [cd (seq '())]
;;          [z  (compilation-top 0 px cd)])
;;     (check-equal? (force-spec (compilation-top->spec z))
;;                   (cons "compilation-top"
;;                         (list (cons "max-let-depth" "0")
;;                               (cons "prefix" "<struct:prefix>")
;;                               (cons "code" "<struct:seq>")))))

;;   ;; prefix->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [gb (global-bucket 'NAME)]
;;          [mv (module-variable mpi 'SYM 0 0 #f)]
;;          [sx (stx (wrapped (void) '() 'tainted))]
;;          [z  (prefix 0
;;                     (list gb mv)
;;                     (list sx))])
;;     (check-equal? (force-spec (prefix->spec z))
;;                   (cons "prefix"
;;                         (list (cons "num-lifts" "0")
;;                               (cons "toplevels" "[<struct:global-bucket> <struct:module-variable>]")
;;                               (cons "stxs" "<struct:stx>[1]")))))

;;   ;; global-bucket->spec
;;   (let* ([z (global-bucket 'arbitrary-symbol)])
;;     (check-equal? (force-spec (global-bucket->spec z))
;;                   (cons "global-bucket"
;;                         (list (cons "name" "arbitrary-symbol")))))

;;   ;; module-variable->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [fs  (function-shape 1 #f)]
;;          [ss  (struct-shape)] 
;;          [z   (module-variable mpi 'arbitrary 999 9001 fs)])
;;     (check-equal? (force-spec (module-variable->spec z))
;;                   (cons "module-variable"
;;                         (list (cons "modidx" "#<module-path-index>")
;;                               (cons "sym" "arbitrary")
;;                               (cons "pos" "999")
;;                               (cons "phase" "9001")
;;                               (cons "constantness" "function-shape arity : 1 preserves-marks? : #f")))))

;;   ;; stx->spec
;;   (let* ([wp (wrapped (void) '() 'tainted)]
;;          [z (stx wp)])
;;     (check-equal? (force-spec (stx->spec z))
;;                   (cons "stx"
;;                         (list (cons "encoded" "<struct:wrapped>")))))

;;   ;; form->spec (see below)
;;   (let* ([z (form)])
;;     (check-equal? (form->spec z) #f))

;;   ;; expr->spec (see below)
;;   (let* ([z (expr)])
;;     (check-equal? (expr->spec z) #f))

;;   ;; wrapped->spec
;;   (let* ([wps (list (prune 'A) (prune 'B) (prune 'C))]
;;          [z   (wrapped 'yolo wps 'tainted)])
;;     (check-equal? (force-spec (wrapped->spec z))
;;                   (cons "wrapped"
;;                         (list (cons "datum" "yolo")
;;                               (cons "wraps" "<struct:prune>[3]")
;;                               (cons "tamper-status" "tainted")))))

;;   ;; wrap->spec (see below)
;;   (let* ([z (wrap)])
;;     (check-equal? (wrap->spec z) #f))
  
;;   ;; free-id-info->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z   (free-id-info mpi 'A mpi 'B #f 101 #f #f)])
;;     (check-equal? (force-spec (free-id-info->spec z))
;;                   (cons "free-id-info"
;;                         (list (cons "path0" "#<module-path-index>")
;;                               (cons "symbol0" "A")
;;                               (cons "path1" "#<module-path-index>")
;;                               (cons "symbol1" "B")
;;                               (cons "phase0" "#f")
;;                               (cons "phase1" "101")
;;                               (cons "phase2" "#f")
;;                               (cons "use-current-inspector?" "#f")))))

;;   ;; all-from-module->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z (all-from-module mpi #f #f '() #f '())])
;;     (check-equal? (force-spec (all-from-module->spec z))
;;                   (cons "all-from-module"
;;                         (list (cons "path" "#<module-path-index>")
;;                               (cons "phase" "#f")
;;                               (cons "src-phase" "#f")
;;                               (cons "exceptions" "[]")
;;                               (cons "prefix" "#f")
;;                               (cons "context" "[]")))))

;;   ;; module-binding->spec (see below)
;;   (let* ([z (module-binding)])
;;     (check-equal? (module-binding->spec z) #f))
  
;;   ;; nominal-path->spec (see below)
;;   (let* ([z (nominal-path)])
;;     (check-equal? (nominal-path->spec z) #f))

;;   ;; def-values->spec
;;   (let* ([ids (list (toplevel 1 2 #t #f))]
;;          [rhs (beg0 '())]
;;          [z (def-values ids rhs)])
;;     (check-equal? (force-spec (def-values->spec z))
;;                   (cons "def-values"
;;                         (list (cons "ids" "<struct:toplevel>[1]")
;;                               (cons "rhs" "<struct:beg0>")))))

;;   ;; def-syntaxes->
;;   (let* ([ids (list (toplevel 1 2 #t #f))]
;;          [rhs (beg0 '())]
;;          [px  (prefix 0 '() '())]
;;          [dm  (toplevel 1 1 #t #t)]
;;          [z   (def-syntaxes ids rhs px 42 dm)])
;;     (check-equal? (force-spec (def-syntaxes->spec z))
;;                   (cons "def-syntaxes"
;;                         (list (cons "ids" "[<struct:toplevel>]")
;;                               (cons "rhs" "<struct:beg0>")
;;                               (cons "prefix" "<struct:prefix>")
;;                               (cons "max-let-depth" "42")
;;                               (cons  "dummy" "<struct:toplevel>")))))

;;   ;; seq-for-syntax->spec
;;   (let* ([fms (list (seq '()))]
;;          [px  (prefix 0 '() '())]
;;          [dm  (toplevel 9 9 #t #t)]
;;          [z   (seq-for-syntax fms px 8 dm)])
;;     (check-equal? (force-spec (seq-for-syntax->spec z))
;;                   (cons "seq-for-syntax"
;;                         (list (cons "forms" "[<struct:seq>]")
;;                               (cons "prefix" "<struct:prefix>")
;;                               (cons "max-let-depth" "8")
;;                               (cons "dummy" "<struct:toplevel>")))))

;;   ;; req->spec
;;   (let* ([sx (stx (wrapped 'XXX '() 'clean))]
;;          [dm (toplevel 1 1 #t #t)]
;;          [z  (req sx dm)])
;;     (check-equal? (force-spec (req->spec z))
;;                   (cons "req"
;;                         (list (cons "reqs" "<struct:stx>")
;;                               (cons "dummy" "<struct:toplevel>")))))

;;   ;; seq->spec
;;   (let* ([fms (list (seq '()) (seq '()) (seq '()))]
;;          [z   (seq fms)])
;;     (check-equal? (force-spec (seq->spec z))
;;                   (cons "seq"
;;                         (list (cons "forms" "[<struct:seq> <struct:seq> <struct:seq>]")))))

  
;;   ;; splice->
;;   (let* ([fms (list (seq '()) (seq '()))]
;;          [z   (splice fms)])
;;     (check-equal? (force-spec (splice->spec z))
;;                   (cons "splice"
;;                         (list (cons "forms" "[<struct:seq> <struct:seq>]")))))
    
;;   ;; inline-variant->spec
;;   (let* ([dr (beg0 '())]
;;          [il (beg0 '())]
;;          [z  (inline-variant dr il)])
;;     (check-equal? (force-spec (inline-variant->spec z))
;;                   (cons "inline-variant"
;;                         (list (cons "direct" "<struct:beg0>")
;;                               (cons "inline" "<struct:beg0>")))))

;;   ;; mod->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [px  (prefix 0 '() '())]
;;          [pd1 (provided 'p1 #f 'B 'C 13 #t)]
;;          [pd2 (provided 'p2 #f 'asdf 'C 6 #f)]
;;          [pd3 (provided 'p3 #f 'B 'lol 832 #t)]
;;          [pd4 (provided 'p4 #f 'R 'xx 1 #t)]
;;          [pvs (list (list #f (list pd1 pd2) (list pd3))
;;                     (list #f (list pd4) '()))]
;;          [bd  (list (seq '()) 'any)]
;;          [ds  (def-syntaxes '() (beg0 '()) (prefix 0 '() '()) 1 #f)]
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
;;     (check-equal? (force-spec (mod->spec z))
;;                   (cons "mod"
;;                         (list (cons "name" "name")
;;                               (cons "srcname" "srcname")
;;                               (cons "self-modidx" "#<module-path-index>")
;;                               (cons "prefix" "<struct:prefix>")
;;                               (cons "provides" "[#f <struct:provided>[2] <struct:provided>[1] #f <struct:provided>[1] []]")
;;                               (cons "requires" "[]")
;;                               (cons "body" "[<struct:seq> any]")
;;                               (cons "syntax-bodies" "[7 [<struct:def-syntaxes>] 8 [<struct:seq-for-syntax>]]")
;;                               (cons "unexported" "[]")
;;                               (cons "max-let-depth" "0")
;;                               (cons "dummy" "<struct:toplevel>")
;;                               (cons "lang-info" "#f")
;;                               (cons "internal-context" "<struct:stx>")
;;                               (cons "flags" "[]")
;;                               (cons "pre-submodules" "<struct:mod>[2]")
;;                               (cons "post-submodules" "<struct:mod>[1]")))))

;;   ;; provided->spec
;;   (let* ([z (provided 'name #f 'srcname 'nomnom 12 #t)])
;;     (check-equal? (force-spec (provided->spec z))
;;                   (cons "provided"
;;                         (list (cons "name" "name")
;;                               (cons "src" "#f")
;;                               (cons "src-name" "srcname")
;;                               (cons "nom-src" "nomnom")
;;                               (cons "src-phase" "12")
;;                               (cons "protected?" "#t")))))

;;   ;; lam->spec
;;   (let* ([bd (beg0 '())]
;;          [z  (lam 'name '() 3 '() #f '#() '() #f 1 bd)])
;;     (check-equal? (force-spec (lam->spec z))
;;                   (cons "lam"
;;                         (list (cons "name" "name")
;;                               (cons "flags" "[]")
;;                               (cons "num-params" "3")
;;                               (cons "param-types" "[]")
;;                               (cons "rest?" "#f")
;;                               (cons "closure-map" "[]")
;;                               (cons "closure-types" "[]")
;;                               (cons "toplevel-map" "#f")
;;                               (cons "max-let-depth" "1")
;;                               (cons "body" "<struct:beg0>")))))

;;   ;; closure->spec
;;   (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
;;          [z  (closure lm 'genid)])
;;     (check-equal? (force-spec (closure->spec z))
;;                   (cons "closure"
;;                         (list (cons "code" "<struct:lam>")
;;                               (cons "gen-id" "genid")))))

;;   ;; case-lam->spec
;;   (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
;;          [cl (closure lm 'id)]
;;          [cls (list lm cl lm)]
;;          [z   (case-lam 'name cls)])
;;     (check-equal? (force-spec (case-lam->spec z))
;;                   (cons "case-lam"
;;                         (list (cons "name" "name")
;;                               (cons "clauses" "[<struct:lam> <struct:closure> <struct:lam>]")))))

;;   ;; let-one->spec
;;   (let* ([rhs (beg0 '())]
;;          [bdy (beg0 '())]
;;          [z   (let-one rhs bdy #f #f)])
;;     (check-equal? (force-spec (let-one->spec z))
;;                   (cons "let-one"
;;                         (list (cons "rhs" "<struct:beg0>")
;;                               (cons "body" "<struct:beg0>")
;;                               (cons "type" "#f")
;;                               (cons "unused?" "#f")))))

;;   ;; let-void->spec
;;   (let* ([bdy (beg0 '())]
;;          [z   (let-void 1 #f bdy)])
;;     (check-equal? (force-spec (let-void->spec z))
;;                   (cons "let-void"
;;                         (list (cons "count" "1")
;;                               (cons "boxes" "#f")
;;                               (cons "body" "<struct:beg0>")))))

;;   ;; install-value->spec
;;   (let* ([rhs (branch #t #t #t)]
;;          [bdy (beg0 '())]
;;          [z   (install-value 2 3 #f rhs bdy)])
;;     (check-equal? (force-spec (install-value->spec z))
;;                   (cons "install-value"
;;                         (list (cons "count" "2")
;;                               (cons "pos" "3")
;;                               (cons "boxes?" "#f")
;;                               (cons "rhs" "<struct:branch>")
;;                               (cons "body" "<struct:beg0>")))))

;;   ;; let-rec->spec
;;   (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
;;          [pcs (list lm lm)]
;;          [bdy (beg0 '())]
;;          [z   (let-rec pcs bdy)])
;;     (check-equal? (force-spec (let-rec->spec z))
;;                   (cons "let-rec"
;;                         (list (cons "procs" "<struct:lam>[2]")
;;                               (cons "body" "<struct:beg0>")))))

;;   ;; boxenv->spec
;;   (let* ([bdy (beg0 '())]
;;          [z   (boxenv 2 bdy)])
;;     (check-equal? (force-spec (boxenv->spec z))
;;                   (cons "boxenv"
;;                         (list (cons "pos" "2")
;;                               (cons "body" "<struct:beg0>")))))

;;   ;; localref->spec
;;   (let ([z (localref #t 1 #t #t #f)])
;;     (check-equal? (force-spec (localref->spec z))
;;                   (cons "localref"
;;                         (list (cons "unbox?" "#t")
;;                               (cons "pos" "1")
;;                               (cons "clear?" "#t")
;;                               (cons "other-clears?" "#t")
;;                               (cons "type" "#f")))))

;;   ;; toplevel->spec
;;   (let ([z (toplevel 1 2 #f #f)])
;;     (check-equal? (force-spec (toplevel->spec z))
;;                   (cons "toplevel"
;;                         (list (cons "depth" "1")
;;                               (cons "pos" "2")
;;                               (cons "const?" "#f")
;;                               (cons "ready?" "#f")))))

;;   ;; topsyntax->spec
;;   (let ([z (topsyntax 1 2 3)])
;;     (check-equal? (force-spec (topsyntax->spec z))
;;                   (cons "topsyntax"
;;                         (list (cons "depth" "1")
;;                               (cons "pos" "2")
;;                               (cons "midpt" "3")))))

;;   ;; application->spec
;;   (let* ([e (beg0 '())]
;;          [s (seq '())]
;;          [z (application s (list e s s '() 'any 54 e))])
;;     (check-equal? (force-spec (application->spec z))
;;                   (cons "application"
;;                         (list (cons "rator" "<struct:seq>")
;;                               (cons "rands" "[<struct:beg0> <struct:seq> <struct:seq> () any 54 <struct:beg0>]")))))

;;   ;; branch->spec
;;   (let* ([z (branch #t (beg0 '()) #f)])
;;     (check-equal? (force-spec (branch->spec z))
;;                   (cons "branch"
;;                         (list (cons "test" "#t")
;;                               (cons "then" "<struct:beg0>")
;;                               (cons "else" "#f")))))

;;   ;; with-cont-mark->spec
;;   (let ([z (with-cont-mark (beg0 '()) (branch #t #t #t) (topsyntax 1 1 1))])
;;     (check-equal? (force-spec (with-cont-mark->spec z))
;;                   (cons "with-cont-mark"
;;                         (list (cons "key" "<struct:beg0>")
;;                               (cons "val" "<struct:branch>")
;;                               (cons "body" "<struct:topsyntax>")))))

;;   ;; beg0->spec
;;   (let ([z (beg0 (list (beg0 '()) 'asdf (beg0 (list (expr)))))])
;;     (check-equal? (force-spec (beg0->spec z))
;;                   (cons "beg0"
;;                         (list (cons "seq" "[<struct:beg0> asdf <struct:beg0>]")))))

;;   ;; varref->spec
;;   (let* ([tl (toplevel 1 1 #f #f)]
;;          [z  (varref tl #f)])
;;     (check-equal? (force-spec (varref->spec z))
;;                   (cons "varref"
;;                         (list (cons "toplevel" "<struct:toplevel>")
;;                               (cons "dummy" "#f")))))

;;   ;; assign->spec
;;   (let* ([id  (toplevel 1 1 #f #f)]
;;          [rhs (beg0 '())]
;;          [z   (assign id rhs #t)])
;;     (check-equal? (force-spec (assign->spec z))
;;                   (cons "assign"
;;                         (list (cons "id" "<struct:toplevel>")
;;                               (cons "rhs" "<struct:beg0>")
;;                               (cons "undef-ok?" "#t")))))

;;   ;; apply-values->spec
;;   (let ([z (apply-values (beg0 '()) (topsyntax 1 2 8))])
;;     (check-equal? (force-spec (apply-values->spec z))
;;                   (cons "apply-values"
;;                         (list (cons "proc" "<struct:beg0>")
;;                               (cons "args-expr" "<struct:topsyntax>")))))

;;   ;; primval->spec
;;   (let ([z (primval 420)])
;;     (check-equal? (force-spec (primval->spec z))
;;                   (cons "primval"
;;                         (list (cons "id" "420")))))

;;   ;; top-level-rename->spec
;;   (let* ([z (top-level-rename #t)])
;;     (check-equal? (force-spec (top-level-rename->spec z))
;;                               (cons "top-level-rename"
;;                                     (list (cons "flag" "#t")))))

;;   ;; mark-barrier->spec
;;   (let* ([z (mark-barrier 'val)])
;;     (check-equal? (force-spec (mark-barrier->spec z))
;;                   (cons "mark-barrier"
;;                         (list (cons "value" "val")))))

;;   ;; lexical-rename->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [fii (free-id-info mpi 'A mpi 'B #f 101 #f #f)]
;;          [alist (list (cons 'A 'B)
;;                       (cons 'C (cons 'D fii))
;;                       (cons 'F (cons 'G (cons 'H 'I))))]
;;          [z (lexical-rename #f #f alist)]
;;          [res (force-spec (lexical-rename->spec z))])
;;     (check-equal? (car res) "lexical-rename")
;;     (check-equal? (cadr res) (cons "has-free-id-renames?" "#f"))
;;     (check-equal? (caddr res) (cons "bool2" "#f"))
;;     (check-equal? (car (cadddr res)) "alist"))

;;   ;; phase-shift->spec
;;   (let ([z (phase-shift #f #f #f #f)])
;;     (check-equal? (force-spec (phase-shift->spec z))
;;                   (cons "phase-shift"
;;                         (list (cons "amt" "#f")
;;                               (cons "src" "#f")
;;                               (cons "dest" "#f")
;;                               (cons "cancel-id" "#f")))))

;;   ;; module-rename->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [ums (list (all-from-module mpi #f #f '() #f '()))]
;;          [bds (list (cons 'A (simple-module-binding mpi)))]
;;          [z (module-rename #f 'marked 'setid ums bds 'any #f)])
;;     (check-equal? (force-spec (module-rename->spec z))
;;                   (cons "module-rename"
;;                         (list (cons "phase" "#f")
;;                               (cons "kind" "marked")
;;                               (cons "set-id" "setid")
;;                               (cons "unmarshals" "<struct:all-from-module>[1]")
;;                               (cons "renames" "[(A <struct:simple-module-binding>)]")
;;                               (cons "mark-renames" "any")
;;                               (cons "plus-kern?" "#f")))))

;;   ;; wrap-mark->spec
;;   (let ([z (wrap-mark 121)])
;;     (check-equal? (force-spec (wrap-mark->spec z))
;;                   (cons "wrap-mark"
;;                         (list (cons "val" "121")))))

;;   ;; prune->spec
;;   (let ([z (prune 'anything-at-all)])
;;     (check-equal? (force-spec (prune->spec z))
;;                   (cons "prune"
;;                         (list (cons "sym" "anything-at-all")))))

;;   ;; simple-module-binding->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z (simple-module-binding mpi)])
;;     (check-equal? (force-spec (simple-module-binding->spec z))
;;                   (cons "simple-module-binding"
;;                         (list (cons "path" "#<module-path-index>")))))

;;   ;; phased-module-binding->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [np (simple-nominal-path mpi)]
;;          [z (phased-module-binding mpi 1 'any np 'any2)])
;;     (check-equal? (force-spec (phased-module-binding->spec z))
;;                   (cons "phased-module-binding"
;;                         (list (cons "path" "#<module-path-index>")
;;                               (cons "phase" "1")
;;                               (cons "export-name" "any")
;;                               (cons "nominal-path" "<struct:simple-nominal-path>")
;;                               (cons "nominal-export-name" "any2")))))

;;   ;; exported-nominal-module-binding->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [np (simple-nominal-path mpi)]
;;          [z (exported-nominal-module-binding mpi 'any np 'any)])
;;     (check-equal? (force-spec (exported-nominal-module-binding->spec z))
;;                   (cons "exported-nominal-module-binding"
;;                         (list (cons "path" "#<module-path-index>")
;;                               (cons "export-name" "any")
;;                               (cons "nominal-path" "<struct:simple-nominal-path>")
;;                               (cons "nominal-export-name" "any")))))

;;   ;; nominal-module-binding->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [np (simple-nominal-path mpi)]
;;          [z (nominal-module-binding mpi np)])
;;     (check-equal? (force-spec (nominal-module-binding->spec z))
;;                   (cons "nominal-module-binding"
;;                         (list (cons "path" "#<module-path-index>")
;;                               (cons "nominal-path" "<struct:simple-nominal-path>")))))

;;   ;; exported-module-binding->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z (exported-module-binding mpi 'any)])
;;     (check-equal? (force-spec (exported-module-binding->spec z))
;;                   (cons "exported-module-binding"
;;                         (list (cons "path" "#<module-path-index>")
;;                               (cons "export-name" "any")))))

;;   ;; simple-nominal-path->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z (simple-nominal-path mpi)])
;;     (check-equal? (force-spec (simple-nominal-path->spec z))
;;                   (cons "simple-nominal-path"
;;                         (list (cons "value" "#<module-path-index>")))))

;;   ;; imported-nominal-path->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z (imported-nominal-path mpi 12423)])
;;     (check-equal? (force-spec (imported-nominal-path->spec z))
;;                   (cons "imported-nominal-path"
;;                         (list (cons "value" "#<module-path-index>")
;;                               (cons "import-phase" "12423")))))

;;   ;; phased-nominal-path->spec
;;   (let* ([mpi (module-path-index-join #f #f)]
;;          [z (phased-nominal-path mpi #f 8)])
;;     (check-equal? (force-spec (phased-nominal-path->spec z))
;;                   (cons "phased-nominal-path"
;;                         (list (cons "value" "#<module-path-index>")
;;                               (cons "import-phase" "#f")
;;                               (cons "phase" "8")))))

;;   ;; --- helpers
;;   ;; any->string
;;   (check-equal? (any->string 'any) "any")
;;   (check-equal? (any->string "any") "any")
;;   (check-equal? (any->string #t) "#t")
;;   (check-equal? (any->string (vector 1 2 3)) "#(1 2 3)")
;;   (check-equal? (any->string (nominal-path)) "#s((nominal-path zo 0))")

;;   ;; boolean->string
;;   (check-equal? (boolean->string #t) "#t")
;;   (check-equal? (boolean->string #f) "#f")

;;   ;; expr-seq-any->string
;;   (check-equal? (expr-seq-any->string (beg0 '())) "<struct:beg0>")
;;   (check-equal? (expr-seq-any->string (branch #t (expr) (expr))) "<struct:branch>")
;;   (check-equal? (expr-seq-any->string (seq '(blah))) "<struct:seq>")
;;   (check-equal? (expr-seq-any->string 420) "420")
;;   (check-equal? (expr-seq-any->string +) "#<procedure:+>")

;;   ;; form-or-any->string
;;   (check-equal? (form-or-any->string (def-values '() (expr))) "<struct:def-values>")
;;   (check-equal? (form-or-any->string (lam 'name '() 3 '() #f '#() '() #f 1 (expr))) "<struct:lam>")
;;   (check-equal? (form-or-any->string (zo)) "#s(zo)")
;;   (check-equal? (form-or-any->string "()") "()")
;;   (check-equal? (form-or-any->string #\H) "H")

;;   ;; format-list
;;   ; (this is just string-join)
;;   (check-equal? (format-list '()) "")
;;   (check-equal? (format-list (list "a" "bear" "man")) "a\nbear\nman")
;;   (check-equal? (format-list #:sep "---" (list "racket" "eering")) "racket---eering")

;;   ;; format-spec
;;   ; No fields
;;   (check-equal? (format-spec #f (cons "hello" '())) "<struct:hello>")
;;   (check-equal? (format-spec #t (cons "hello" '())) "<struct:hello>")
;;   ; String fields
;;   (check-equal? (format-spec #f (cons "str" (list (cons "hello" (lambda () "there"))))) "<struct:str>")
;;   (check-equal? (format-spec #t (cons "str" (list (cons "hello" (lambda () "there"))))) "<struct:str>\n  hello : there")
;;   ; Nested struct fields
;;   (check-equal? (format-spec #f (cons "pika" (list (cons "f1" (lambda () "val1"))
;;                                                    (cons "f2" (lambda () (list "nested" (cons "n1" (lambda () "wepa")))))))) "<struct:pika>")
;;   (check-equal? (format-spec #t (cons "pika" (list (cons "f1" (lambda () "val1"))
;;                                                    (cons "f2" (lambda () (list "nested" (cons "n1" (lambda () "wepa")))))))) "<struct:pika>\n  f1 : val1\n  f2 : <struct:nested>")
;;   ; Padding
;;   (check-equal? (format-spec #t (cons "pika" (list (cons "long-name" (lambda () "v1"))
;;                                                    (cons "name" (lambda () "v2"))))) "<struct:pika>\n  long-name : v1\n  name      : v2")

;;   ;; list->string
;;   (check-equal? (list->string (lambda (x) "blah") '()) "[]")
;;   (check-equal? (list->string number->string (list 1 2 3 4)) "[1 2 3 4]")
;;   (check-equal? (list->string (lambda (x) (format-spec #f (expr->spec x))) (list (branch #t #t #t))) "[<struct:branch>]")

;;   ;; listof-form-or-any->string
;;   (check-equal? (listof-form-or-any->string (list (seq '()) 'cat 53)) "[<struct:seq> cat 53]")

;;   ;; listof-zo->string
;;   (check-equal? (listof-zo->string toplevel->spec (list (toplevel 1 1 #f #f))) "<struct:toplevel>[1]")

;;   ;; module-path-index->string
;;   (check-equal? (module-path-index->string (module-path-index-join #f #f)) "#<module-path-index>")

;;   ;; module-path->spec
;;   (check-equal? (module-path->spec 'lalala) "lalala")

;;   ;; number-or-f->string
;;   (check-equal? (number-or-f->string #f) "#f")
;;   (check-equal? (number-or-f->string 0) "0")
;;   (check-equal? (number-or-f->string -1) "-1")
;;   (check-equal? (number-or-f->string 98) "98")

;;   ;; symbol-or-f->spec
;;   (check-equal? (symbol-or-f->spec #f) "#f")
;;   (check-equal? (symbol-or-f->spec '#f) "#f")
;;   (check-equal? (symbol-or-f->spec 'foobar) "foobar")
;;   (check-equal? (symbol-or-f->spec 'wunderbar) "wunderbar")

;;   ;; toplevel-or-any->string
;;   (check-equal? (toplevel-or-any->string (toplevel 19 462 #t #t)) "<struct:toplevel>")
;;   (check-equal? (toplevel-or-any->string (toplevel 0 0 #f #f)) "<struct:toplevel>")
;;   ; Only toplevel zo structs get pretty-printed
;;   (check-equal? (toplevel-or-any->string (branch #t #t (beg0 '()))) "#s((branch expr 0 form 0 zo 0) #t #t #s((beg0 expr 0 form 0 zo 0) ()))")
;;   (check-equal? (toplevel-or-any->string "help") "help")

;;   ;; starts-with
;;   ; passes
;;   (check-true (starts-with "asdf" ""))
;;   (check-true (starts-with "asdf" "a"))
;;   (check-true (starts-with "asdf" "as"))
;;   (check-true (starts-with "asdf" "asd"))
;;   (check-true (starts-with "asdf" "asdf"))
;;   ; fails
;;   (check-false (starts-with "asdf" "s"))
;;   (check-false (starts-with "asdf" "asdfg"))
;;   (check-false (starts-with "asdf" "ass"))

;;   ;; pad
;;   (check-equal? (pad "str" 3) "str")
;;   (check-equal? (pad "str" 4) "str ")
;;   (check-equal? (pad "str" 5 #:char #\X) "strXX")
;; )
