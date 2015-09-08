#lang typed/racket


;; A Spec is the name of a zo struct and a list of pairs representing its fields:
;; - The car of each field is the name of that field
;; - The cdr of each field is a thunk for building a representation of the field's value.
;;   If the value is a zo-struct, the thunk should build a Spec
;;   Otherwise, the thunk should build a string
(define-type Spec
  (Rec Spec
   (Pair String (Listof (Pair String (-> (U Spec String)))))))
(provide Spec)

(require/typed/provide compiler/zo-structs
               [#:struct zo ()]
               [#:struct (compilation-top zo) (
                 [max-let-depth : Exact-Nonnegative-Integer]
                 [prefix : prefix]
                 [code : (U form Any)])]
               [#:struct (prefix zo) (
                 [num-lifts : Exact-Nonnegative-Integer] 
                 [toplevels : (Listof (U #f Symbol global-bucket module-variable))]
                 [stxs : (Listof stx)])]
               [#:struct (global-bucket zo) (
                 [name : Symbol])]
               [#:struct (module-variable zo) (
                 [modidx : Module-Path-Index]
                 [sym : Symbol]
                 [pos : Integer]
                 [phase : Exact-Nonnegative-Integer]
                 [constantness : (U #f 'constant 'fixed
                                    function-shape struct-shape)])]
               [#:struct function-shape (
                 [arity : (U Natural arity-at-least (Listof (U Natural arity-at-least)))]
                 [preserves-marks? : Boolean])] ;;bg; got type from (:print-type procedure-arity)
               [#:struct struct-shape ()]
               [#:struct (struct-type-shape struct-shape) (
                 [field-count : Exact-Nonnegative-Integer])]
               [#:struct (constructor-shape struct-shape) (
                 [arity : Exact-Nonnegative-Integer])]
               [#:struct (predicate-shape struct-shape) ()]
               [#:struct (accessor-shape struct-shape) (
                 [field-count : Exact-Nonnegative-Integer])]
               [#:struct (mutator-shape struct-shape) (
                 [field-count : Exact-Nonnegative-Integer])]
               [#:struct (struct-other-shape struct-shape) ()]
               [#:struct (stx zo) ([encoded : wrapped])]
               [#:struct (form zo) ()]
               [#:struct (expr form) ()]
               [#:struct (wrapped zo) (
                 [datum : Any]
                 [wraps : (Listof wrap)]
                 [tamper-status : (U 'clean 'armed 'tainted)])]
               [#:struct (wrap zo) ()]
               [#:struct (free-id-info zo) (
                 [path0 : Module-Path-Index]
                 [symbol0 : Symbol]
                 [path1 : Module-Path-Index]
                 [symbol1 : Symbol]
                 [phase0 : (U Integer #f)]
                 [phase1 : (U Integer #f)]
                 [phase2 : (U Integer #f)]
                 [use-current-inspector? : Boolean])]
               [#:struct (all-from-module zo) (
                 [path : Module-Path-Index]
                 [phase : (U Integer #f)]
                 [src-phase : (U Integer #f)]
                 [exceptions : (Listof Symbol)]
                 [prefix : (U Symbol #f)]
                 [context : (U (Listof Integer)
                               (Vector (Listof Integer) Any))])]
               [#:struct (module-binding zo) ()]
               [#:struct (nominal-path zo) ()]
               [#:struct (provided zo) (
                 [name : Symbol]
                 [src : (U Module-Path-Index #f)]
                 [src-name : Symbol]
                 [nom-src : Any] ; mflatt?: should be (or/c module-path-index? #f)
                 [src-phase : Exact-Nonnegative-Integer]
                 [protected? : Boolean])]
               [#:struct (def-values form) (
                 [ids : (Listof (U toplevel Symbol))]
                 [rhs : (U expr seq inline-variant Any)])]
               [#:struct (def-syntaxes form) (
                 [ids : (Listof (U toplevel Symbol))]
                 [rhs : (U expr seq Any)]
                 [prefix : prefix]
                 [max-let-depth : Exact-Nonnegative-Integer]
                 [dummy : (U toplevel #f)])]
               [#:struct (seq-for-syntax form) (
                 [forms : (Listof (U form Any))] ; `begin-for-syntax'
                 [prefix : prefix]
                 [max-let-depth : Exact-Nonnegative-Integer]
                 [dummy : (U toplevel #f)])]
               [#:struct (req form) (
                 [reqs : stx]
                 [dummy : toplevel])]
               [#:struct (seq form) (
                 [forms : (Listof (U form Any))])]
               [#:struct (splice form) (
                 [forms : (Listof (U form Any))])]
               [#:struct (inline-variant form) (
                 [direct : expr]
                 [inline : expr])]
               [#:struct (mod form) (
                 [name : (U Symbol (Listof Symbol))]
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
               [#:struct (lam expr) (
                 [name : (U Symbol (Vectorof Any) (List ))] ;empty list
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
               [#:struct (closure expr) (
                 [code : lam]
                 [gen-id : Symbol])]
               [#:struct (case-lam expr) (
                 [name : (U Symbol (Vectorof Any) (List ))]
                 [clauses : (Listof (U lam closure))])]
               [#:struct (let-one expr) (
                 [rhs : (U expr seq Any)]  ; pushes one value onto stack
                 [body : (U expr seq Any)] 
                 [type : (U #f 'flonum 'fixnum 'extflonum)]
                 [unused? : Boolean])]
               [#:struct (let-void expr) (
                 [count : Exact-Nonnegative-Integer]
                 [boxes? : Boolean]
                 [body : (U expr seq Any)])]
               [#:struct (install-value expr) (
                 [count : Exact-Nonnegative-Integer]
                 [pos : Exact-Nonnegative-Integer]
                 [boxes? : Boolean]
                 [rhs : (U expr seq Any)]
                 [body : (U expr seq Any)])]
               [#:struct (let-rec expr) (
                 [procs : (Listof lam)]
                 [body : (U expr seq Any)])]
               [#:struct (boxenv expr) (
                 [pos : Exact-Nonnegative-Integer]
                 [body : (U expr seq Any)])]
               [#:struct (localref expr) (
                 [unbox? : Boolean]
                 [pos : Exact-Nonnegative-Integer]
                 [clear? : Boolean]
                 [other-clears? : Boolean]
                 [type : (U #f 'flonum 'fixnum 'extflonum)])]
               [#:struct (toplevel expr) (
                 [depth : Exact-Nonnegative-Integer]
                 [pos : Exact-Nonnegative-Integer]
                 [const? : Boolean]
                 [ready? : Boolean])]
               [#:struct (topsyntax expr) (
                 [depth : Exact-Nonnegative-Integer]
                 [pos : Exact-Nonnegative-Integer]
                 [midpt : Exact-Nonnegative-Integer])]
               [#:struct (application expr) (
                 [rator : (U expr seq Any)]
                 [rands : (Listof (U expr seq Any))])]
               [#:struct (branch expr) (
                 [test : (U expr seq Any)]
                 [then : (U expr seq Any)]
                 [else : (U expr seq Any)])]
               [#:struct (with-cont-mark expr) (
                 [key : (U expr seq Any)]
                 [val : (U expr seq Any)]
                 [body : (U expr seq Any)])]
               [#:struct (beg0 expr) (
                 [seq : (Listof (U expr seq Any))])]
               [#:struct (varref expr) (
                 [toplevel : (U toplevel #t)]
                 [dummy : (U toplevel #f)])]
               [#:struct (assign expr) (
                 [id : toplevel]
                 [rhs : (U expr seq Any)]
                 [undef-ok? : Boolean])]
               [#:struct (apply-values expr) (
                 [proc : (U expr seq Any)]
                 [args-expr : (U expr seq Any)])]
               [#:struct (primval expr) (
                 [id : Exact-Nonnegative-Integer])]
               [#:struct (top-level-rename wrap) (
                 [flag : Boolean])]
               [#:struct (mark-barrier wrap) (
                 [value : Symbol])]
               [#:struct (lexical-rename wrap) (
                 [has-free-id-renames? : Boolean]
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
               [#:struct (phase-shift wrap) (
                 [amt : (U Integer #f)]
                 [src : (U Module-Path-Index #f)]
                 [dest : (U Module-Path-Index #f)]
                 [cancel-id : (U Integer #f)])]
               [#:struct (module-rename wrap) (
                 [phase : (U Integer #f)]
                 [kind : (U 'marked 'normal)]
                 [set-id : Any]
                 [unmarshals : (Listof all-from-module)]
                 [renames : (Listof (Pair Symbol module-binding))]
                 [mark-renames : Any] 
                 [plus-kern? : Boolean])]
               [#:struct (wrap-mark wrap) ([val : Integer])]
               [#:struct (prune wrap) ([sym : Any])]
               [#:struct (simple-module-binding module-binding) (
                 [path : Module-Path-Index])]
               [#:struct (phased-module-binding module-binding) (
                 [path : Module-Path-Index]
                 [phase : Integer]
                 [export-name : Any]
                 [nominal-path : nominal-path]
                 [nominal-export-name : Any])]
               [#:struct (exported-nominal-module-binding module-binding) (
                 [path : Module-Path-Index]
                 [export-name : Any]
                 [nominal-path : nominal-path]
                 [nominal-export-name : Any])]
               [#:struct (nominal-module-binding module-binding) (
                 [path : Module-Path-Index]
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
