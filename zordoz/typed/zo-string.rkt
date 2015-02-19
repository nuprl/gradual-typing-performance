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
 Spec)

;; --- string specifications

(require racket/match
         (only-in racket/list   empty?)
         (only-in racket/string string-join)
         (for-syntax racket/base racket/syntax)
         "../base/typed-zo-structs.rkt")

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

