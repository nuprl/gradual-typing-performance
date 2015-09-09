#lang typed/racket/base

;; Access the fields of a struct by name at runtime.

;; Uses predicates to guess what struct its argument is,
;; then compares strings with statically-known field names.
;; Functions that end with '->' are the specific transition function
;; for a type of zo struct.

(provide
 ;; (-> zo String (values (U zo (Listof zo)) Boolean)))
 ;; Access "structName-fieldName myStruct" at runtime.
 zo-transition)

(require racket/match
         (only-in racket/list empty? empty)
         "../base/typed-zo-structs.rkt")

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

