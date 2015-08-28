#lang typed/racket/base

(require benchmark-util)

(define-type Elem (Vectorof (U Char Symbol)))
(define-type Label label)

(require/typed/check "label.rkt"
 [#:struct label ([datum : Elem]
                  [i : Natural]
                  [j : Natural])]
 [set-label-i! (-> Label Natural Void)]
 [set-label-j! (-> Label Natural Void)]
 ;; --
 [label-element-equal? (-> Any Any Boolean)]
 [label->string (-> Label String)]
 [string->label (-> String Label)]
 [string->label/with-sentinel (-> String Label)]
 [make-label (-> (U String Elem) Label)]
 [label-source-eq? (-> Label Label Boolean)]
 [label-length (-> Label Index)]
 [vector->label (-> Elem Label)]
 [sublabel (case-> (-> label Index label)
                    (-> label Index Index label))]

 [label-copy (-> Label Label)]
 [label-same-source? (-> Label Label Boolean)]
 [label-ref-at-end? (-> Label Integer Boolean)]
 [label-ref (-> Label Integer (U Symbol Char))])

(provide
  Label
  label label? label-datum label-i label-j
  set-label-i! set-label-j!
  make-label
  sublabel
  label-element-equal?
  label->string
  string->label
  string->label/with-sentinel
  label-source-eq?
  label-length
  vector->label
  label-copy
  label-same-source?
  label-ref-at-end?
  label-ref)
