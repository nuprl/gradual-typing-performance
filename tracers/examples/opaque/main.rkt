#lang typed/racket/base

(require/typed "data.rkt"
  [#:opaque ADT adt?]
  [v1 ADT]
  [get-tag (-> ADT Symbol)])

(define t (get-tag v1))

;; -----------------------------------------------------------------------------
;; --- output
;; [BG:CREATE]	0	main.rkt	adt?	data.rkt
;; [BG:CREATE]	1	main.rkt	v1	data.rkt
;; [BG:APPLY]	0	main.rkt	adt?	data.rkt
;; [BG:APPLY]	1	main.rkt	v1	data.rkt
;; [BG:CREATE]	2	main.rkt	get-tag	data.rkt
;; [BG:APPLY]	2	main.rkt	get-tag	data.rkt
