#lang typed/racket/base

;; Yes! require/typed always makes a contract
;(require/typed "from.rkt" [x Natural])

;; require/typed/check will not make a contract unless we change its implementation
;; to just desugar to a require/typed
(require benchmark-util)
(require/typed/check "from.rkt" [x Natural])

(define y (add1 x))

;; -----------------------------------------------------------------------------
;; --- output
;; [BG:CREATE]	0	main.rkt	x	from.rkt
;; [BG:APPLY]	0	main.rkt	x	from.rkt
