#lang typed/racket/base

(require/typed "message.rkt" [message String])

(printf "~a\n" message)

;; -----------------------------------------------------------------------------
;; --- output
;; [BG:CREATE]	0	main.rkt	message	message.rkt
;; [BG:APPLY]	0	main.rkt	message	message.rkt
;; HELLO WORLD
