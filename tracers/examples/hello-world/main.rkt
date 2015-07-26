#lang typed/racket/base

(require/typed "message.rkt" [message String])

(printf "~a\n" message)
