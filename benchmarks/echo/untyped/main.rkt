#lang racket/base

(require benchmark-util)
(require/check
  (only-in "client.rkt" client)
  (only-in "server.rkt" server))

;; ---------------------------------------------------------------------------------------------------

(define (main arg)
  (thread (lambda () (client arg)))
  (server))

(time (main 200000))
