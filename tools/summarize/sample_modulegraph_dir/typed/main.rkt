#lang typed/racket/base

(require benchmark-util)

(require/typed/check "client.rkt"
  [client (-> Natural Void)])

(require/typed/check "server.rkt"
  [server (-> Void)])

;; ---------------------------------------------------------------------------------------------------

(: main (-> Natural Void))
(define (main arg)
  (thread (lambda () (client arg)))
  (server))

(time (main 200000))
