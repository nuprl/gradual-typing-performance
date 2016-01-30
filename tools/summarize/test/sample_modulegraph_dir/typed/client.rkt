#lang typed/racket/base

;; TCP client bot: loop for a fixed number of iterations
;; sending a message over a port.
;; The message and port are defined in constants.rkt

(provide client)

(require benchmark-util
         (only-in racket/tcp tcp-connect))

(require/typed/check "constants.rkt"
  [PORT Natural]
  [DATA String])

;; ---------------------------------------------------------------------------------------------------

;; `client n` loop for `n` iterations, sending a constant message on a constant port.
(: client (-> Natural Void))
(define (client num-iters)
  (define-values (in out) (tcp-connect "127.0.0.1" PORT))
  (define buffer (make-string (string-length DATA)))
  (file-stream-buffer-mode out 'none)
  (for ([n num-iters])
    (display DATA out)
    (read-string! buffer in)
    (unless (equal? DATA buffer)
        (error (format "Unexpected data ~e in buffer" DATA))))
  (close-output-port out))

