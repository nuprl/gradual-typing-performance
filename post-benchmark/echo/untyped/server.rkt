#lang racket/base

;; TCP server: read from a buffer until end of file.

(provide server)

(require "constants.rkt"
         (only-in racket/tcp tcp-accept tcp-listen))

;; ---------------------------------------------------------------------------------------------------

(define (server)
  (define-values (in out) (tcp-accept (tcp-listen PORT 5 #t)))
  (define buffer (make-string (string-length DATA)))
  (file-stream-buffer-mode out 'none)
  (let loop ([i (read-string! buffer in)]
             [bytes 0])
    (cond [(not (eof-object? i))
           (display buffer out)
           (loop (read-string! buffer in)
             (+ bytes (string-length buffer)))]
          [else (printf "server processed ~a bytes\n" bytes)])))

