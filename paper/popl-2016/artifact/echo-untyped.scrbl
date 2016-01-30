#lang scribble/manual
@(require scribble/eval
          scriblib/autobib
          (for-label racket/base)
          )

@title{Untyped}
We start with the @tt{untyped/} files.
A file @tt{constants.rkt} containing global constants:

@(racketmod
 racket/base

(provide
  (code:comment "Natural number port number to run the echo server on")
  PORT
  (code:comment "String message to send over the tcp connection")
  DATA)

(define PORT 8887)
(define DATA "Hello there sailor\n"))

@tt{server.rkt} which contains the actual echo server:

@(racketmod
 racket/base

(code:comment "TCP server: read from a buffer until end of file.")

(provide server)

(require "constants.rkt"
         (only-in racket/tcp tcp-accept tcp-listen))

(code:comment "---------------------------------------------------------------------------------------------------")

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
          [else (printf "server processed ~a bytes\n" bytes)]))))

@tt{client.rkt} which is a simple client that repeatedly sends the same message to the server:
@(racketmod
racket/base

(code:comment "TCP client bot: loop for a fixed number of iterations")
(code:comment "sending a message over a port.")
(code:comment "The message and port are defined in constants.rkt")

(provide client)

(require "constants.rkt"
         (only-in racket/tcp tcp-connect))
(code:comment "---------------------------------------------------------------------------------------------------")

(code:comment "`client n` loop for `n` iterations, sending a constant message on a constant port.")
(define (client num-iters)
  (define-values (in out) (tcp-connect "127.0.0.1" PORT))
  (define buffer (make-string (string-length DATA)))
  (file-stream-buffer-mode out 'none)
  (for ([n num-iters])
    (display DATA out)
    (read-string! buffer in)
    (unless (equal? DATA buffer)
        (error (format "Unexpected data ~e in buffer" DATA))))
  (close-output-port out)))

@; @todo{link to the time form}
and finally @tt{main.rkt}, which hooks the client up to the server and runs for a while. Importantly,
the file includes a usage of racket's @tt{time} form, which prints out the time that the block
inside it takes to execute. This is what the benchmarking script will parse as the runtime.

@(racketmod racket/base
 
(require (only-in "client.rkt" client)
         (only-in "server.rkt" server))

(code:comment "---------------------------------------------------------------------------------------------------")
; wtfffffffffff
(define (main arg)
        (thread (lambda () (client arg)))
        (server))

(time (main 200000)))
