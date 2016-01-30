#lang scribble/manual
@(require scribble/eval
          scriblib/autobib
          (for-label racket/base)
          )

@title{Typed}
Fortunately, it is not difficult to add types to the code above. Put all of the following files in
the @tt{benchmarks/echo/typed/} directory.
The only changes we need to make are to use typed racket and add a few annotations.

First, we annotate the constants in @tt{constants.rkt}:
@(racketmod typed/racket/base

(provide
  (code:comment "Natural number port number to run the echo server on")
  PORT
  (code:comment "String message to send over the tcp connection")
  DATA)

(: PORT Natural)
(define PORT 8887)

(: DATA String)
(define DATA "Hello there sailor\n"))

For @tt{server.rkt} we add an annotation, and we annotate our import of @tt{constants.rkt} for
the cases where @tt{server.rkt} is typed and @tt{constants.rkt} is untyped:
@(racketmod typed/racket/base

(code:comment "TCP server: read from a buffer until end of file.")

(provide server)

(require benchmark-util
         (only-in racket/tcp tcp-accept tcp-listen))

(require/typed/check "constants.rkt"
                     [PORT Natural]
                     [DATA String])

(code:comment "---------------------------------------------------------------------------------------------------")
(: server (-> Void))
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

@tt{client.rkt} is similar:
@(racketmod typed/racket/base

(code:comment "TCP client bot: loop for a fixed number of iterations")
(code:comment "sending a message over a port.")
(code:comment "The message and port are defined in constants.rkt")

(provide client)

(require benchmark-util
         (only-in racket/tcp tcp-connect))

(require/typed/check "constants.rkt"
  [PORT Natural]
  [DATA String])

(code:comment "---------------------------------------------------------------------------------------------------")

(code:comment "`client n` loop for `n` iterations, sending a constant message on a constant port.")
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
  (close-output-port out)))

As is @tt{main.rkt}:
@(racketmod typed/racket/base

(require benchmark-util)

(require/typed/check "client.rkt"
  [client (-> Natural Void)])

(require/typed/check "server.rkt"
  [server (-> Void)])

(code:comment "---------------------------------------------------------------------------------------------------")

(: main (-> Natural Void))
(define (main arg)
  (thread (lambda () (client arg)))
  (server))

(time (main 200000)))

