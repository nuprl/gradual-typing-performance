#lang racket/base

(require racket/system)

(define all-files (map symbol->string '(
collide.rkt
const.rkt
cut-tail.rkt
data.rkt
handlers.rkt
main.rkt
motion-help.rkt
motion.rkt
)))

(define (run)
  (system "mkdir foobar")
  (for ([fname (in-list all-files)])
    (printf "Testing ~a\n" fname)
    (system "rm foobar/*.rkt")
    (system "cp both/*.rkt untyped/*.rkt foobar/")
    (system (format "cp typed/~a foobar/" fname))
    (parameterize ([current-directory "foobar"])
      (unless (system "racket main.rkt")
        (error 'run-horizontal (format "Couldn't compile with '~a' typed" fname))))))

(module+ main
  (run))
