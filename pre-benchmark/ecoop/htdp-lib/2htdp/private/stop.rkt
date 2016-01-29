#lang typed/racket

(require "world-type.rkt")

(provide (struct-out stop-the-world))

(define-struct: stop-the-world ([world : World]) #:transparent)
