#lang typed/racket/base

(provide
  ;; Natural number port number to run the echo server on
  PORT
  ;; String message to send over the tcp connection
  DATA)

(: PORT Natural)
(define PORT 8887)

(: DATA String)
(define DATA "Hello there sailor\n")
