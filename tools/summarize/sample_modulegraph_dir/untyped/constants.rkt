#lang racket/base

(provide
  ;; Natural number port number to run the echo server on
  PORT
  ;; String message to send over the tcp connection
  DATA)

(define PORT 8887)
(define DATA "Hello there sailor\n")
