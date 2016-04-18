#lang typed/racket/base

(provide
  enqueue-message!
  reset-message-queue!
)

(define-type Queue (Listof String))

;; list of strings (messages) which were produced since the previous
;; previous display, and need to be displayed now
(: message-queue Queue)
(define message-queue '())

(: enqueue-message! (-> String Void))
(define (enqueue-message! m)
  (set! message-queue (cons m message-queue)))

(: reset-message-queue! (-> Void))
(define (reset-message-queue!)
  (set! message-queue '()))
