#lang racket

(module t typed/racket
  (provide the-bomb)
  (require math/array)

  (: the-bomb (-> (Mutable-Array (Class))))
  (define (the-bomb)
    (raise-user-error 'not-implemented))
)

(require 't)
(void the-bomb)      ;; Funny, this line is necessary. Gotta use `the-bomb`.
