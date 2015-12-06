#lang racket/base

(require racket/class)

;; Forth stacks,
;;  data definition & operations

(provide
  stack%
)

(require
  "command-types.rkt")

;; =============================================================================


(define stack%
  (class object%
    (super-new)
    (field [S '()])
    (define/public (stack-drop)
      (let ([v (send this stack-pop)])
        (void)))

    (define/public (stack-dup)
      (let ([v (send this stack-pop)])
        (send this stack-push v)
        (send this stack-push v)
        (void)))

    (define/public (stack-over)
      (let ([v1 (send this stack-pop)]
            [v2 (send this stack-pop)])
        (send this stack-push v1)
        (send this stack-push v2)
        (send this stack-push v1)
        (void)))

    (define/public (stack-pop)
      (if (null? S)
          (raise-user-error "empty stack")
          (car S)))

    (define/public (stack-push v)
      (set-field! S this (cons v (get-field S this))))

    (define/public (stack-swap)
      (let ([v1 (send this stack-pop)]
            [v2 (send this stack-pop)])
        (send this stack-push v1)
        (send this stack-push v2)
        (void)))
))
