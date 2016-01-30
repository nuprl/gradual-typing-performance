#lang typed/racket/base

(provide
  Stack%
  Stack
  Env
  Val
  State
  Result
  Command%)

(require typed/racket/class)

(define-type Stack%
  (Class
    (field [S (Listof Integer)])
    (stack-drop (-> Void))
    (stack-dup (-> Void))
    (stack-over (-> Void))
    (stack-pop (-> Integer))
    (stack-push (-> Integer Void))
    (stack-swap (-> Void))
))
(define-type Stack (Instance Stack%))
(define-type Env (Boxof (Listof (Instance Command%))))
(define-type Val (U Symbol Integer))
(define-type State Stack)
(define-type Result (U 'EXIT #f Void))

(define-type Command%
  (Class
    (init-field
      [id Symbol]
      [descr String]
      [exec (-> Any Result)])))

