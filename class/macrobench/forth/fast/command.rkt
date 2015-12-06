#lang typed/racket/base

(provide
  command%
  make-CMD*
)

;; -----------------------------------------------------------------------------

(require
 racket/match
 typed/racket/class
 "command-types.rkt"
 (only-in racket/string string-join)
 (for-syntax racket/base racket/syntax syntax/parse)
)

;; =============================================================================
;; -- Commands

(: command% Command%)
(define command%
  (class object%
    (super-new)
    (init-field
      id
      descr
      exec)))

;; True if the argument is a list with one element
(define-predicate singleton-list? (List Any))

(define-type Binop-Command%
  (Class
   (init
     (binop (-> Integer Integer Integer))
     (descr String))
   (field
     (binop (-> Integer Integer Integer))
     (descr String)
     (exec (-> Any (U False 'EXIT Void)))
     (id Symbol))))

;; Create a binary operation command.
;; Command is recognized by its identifier,
;;  the identifier is then applied to the top 2 numbers on the stack.
(: binop-command% (-> Stack Binop-Command%))
(define (binop-command% S)
  (class command%
    (init-field
     binop)
    (super-new
      (id (assert (object-name binop) symbol?))
      (exec (lambda ([v : Any])
        (if (singleton-list? v)
          (if (eq? (car v) (get-field id this))
             (let ([v1 (send S stack-pop)]
                   [v2 (send S stack-pop)])
               (send S stack-push (binop v2 v1))
               (void))
             #f)
           #f))))))

;; Turns a symbol into a stack command parser
(define-syntax make-stack-command
  (syntax-parser
   [(_ opcode:id d:str)
    #:with stack-cmd (format-id #'opcode "stack-~a" (syntax-e #'opcode))
    #`(lambda ([S : Stack]) (new command%
        (id '#,(syntax-e #'opcode))
        (descr d)
        (exec (lambda ([v : Any])
          (and (singleton-list? v)
               (eq? '#,(syntax-e #'opcode) (car v))
               (send S stack-cmd)
               (void))))))]))

;; Default environment of commands
(: make-CMD* (-> Stack (Listof (Instance Command%))))
(define (make-CMD* S) (list
  (new command%
    (id 'exit)
    (descr "End the REPL session")
    (exec (lambda ([v : Any])
      (if (or (eof-object? v)
              (and (symbol? v)
                   (exit? v))
              (and (list? v)
                   (not (null? v))
                   (exit? (car v))))
          'EXIT
          #f))))
  (new command%
   (id 'help)
   (descr "Print help information")
   (exec (lambda ([v : Any])
     (cond
      [(and (symbol? v) (help? v))
       (displayln "hellllp")
       (void)]
      [(and (list? v) (not (null? v)) (help? (car v)))
       ;(displayln (show-help E (and (not (null? (cdr v))) (cdr v))))
       (void)]
      [else
       #f]))))
  (instantiate (binop-command% S) (+) (descr "Add the top two numbers on the stack"))
  (instantiate (binop-command% S) (-) (descr "Subtract the top item of the stack from the second item."))
  (instantiate (binop-command% S) (*) (descr "Multiply the top two item on the stack."))
  ;(instantiate (binop-command% (/) (descr "Divide the top item of the stack by the second item."))
  ((make-stack-command drop "Drop the top item from the stack") S)
  ((make-stack-command dup  "Duplicate the top item of the stack") S)
  ((make-stack-command over "Duplicate the top item of the stack, but place the duplicate in the third position of the stack.") S)
  ((make-stack-command swap "Swap the first two numbers on the stack") S)
  (new command%
    (id 'push)
    (descr "Push a number onto the stack")
    (exec (lambda ([v : Any])
      (match v
        [`(push ,(? exact-integer? n))
         (send S stack-push n)
         (void)]
        [`(,(? exact-integer? n))
         (send S stack-push n)
         (void)]
        [_ #f]))))
  (new command%
    (id 'show)
    (descr "Print the current stack")
    (exec (lambda ([v : Any])
      (match v
        [`(,(? show?))
         (displayln S)
         (void)]
        [_ #f]))))
))

(: exit? (-> Any Boolean))
(define (exit? sym)
  (and (memq sym '(exit quit q leave bye)) #t))

(: help? (-> Any Boolean))
(define (help? sym)
  (and (memq sym '(help ? ??? -help --help h)) #t))

(: show? (-> Any Boolean))
(define (show? sym)
  (and (memq sym '(show print pp ls stack)) #t))

;;; Search the environment for a command with `id` equal to `sym`
;(: find-command (-> Env Symbol (Option (Instance Command%))))
;(define (find-command E sym)
;  (for/or : (Option (Instance Command%)) ([c : (Instance Command%) (in-list E)])
;    (get-field id c) (error 'no)))
;    ;(if (eq? sym (get-field id c)) c #f)))

;; Print a help message.
;; If the optional argument is given, try to print information about it.
;(: show-help (->* [Env] [Any] String))
;(define (show-help E [v #f])
;  (match v
;    [#f
;     (string-join
;      (for/list : (Listof String) ([c : (Instance Command%) (in-list E)])
;        (format "    ~a : ~a" (get-field id c) (get-field descr c)))
;      "\n"
;      #:before-first "Available commands:\n")]
;    [(or (list (? symbol? s)) (? symbol? s))
;     (define c (find-command E (assert s symbol?)))
;     (if c
;         (get-field descr c)
;         (format "Unknown command '~a'" s))]
;    [x
;     (format "Cannot help with '~a'" x)]))
;
