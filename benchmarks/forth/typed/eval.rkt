#lang typed/racket/base

(provide forth-eval*)

;; -----------------------------------------------------------------------------

(require
  require-typed-check
  racket/match
  typed/racket/class
  "../base/command-types.rkt"
  (only-in racket/port with-input-from-string)
)
(require/typed/check "command.rkt"
  (CMD* (Listof (Instance Command%)))
  (command% Command%)
)
(require/typed/check "stack.rkt"
  (stack-init (-> Stack))
)

;; =============================================================================

(: defn-command (Instance Command%))
(define defn-command
  (new command%
    (id 'define)
    (descr "Define a new command as a sequence of existing commands")
    (exec (lambda ([E : Env] [S : Stack] [v : Any])
      (match v
       [(cons (or ': 'define) (cons w defn*-any))
        (define defn* (assert defn*-any list?))
        (define cmd
          (new command%
            (id (assert w symbol?))
            (descr (format "~a" defn*))
            (exec (lambda ([E : Env] [S : Stack] [v : Any])
              (if (equal? v (list w))
                  (let-values ([(e+ s+)
                                (for/fold : (Values (Option Env) Stack)
                                    ([e (ann E (Option Env))] [s S])
                                    ([d (in-list defn*)])
                                  (if e
                                    (forth-eval e s (list d))
                                    (values e s)))])
                    (if e+
                      (cons e+ s+)
                      e+))
                  #f)))))
        (cons (cons cmd E) S)]
       [_ #f])))))

(: forth-eval* (-> Input-Port (Values (Option Env) Stack)))
(define (forth-eval* in)
  (for/fold : (Values (Option Env) Stack)
            ([e (ann (cons defn-command CMD*) (Option Env))]
             [s (stack-init)])
      ([ln (in-lines in)])
    (define token* (forth-tokenize ln))
    (cond
     [(or (null? token*)
          (not (list? e))) ;; Cheap way to detect EXIT
      (values '() s)]
     [else
      (forth-eval e s token*)])))

(: forth-eval (-> Env State (Listof Any) (Values (Option Env) Stack)))
(define (forth-eval E S token*)
  (match (for/or : Result
                 ([c (in-list E)]) ((get-field exec c) E S token*))
    ['EXIT
     (values #f S)]
    [#f
     (printf "Unrecognized command '~a'.\n" token*)
     (values E S)]
    [(? pair? E+S)
     (values (car E+S) (cdr E+S))]))

(: forth-tokenize (-> String (Listof Any)))
(define (forth-tokenize str)
  (parameterize ([read-case-sensitive #f]) ;; Converts symbols to lowercase
    (with-input-from-string str
      (lambda ()
        (de-nest
         (let loop ()
           (match (read)
             [(? eof-object?) '()]
             [val (cons val (loop))])))))))

;; Remove all parentheses around a singleton list
(: de-nest (-> (Listof Any) (Listof Any)))
(define (de-nest v*)
  (if (and (list? v*)
           (not (null? v*))
           (list? (car v*))
           (null? (cdr v*)))
      (de-nest (car v*))
      v*))

