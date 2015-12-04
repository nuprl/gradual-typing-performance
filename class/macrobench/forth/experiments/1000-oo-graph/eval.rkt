#lang racket/base

(provide forth-eval*)

;; -----------------------------------------------------------------------------

(require
  racket/match
  racket/class
  (only-in racket/port with-input-from-string)
)
(require (only-in "command.rkt"
  make-CMD*
  command%
))
(require (only-in "stack.rkt"
  stack%
))

(define (assert v p)
  (unless (p v) (error 'assert))
  v)

;; =============================================================================

(define (defn-command S E)
  (define forth-eval (make-forth-eval S E))
  (new command%
    (id 'define)
    (descr "Define a new command as a sequence of existing commands")
    (exec (lambda (v)
      (match v
       [(cons (or ': 'define) (cons w defn*-any))
        (define defn* (assert defn*-any list?))
        (define cmd
          (new command%
            (id (assert w symbol?))
            (descr (format "~a" defn*))
            (exec (lambda (v)
              (if (equal? v (list w))
                  (for
                                    ([d (in-list defn*)])
                                    (forth-eval (list d)))
                  #f)))))
        (set-box! E (cons cmd (unbox E)))]
       [_ #f])))))

(define (forth-eval* in)
  (define S (new stack%))
  (define e (box (make-CMD* S)))
  (define defn (defn-command S e))
  (set-box! e (cons defn (unbox e)))
  (define forth-eval (make-forth-eval S e))
  (for
      ([ln (in-lines in)])
    (define token* (forth-tokenize ln))
    (cond
     [(null? token*)
      '()]
     [else
      (forth-eval token*)]))
  (void))

(define ((make-forth-eval S E) token*)
  (match (for/or
                 ([c (in-list (unbox E))]) ((get-field exec c) token*))
    ['EXIT
     #f]
    [#f
     (printf "Unrecognized command '~a'.\n" token*)]
    [_ (void)]))

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
(define (de-nest v*)
  (if (and (list? v*)
           (not (null? v*))
           (list? (car v*))
           (null? (cdr v*)))
      (de-nest (car v*))
      v*))

