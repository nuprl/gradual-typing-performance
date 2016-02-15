#lang racket/base

;; Simple utility for searching zo structs.
;; Explores the current struct's fields recursively for a exact string match.

(provide
  ;; (-> zo? string? (listof result?))
 ;; Search a struct recursively for member zo-structs matching a string.
 zo-find
 ;; Search result: a zo-struct and the path to reach it
 (struct-out result))

(require (only-in racket/list empty?)
         (only-in racket/string string-split string-trim)
         (only-in compiler/zo-structs zo?)
         (only-in "zo-transition.rkt" zo-transition)
         (only-in "zo-string.rkt" zo->spec)
         racket/match)

;; -----------------------------------------------------------------------------

;; --- API functions

(struct result (z path) #:transparent)

;; Searches a zo-struct `z` recursively for member zo-structs matching the `s`.
;; Terminates after at most `#:limit` recursive calls.
;; Return a list of 'result' structs.
(define (zo-find z str #:limit [lim #f])
  ;; (-> zo? string? (listof result?))
  (define-values (_ children) (parse-zo z))
  (apply append (for/list ([z* children]) (zo-find-aux z* '() str 1 lim '()))))

;; --- private functions

;; Check if `str` is one of the known looping zo-structs.
;; 2015-01-23: So far as I know, only closures may loop.
(define (may-loop? str)
  ;; (-> string? boolean?)
  (member str (list "closure")))

;; Recursive helper for `zo-find`.
;; Add the current struct to the results, if it matches.
;; Check struct members for matches unless the search has reached its limit.
(define (zo-find-aux z hist str i lim seen)
  (define-values (title children) (parse-zo z))
  (define results
    (cond
     [(and lim (<= lim i))
      '()]
     ;; Terminate search if we're seeing a node for the second time
     [(and (may-loop? title) (member z seen))
      '()]
     [else
      ;; Remember current node if we might see it again.
      (define seen* (if (may-loop? title) (cons z seen) seen))
      (define hist* (cons z hist))
      (apply append (for/list ([z* children]) (zo-find-aux z* hist* str (add1 i) lim seen*)))]))
  (if (and (string=? str title) (not (member z seen)))
      (cons (result z hist) results)
      results))

;; Return the name of the zo `z` and a list of its child zo structs.
;; Uses `zo-string.rkt` to parse a raw struct.
(define (parse-zo z)
  ;; (-> zo? (values string? (listof zo?)))
  (define z-spec     (zo->spec z))
  (define title      (car z-spec))
  (define child-strs (for/list ([pair (cdr z-spec)]) (car pair)))
  (values title (get-children z child-strs)))

;; Given a zo `z` and list of possible field names `strs`, return the list
;; of zo-structs obtained by looking up each name in `strs` in the struct `z`.
;; Relies on `zo-transition.rkt` to do the lookup.
(define (get-children z strs)
  ;; (-> zo? string? (listof zo?))
  (match strs
    ['() '()]
    [(cons hd tl)
     (define-values (r success?) (zo-transition z hd))
     (cond [(not success?) (get-children z tl)]
           [(list? r)      (append (filter zo? r) (get-children z tl))]
           [(zo?   r)      (cons r (get-children z tl))])]))
                
