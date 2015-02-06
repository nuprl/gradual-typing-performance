#lang typed/racket/base

;; Simple utility for searching zo structs.
;; Explores the current struct's fields recursively for a exact string match.

(provide
 ;; (-> zo String [#:limit (U Natural #f)] (Listof result))
 ;; Search a struct recursively for member zo-structs matching a string.
 zo-find
 ;; Search result: a zo-struct and the path to reach it
 result result? result-z result-path)

(require (only-in racket/list empty?)
         (only-in racket/string string-split string-trim)
         (only-in "zo-transition.rkt" zo-transition)
         (only-in "zo-string.rkt" zo->spec Spec)
         "typed-zo-structs.rkt"
         racket/match)

;; -----------------------------------------------------------------------------

;; --- API functions

(struct result ([z : zo]
                [path : (Listof zo)])
        #:transparent)

(: append-all (All (A) (-> (Listof (Listof A)) (Listof A))))
(define (append-all xss)
  (cond [(empty? xss) '()]
        [else (append (car xss) (append-all (cdr xss)))]))

;; Searches a zo-struct `z` recursively for member zo-structs matching the `s`.
;; Terminates after at most `#:limit` recursive calls.
;; Return a list of 'result' structs.
(: zo-find (-> zo String [#:limit (U Natural #f)] (Listof result)))
(define (zo-find z str #:limit [lim #f])
  ;; (-> zo? string? (listof result?))
  (define-values (_ children) (parse-zo z))
  (append-all (for/list : (Listof (Listof result)) ([z* : zo children])
                        (zo-find-aux z* '() str 1 lim '()))))

;; ;; --- private functions

;; Check if `str` is one of the known looping zo-structs.
;; 2015-01-23: So far as I know, only closures may loop.
(: may-loop? (-> String Boolean))
(define (may-loop? str)
  (if (member str (list "closure"))
      #t #f))

;; Recursive helper for `zo-find`.
;; Add the current struct to the results, if it matches.
;; Check struct members for matches unless the search has reached its limit.
(: zo-find-aux (-> zo (Listof zo) String Natural (U Natural #f) (Listof zo) (Listof result)))
(define (zo-find-aux z hist str i lim seen)
  (define-values (title children) (parse-zo z))
  (: results (Listof result))
  (define results
    (cond
     [(and lim (<= lim i))
      '()]
     ;; Terminate search if we're seeing a node for the second time
     [(and (may-loop? title) (member z seen))
      '()]
     [else
      ;; Remember current node if we might see it again.
      (: seen* (Listof zo))
      (define seen* (if (may-loop? title) (cons z seen) seen))
      (: hist* (Listof zo))
      (define hist* (cons z hist))
      (append-all (for/list : (Listof (Listof result)) ([z* : zo children])
                              (zo-find-aux z* hist* str (add1 i) lim seen*)))]))
  (if (and (string=? str title) (not (member z seen)))
      (cons (result z hist) results)
      results))

;; Return the name of the zo `z` and a list of its child zo structs.
;; Uses `zo-string.rkt` to parse a raw struct.
(: parse-zo (-> zo (values String (Listof zo))))
(define (parse-zo z)
  (: z-spec Spec)
  (define z-spec     (zo->spec z))
  (: title String)
  (define title      (car z-spec))
  (: child-strs (Listof String))
  (define child-strs (for/list : (Listof String) ([pair : (Pair String (-> (U String Spec))) (cdr z-spec)])
                               (car pair)))
  (values title (get-children z child-strs)))

;; Given a zo `z` and list of possible field names `strs`, return the list
;; of zo-structs obtained by looking up each name in `strs` in the struct `z`.
;; Relies on `zo-transition.rkt` to do the lookup.
(: get-children (-> zo (Listof String) (Listof zo)))
(define (get-children z strs)
  (match strs
    ['() '()]
    [(cons hd tl)
     (define-values (r* success*) (zo-transition z hd))
     (: r (U zo (Listof zo)))
     (define r r*)
     (: success? Boolean)
     (define success? success*)
     (cond [(not success?) (get-children z tl)]
           [(list? r)      (append (filter zo? r) (get-children z tl))]
           [(zo?   r)      (cons r (get-children z tl))])]))
                
;; -----------------------------------------------------------------------------
;; --- testing

;; (module+ test
;;   (require rackunit
;;            compiler/zo-structs)

;;   ;; --- API
;;   ;; Success, one search path
;;   (let* ([z   (branch #t #t (branch #t #t (branch #t #t (branch #t #t #t))))]
;;          [arg "branch"]
;;          [res (zo-find z arg)])
;;     (begin (check-equal? (length res) 3)
;;            (check-equal? (result-z (car res))   (branch-else z))
;;            (check-equal? (result-path (car res)) '())))

;;   ;; Success, #:limit-ed results
;;   (let* ([z   (branch #t #t (branch #t #t (branch #t #t (branch #t #t #t))))]
;;          [arg "branch"]
;;          [res (zo-find z arg #:limit 2)])
;;     (begin (check-equal? (length res) 2)
;;            (check-equal? (result-z (cadr res)) (branch-else (branch-else z)))
;;            (check-equal? (result-path (cadr res)) (list (branch-else z)))))

;;   ;; This test was problematic in REPL. Should succeed
;;   (let* ([tgt (wrap-mark 42)]
;;          [z (wrapped #f (list tgt tgt tgt) 'tainted)]
;;          [arg "wrap-mark"]
;;          [res (zo-find z arg)])
;;     (begin (check-equal? (length res) 3)
;;            (check-equal? (result-z (car res)) tgt)))

;;   ;; Fail, no results
;;   (let* ([z (primval 8)]
;;          [arg "apply-values"]
;;          [res (zo-find z arg)])
;;     (check-equal? res '()))

;;   ;; Fail, search excludes root
;;   (let* ([z (primval 8)]
;;          [arg "primval"]
;;          [res (zo-find z arg)])
;;     (check-equal? res '()))

;;   ;; --- private
;;   ;; -- find-aux
;;   ;; Success, search INCLUDES root (empty history)
;;   (let* ([z (primval 8)]
;;          [arg "primval"]
;;          [res (zo-find-aux z '() arg 1 10 '())])
;;     (begin (check-equal? (length res) 1)
;;            (check-equal? (result-z (car res)) z)
;;            (check-equal? (result-path (car res)) '())))

;;   ;; Success, search INCLUDES root (make sure history is passed in result)
;;   (let* ([z (primval 8)]
;;          [arg "primval"]
;;          [hist '(a b c d)]
;;          [res (zo-find-aux z hist arg 1 10 '())])
;;     (begin (check-equal? (result-z (car res)) z)
;;            (check-equal? (result-path (car res)) hist)))

;;   ;; Failure, search at limit (remember, find-aux searches the head)
;;   (let* ([z (branch #t #t (primval 8))]
;;          [arg "primval"]
;;          [hist '()]
;;          [res (zo-find-aux z hist arg 9 9 '())])
;;     (check-equal? res '()))

;;   ;; Failure, search past limit
;;   (let* ([z (branch #t #t (primval 8))]
;;          [arg "primval"]
;;          [hist '()]
;;          [res (zo-find-aux z hist arg 9 1 '())])
;;     (check-equal? res '()))

;;   ;; Success, searching a few branches
;;   (let* ([tgt (inline-variant (branch #f #f #f) (branch #f #f #f))]
;;          [z   (with-cont-mark (let-one (boxenv 7 #f) (localref #t 1 #t #t #f) #f #t)
;;                               (seq (list tgt))
;;                               #f)]
;;          [arg "inline-variant"]
;;          [hist '(a b)]
;;          [res (zo-find-aux z hist arg 1 10 '())])
;;     (begin (check-equal? (length res) 1)
;;            (check-equal? (result-z (car res)) tgt)
;;            (check-equal? (result-path (car res)) (cons (with-cont-mark-val z) (cons z hist)))))

;;   ;; Success, find multiple results
;;   (let* ([tgt (topsyntax 1 2 3)]
;;          [z   (application (beg0 (list (beg0 (list tgt))))
;;                            (list (primval 3) (primval 4) tgt tgt))]
;;          [arg "topsyntax"]
;;          [hist '(a b c)]
;;          [res (zo-find-aux z hist arg 1 10 '())])
;;     (begin (check-equal? (length res) 3)
;;            (check-equal? (result-z (car res)) tgt)
;;            (check-equal? (result-z (cadr res)) tgt)
;;            (check-equal? (result-z (caddr res)) tgt)
;;            ;; Verify one history
;;            (check-equal? (result-path (car res)) (cons (car (beg0-seq (application-rator z)))
;;                                                        (cons (application-rator z)
;;                                                              (cons z hist))))))

;;   ;; Failure, thing is already seen
;;   (let* ([z (closure (lam 'N '() 0 '() #f '#() '() #f 0 #f) 'C)]
;;          [arg "lam"]
;;          [res (zo-find-aux z '() arg 1 10 (list z))])
;;     (begin (check-equal? (length res) 0)))
  
;;   ;; Success, it's a closure but we have not seen it
;;   (let* ([z (closure (lam 'N '() 0 '() #f '#() '() #f 0 #f) 'C)]
;;          [arg "lam"]
;;          [res (zo-find-aux z '() arg 1 10 '())])
;;     (begin (check-equal? (length res) 1)
;;            (check-equal? (result-z (car res)) (closure-code z))))

;;   ;; Checking that we don't add already-seen things
;;   (let* ([z (closure (lam 'N '() 0 '() #f '#() '() #f 0 #f) 'C)]
;;          [arg "closure"]
;;          [res (zo-find-aux z '() arg 1 10 (list z))])
;;     (begin (check-equal? (length res) 0)))

;;   ;; -- parse-zo
;;   ;; Simple zo, no interesting fields
;;   (let ([z (topsyntax 1 2 3)])
;;     (let-values ([(title children) (parse-zo z)])
;;       (begin (check-equal? title "topsyntax")
;;              (check-equal? (length children) 0))))

;;   ;; Three interesting fields
;;   (let ([z (branch (branch #t #t #t) (branch #t #t #f) (branch #t #f #f))])
;;     (let-values ([(title children) (parse-zo z)])
;;       (begin (check-equal? title             "branch")
;;              (check-equal? (length children) 3)
;;              (check-equal? (car children)    (branch #t #t #t)))))

;;   ;; 2 of 3 fields are interesting
;;   (let ([z (branch #f (branch #t #t #f) (branch #t #f #f))])
;;     (let-values ([(title children) (parse-zo z)])
;;       (begin (check-equal? title             "branch")
;;              (check-equal? (length children) 2)
;;              (check-equal? (car children)    (branch #t #t #f)))))

;;   ;; Nested children are not returned
;;   (let* ([tgt (beg0 (list (beg0 '())))]
;;          [z (apply-values tgt
;;                           (assign (toplevel 1 1 #t #t) #f #f))])
;;     (let-values ([(title children) (parse-zo z)])
;;       (begin (check-equal? title            "apply-values")
;;              (check-equal? (length children) 2)
;;              (check-equal? (car children)    tgt))))

;;   ;; -- get-children
;;   ;; Two valid fields, only 1 result
;;   (let* ([tgt  (toplevel 1 1 #t #f)]
;;          [z    (def-values (list 'A 'B 'C tgt) #f)]
;;          [args (list "ids" "rhs")]
;;          [res  (get-children z args)])
;;     (begin (check-equal? (length res) 1)
;;            (check-equal? (car res) tgt)))

;;   ;; Two fields, 2 results
;;   (let* ([tgt  (lam 'name '() 0 '() #f '#() '() #f 0 #f)]
;;          [z    (inline-variant tgt (let-rec '() #f))]
;;          [args (list "inline" "direct")]
;;          [res  (get-children z args)])
;;     (begin (check-equal? (length res) 2)
;;            (check-pred (lambda (x) (memq tgt res)) '())))

;;   ;; Only search 1 of 2 possible fields
;;   (let* ([tgt  (lam 'name '() 0 '() #f '#() '() #f 0 #f)]
;;          [z    (inline-variant tgt (let-rec '() #f))]
;;          [args (list "direct")]
;;          [res  (get-children z args)])
;;     (begin (check-equal? (length res) 1)
;;            (check-equal? (car res) tgt)))

;;   ;; Failure, search no valid fields
;;   (let* ([tgt  (lam 'name '() 0 '() #f '#() '() #f 0 #f)]
;;          [z    (inline-variant tgt (let-rec '() #f))]
;;          [args '()]
;;          [res  (get-children z args)])
;;     (check-equal? (length res) 0))

;;   (let* ([tgt  (lam 'name '() 0 '() #f '#() '() #f 0 #f)]
;;          [z    (inline-variant tgt (let-rec '() #f))]
;;          [args (list "outline" "NOTHING")]
;;          [res  (get-children z args)])
;;     (check-equal? (length res) 0))

;;   ;; Failure, no fields are zo
;;   (let* ([z    (let-void 777 #f 'NOTHING)]
;;          [args (list "count" "boxes?" "body" "something" "anything" "zo")]
;;          [res  (get-children z args)])
;;     (check-equal? (length res) 0))
;; )
