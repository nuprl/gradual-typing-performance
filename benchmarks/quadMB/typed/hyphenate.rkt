#lang typed/racket/base

;; bg: removed XML and TxExprs from the original; this only works on strings now

(provide
 hyphenate
 unhyphenate
 )

;; -----------------------------------------------------------------------------

(require
 require-typed-check
 (only-in racket/string string-replace string-join)
 (only-in racket/list partition drop-right drop make-list filter-not take splitf-at))
(require/typed/check "patterns-hashed.rkt"
 [hashed-patterns (HashTable String (Listof Index))])
(require/typed/check "exceptions.rkt"
  [default-exceptions (Listof Symbol)])

;; =============================================================================
;; bg: utilities for working with type Index
;; maybe we could drop these and go with type Integer everywhere

(: max-index (-> Index Index Index))
(define (max-index a b)
  (if (<= a b) b a))

(: min-index (-> Index Index Index))
(define (min-index a b)
  (if (<= a b) a b))

(: max-indexes (-> (Listof Index) Index))
(define (max-indexes xs)
  (for/fold : Index ([init : Index (car xs)]) ([next : Index (in-list (cdr xs))])
            (max-index init next)))

(: assert-index (-> Integer Index))
(define (assert-index i)
  (unless (index? i) (error (format "assert-index: ~a is not an index" i)))
  i)

;; -----------------------------------------------------------------------------

;; bg: changed default from #f
;; module data, define now but set! them later (because they're potentially big & slow)
(: patterns (HashTable String (Listof Index)))
(define patterns (make-hash))
(: pattern-cache (HashTable String (Listof Index)))
(define pattern-cache (make-hash))

;; module default values
(define default-min-length 5)
(define default-min-left-length 2)
(define default-min-right-length 2)
(define default-joiner #\u00AD)

;; bg: from racket docs http://docs.racket-lang.org/reference/hashtables.html?q=hash-empty#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._hash-empty~3f%29%29
(define-syntax-rule (hash-empty? hash)
  (zero? (hash-count hash)))

(: add-pattern-to-cache (-> (Pairof String (Listof Index)) Void))
(define (add-pattern-to-cache pat)
  (hash-set! pattern-cache (car pat) (cdr pat)))

;; bg: renamed from (initialize-patterns)
(define (initialize-patterns)
  (when (hash-empty? pattern-cache)
    (for ([e default-exceptions])
      (add-exception (symbol->string e))))
  (when (hash-empty? patterns)
    (set! patterns hashed-patterns)))

;; Convert the hyphenated pattern into a point array for use later.
(: add-exception (-> String Void))
(define (add-exception exception)
  (: make-key (-> String String))
  (define (make-key x) (format ".~a." (string-replace x "-" "")))
  (: make-value (-> String (Listof Index)))
  (define (make-value x)
    `(0 ,@(map (λ([x : String])
                 (if (equal? x "-") 1 0))
               (regexp-split #px"[a-z]" x)) 0))
  (add-pattern-to-cache (cons (make-key exception) (make-value exception))))

;; An exception-word is a string of word characters or hyphens.
(: exception-word? (-> String Boolean))
(define (exception-word? x)
  (if (regexp-match #px"^[\\w-]+$" x) #t #f))

(: exception-words? (-> (Listof String) Boolean))
(define (exception-words? xs)
  (and (list? xs) (andmap exception-word? xs)))

(: string->hashpair (-> String (Pairof String (Listof Index))))
(define (string->hashpair pat)
  (define boundary-name ".")
  ;; first convert the pattern to a list of alternating letters and numbers.
  ;; insert zeroes where there isn't a number in the pattern.
  (: new-pat (Listof (U Index String)))
  (define new-pat
    (let* ([pat : (Listof String)
                (regexp-match* #rx"." pat)] ; convert to list
           [pat : (Listof (U Index String))
                (map (λ([i : String])
                       (or (and (string->number i) (index? i) i)
                           i)) pat)] ; convert numbers
           [pat : (Listof (U Index String))
                   (if (string? (car pat))
                       (cons 0 pat) pat)] ; add zeroes to front where needed
           [pat : (Listof (U Index String))
                   (if (string? (car (reverse pat)))
                       (reverse (cons 0 (reverse pat))) pat)]) ; and back
      ;; bg: not using flatten, made all if-branches in for loop return lists
      (apply append
             (for/list : (Listof (Listof (U String Index)))
                       ([(current i) (in-indexed pat)])
                       (if (= i (sub1 (length pat)))
                           (list current)
                           (let ([next (list-ref pat (add1 i))])
                             ;; insert zeroes where there isn't a number
                             (if (and (or (equal? current boundary-name)
                                          (string? current))
                                      (string? next))
                                 (list current 0)
                                 (list current))))))))
  ;; then slice out the string & numerical parts to be a key / value pair.
  ;; bg: partition doesn't have a negative filter
  (define value (filter index? new-pat))
  (define key (filter string? new-pat))
  (cons (apply string-append key) value))

(: apply-map-max (-> (Listof (Listof Index)) (Listof Index)))
(define (apply-map-max xss)
  (if (or (eq? '() xss) (eq? '() (car xss))) '()
      (cons (max-indexes (for/list ([xs (in-list xss)]) (car xs)))
            (apply-map-max (for/list ([xs (in-list xss)]) (cdr xs))))))

(: make-points (-> String (Listof Index)))
(define (make-points word)
  ;; walk through all the substrings and see if there's a matching pattern.
  ;; if so, pad it out to full length (so we can (apply map max ...) later on)
  (define word-with-dots (format ".~a." (string-downcase word)))
  (: matching-patterns (Listof (Listof Index)))
  (define matching-patterns
    (cond
     [(hash-has-key? pattern-cache word-with-dots)
      (list (hash-ref pattern-cache word-with-dots))]
     [else
      (define word-as-list (string->list word-with-dots))
      (: hd (Listof Index))
      (define hd (make-list (assert-index (add1 (length word-as-list))) 0))
      ;; bg: typed racket can't handle filtering the Void from a (U Void (Listof Index)) list
      (: tl (Listof (Listof Index)))
      (define tl
        (for*/fold : (Listof (Listof Index))
          ([init : (Listof (Listof Index)) '()])
          ([len (in-range (length word-as-list))]
           [index (in-range (- (length word-as-list) len))])
          (define substring (list->string (take (drop word-as-list index) (add1 len))))
          (cond [(hash-has-key? patterns substring)
                 (define value (hash-ref patterns substring))
                 ;; put together head padding + value + tail padding
                 (cons (append ((inst make-list Index) index 0)
                               value
                               (make-list (assert-index (- (add1 (length word-as-list)) (length value) index)) 0))
                       init)]
                [else init])))
      (cons hd tl)]))
  (: max-value-pattern (Listof Index))
  (define max-value-pattern (apply-map-max matching-patterns))
  (add-pattern-to-cache (cons word-with-dots max-value-pattern))
  ;; for point list,
  ;; drop first two elements because they represent hyphenation weight
  ;; before the starting "." and between "." and the first letter.
  ;; drop last element because it represents hyphen after last "."
  ;; after you drop these two, then each number corresponds to
  ;; whether a hyphen goes after that letter.
  (drop-right (drop max-value-pattern 2) 1))


;; Find hyphenation points in a word. This is not quite synonymous with syllables.
(: word->hyphenation-points (->* (String) (Index Index Index) (Listof String)))
(define (word->hyphenation-points word
                                  [min-l : Index default-min-length]
                                  [min-ll : Index default-min-left-length]
                                  [min-rl : Index default-min-right-length])
  (: add-no-hyphen-zone (-> (Listof Index) (Listof Integer)))
  (define (add-no-hyphen-zone points)
    ; points is a list corresponding to the letters of the word.
    ; to create a no-hyphenation zone of length n, zero out the first n-1 points
    ; and the last n points (because the last value in points is always superfluous)
    (: min-left-length Index)
    (define min-left-length
      (min-index min-ll (length points)))
    (define min-right-length
      (min-index min-rl (length points)))
    (define points-with-zeroes-on-left
      (append (make-list (sub1 min-left-length) 0)
              (drop points (sub1 min-left-length))))
    (define points-with-zeroes-on-left-and-right
      (append (drop-right points-with-zeroes-on-left min-right-length)
              (make-list min-right-length 0)))
    points-with-zeroes-on-left-and-right)
  (: make-pieces (-> String (Listof String)))
  (define (make-pieces word)
    (: tmp+word-dissected (Pairof (Listof Char) (Listof (Listof Char))))
    (define tmp+word-dissected
      (for/fold : (Pairof (Listof Char) (Listof (Listof Char)))
        ;; bg: Accumulator is a "temp-list" and a "final-list"
        ;;     The temp-list collects characters until we reach a syllable.
        ;;     At that point, the temp-list is cons'd to the final-list and
        ;;     we initialize a fresh temp-list.
        ([acc   : (Pairof (Listof Char) (Listof (Listof Char)))
                (cons '() '())])
        ([char  : Char
                (in-string word)]
         [point : Integer
                (in-list (add-no-hyphen-zone (make-points word)))])
        (if (even? point)
            (cons (cons char (car acc))
                  (cdr acc))
            (cons '()
                  (cons (reverse (cons char (car acc)))
                        (cdr acc))))))
    ;; bg: sorry about all the reverses, but that's the fold left game
    (: word-dissected (Listof (Listof Char)))
    (define word-dissected
      (reverse
       (cons (reverse (car tmp+word-dissected))
             (cdr tmp+word-dissected))))
    (map list->string word-dissected))
  (if (and min-l (< (string-length word) min-l))
     (list word)
     (make-pieces word)))

;; joiner contract allows char or string; this coerces to string.
(: joiner->string (-> (U Char String) String))
(define (joiner->string joiner)
  (if (char? joiner) (string joiner) joiner))

(: apply-proc (->* ((-> String String) String)
                   ((-> String Boolean))
                   String))
(define (apply-proc proc x [omit-string (λ([x : String]) #f)])
  (let loop ([x x])
    (cond
     [(and (string? x) (not (omit-string x)))
      (proc x)]
     [else x])))

(: hyphenate (->* (String)
                  ((U Char String)
                   #:exceptions (Listof String)
                   #:min-length Index
                   #:min-left-length Index
                   #:min-right-length Index
                   #:omit-word (-> String Boolean)
                   #:omit-string (-> String Boolean))
                  String))
(define (hyphenate x [joiner default-joiner]
                               #:exceptions [extra-exceptions '()]
                               #:min-length [min-length default-min-length]
                               #:min-left-length [min-left-length default-min-left-length]
                               #:min-right-length [min-right-length default-min-right-length]
                               #:omit-word [omit-word? (λ([x : String]) #f)]
                               #:omit-string [omit-string? (λ([x : String]) #f)])
  (initialize-patterns) ; reset everything each time hyphenate is called
  (for ([sym : String extra-exceptions]) (add-exception sym))
  (define joiner-string (joiner->string joiner))
  ;; todo?: connect this regexp pattern to the one used in word? predicate
  (define word-pattern #px"\\w+") ;; more restrictive than exception-word
  (: insert-hyphens (-> String String))
  (define (insert-hyphens text)
    (: lam (-> String String * String))
    (define (lam word . rest)
      ;; bg: what to do about rest?
      (if (not (omit-word? word))
          (string-join (word->hyphenation-points word min-length min-left-length min-right-length) joiner-string)
          word))
    (regexp-replace* word-pattern
                     text
                     lam))
 (apply-proc insert-hyphens x omit-string?))

(: unhyphenate (->* (String)
                    ((U Char String)
                     #:omit-word (-> String Boolean)
                     #:omit-string (-> String Boolean))
                    String))
(define (unhyphenate x [joiner default-joiner]
                     #:omit-word [omit-word? (λ([x : String]) #f)]
                     #:omit-string [omit-string? (λ([x : String]) #f)])
  (define word-pattern (pregexp (format "[\\w~a]+" joiner)))
  (: remove-hyphens (-> String String))
  (define (remove-hyphens text)
    (: lam (-> String String * String))
    (define (lam word . rest)
      (if (not (omit-word? word))
          (string-replace word (joiner->string joiner) "")
          word))
    (regexp-replace* word-pattern text lam))
  (apply-proc remove-hyphens x omit-string?))
