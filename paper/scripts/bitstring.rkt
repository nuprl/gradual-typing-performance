#lang typed/racket

;; A bitstring is a string of #\1 and #\0.
;; Technically bitstrings of unequal length are incompatible,
;;  but our scripts do nothing to enforce this.
(define-type Bitstring String)
(provide
  Bitstring

  all-paths-from
  ;; (-> Bitstring (Sequenceof (Listof Bitstring)))

  log2
  ;; (-> Natural Natural)
  ;; Base 2 log, on naturals

  natural->bitstring
  ;; (-> Index #:pad Index Bitstring)
  ;; Convert a natural number to a binary string representation
  ;; Keyword argument #:pad sets the width of the result string

  bitstring->natural
  ;; (-> Bitstring Index)
  ;; Convert a string representation of a binary number to a natural.

  in-reach
  ;; (-> Bitstring Index (Listof Bitstring))
  ;; (in-reach s l)
  ;; List all bitstrings reachable from `s` by flipping at most `l` bits

  bit-high?
  ;; (-> Bitstring Index Boolean)
  ;; True if the bit at the index is set

  bit-low?
  ;; (-> Bitstring Index Boolean)
  ;; True if the bit at the index is unset
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/math exact-ceiling)
  (only-in racket/format ~r)
  (only-in racket/list remove-duplicates)
  (only-in math/number-theory factorial)
  (only-in racket/sequence sequence-map)
)

;; =============================================================================

(: log2 (-> Natural Natural))
(define (log2 n)
  (: log2-help (-> Natural Natural Natural))
  (define (log2-help pow acc)
    (if (= acc n) pow (log2-help (add1 pow) (* acc 2))))
  (log2-help 1 2))

;; Convert a natural number to a binary string, padded to the supplied width
(: natural->bitstring (-> Index #:pad Exact-Positive-Integer Bitstring))
(define (natural->bitstring n #:pad pad-width)
  (~r n #:base 2 #:min-width pad-width #:pad-string "0"))

;; Convert a binary string to a natural number
(: bitstring->natural (-> Bitstring Index))
(define (bitstring->natural str)
  (define N (string-length str))
  (define res
    (for/sum : Integer ([i (in-range N)])
      (define c (string-ref str (- N (add1 i))))
      (if (equal? #\1 c)
          (exact-ceiling (expt 2 i))
          0)))
  (if (index? res) res (error 'bitstring->natural)))

;; Return a copy of `str` where the `i`-th bit is flipped.
;; (Flipped => 0 goes to 1 and 1 goes to 0)
;; Should take an INDEX, but is Integer for now
(: bitstring-flip (-> Bitstring Integer Bitstring))
(define (bitstring-flip str i)
  (define new (if (equal? #\0 (string-ref str i)) "1" "0"))
  (string-append (substring str 0 i)
                 new
                 (substring str (add1 i) (string-length str))))

;; Return all bitstrings reachable from `str`
;;  after incrementing at most `L` bits.
;; Result does NOT include the argument bitstring.
(: in-reach (-> Bitstring Index (Listof Bitstring)))
(define (in-reach str L)
  (cond [(zero? L) '()]
        [else
         (define res* : (Listof (Listof Bitstring))
           (for/list ([i (in-range (string-length str))]
                      #:when (equal? #\0 (string-ref str i)))
             (define str+ (bitstring-flip str i))
             (cons str+ (in-reach str+ (sub1 L)))))
         (remove-duplicates (apply append res*) string=?)]))

;; Goal: enumerate all lists of N strings (where N = length of argument)
;;  such that the first string in each list is all zeroes and subsequent
;;  elements have one 1 in a position where the previous had a 0.
(: all-paths-from (-> Bitstring (Sequenceof (Listof Bitstring))))
(define (all-paths-from str)
  (sequence-map
    permutation->path
    (in-permutations (range (string-length str)))))

(: permutation->path (-> (Listof Natural) (Listof Bitstring)))
(define (permutation->path index*)
  (define L (length index*))
  ;; Create the path in reverse order
  (for/fold ([acc (list (bitstring-init L #:hi? #t))])
            ([i (in-list index*)])
    (cons (bitstring-flip (car acc) i) acc)))

(: bitstring-init (->* [Natural] [#:hi? Boolean] Bitstring))
(define (bitstring-init n #:hi? [hi? #f])
  (make-string n (if hi? #\1 #\0)))

(: all-high? (-> Bitstring Boolean))
(define (all-high? str)
  (for/and ([i : Natural (in-range (string-length str))])
    (bit-high? str i)))

(: all-low? (-> Bitstring Boolean))
(define (all-low? str)
  (for/and ([i : Natural (in-range (string-length str))])
    (bit-low? str i)))

(: bit-high? (-> Bitstring Natural Boolean))
(define (bit-high? str i)
  (high? (string-ref str i)))

(: bit-low? (-> Bitstring Natural Boolean))
(define (bit-low? str i)
  (low? (string-ref str i)))

(: high? (-> Char Boolean))
(define (high? c)
  (eq? #\1 c))

(: low? (-> Char Boolean))
(define (low? c)
  (eq? #\0 c))

;; =============================================================================

(module+ test
  (require typed/rackunit)

  ;; -- log2
  (check-equal? (log2 2) 1)
  (check-equal? (log2 32) 5)
  (check-equal? (log2 1024) 10)

  ;; -- natural->bitstring
  (check-equal? (natural->bitstring 2 #:pad 2) "10")
  (check-equal? (natural->bitstring 2 #:pad 10) "0000000010")

  ;; -- bitstring->natural
  (check-equal? (bitstring->natural "10") 2)
  (check-equal? (bitstring->natural "00010") 2)

  ;; -- in-reach
  (define-syntax-rule (in-reach-test bitstring pad result)
    (check-equal? (sort (in-reach bitstring pad) string>?)
                  result))
  (in-reach-test "0" 0 '())
  (in-reach-test "111111" 0 '())
  (in-reach-test "10101" 0 '())
  (in-reach-test "0" 1 '("1"))
  (in-reach-test "111" 1 '())
  (in-reach-test "10101" 1 '("11101" "10111"))
  (in-reach-test "0001" 1 '("1001" "0101" "0011"))
  (in-reach-test "0" 3 '("1"))
  (in-reach-test "110" 3 '("111"))
  (in-reach-test "10010" 3 '("11111" "11110" "11011" "11010" "10111" "10110" "10011"))

  ;; -- all-paths-from
  (define-syntax-rule (check-all-paths [in out] ...)
    (begin (check-equal?
             (for/list : (Listof (Listof Bitstring))
                       ([p (all-paths-from in)])
               p)
             out) ...))
  (check-all-paths
   ;["" '(())]
   ["0" '(("0" "1"))]
   ;["1" '(("1"))]
   ["00" '(("00" "10" "11")
           ("00" "01" "11"))]
   ["000" '(("000" "100" "110" "111")
            ("000" "100" "101" "111")
            ("000" "010" "110" "111")
            ("000" "010" "011" "111")
            ("000" "001" "101" "111")
            ("000" "001" "011" "111"))])

  ;; If this doesn't return immediately, we have a bug
  (let* ([all_10 (all-paths-from (make-string 20 #\0))]
         [first_path (car (for/list : (Listof (Listof Bitstring))
                                    ([a all_10] [_i (in-range 1)]) a))])
    (check-equal? (length first_path) 21))

  ;; -- bitstring-flip
  (check-equal? (bitstring-flip "0" 0) "1")
  (check-equal? (bitstring-flip "1" 0) "0")
  (check-equal? (bitstring-flip "0010" 2) "0000")
  (check-equal? (bitstring-flip "11011" 4) "11010")
  (check-equal? (bitstring-flip "000" 0) "100")

  ;; -- all-high? / low
  (check-true (all-high? "1111"))
  (check-true (all-high? "1"))
  (check-false (all-high? "0"))
  (check-false (all-high? "00010"))

  (check-false (all-low? "1101"))
  (check-false (all-low? "1"))
  (check-true  (all-low? "0"))
  (check-true  (all-low? "00000"))

  ;; -- bit-high? bit-low?
  (check-true (bit-high? "1111" 0))
  (check-true (bit-high? "1111" 1))
  (check-true (bit-high? "1111" 2))
  (check-true (bit-high? "1111" 3))
  (check-true (bit-high? "1001" 0))
  (check-true (bit-high? "1001" 3))
  (check-false (bit-high? "1001" 1))
  (check-false (bit-high? "1001" 2))
  (check-false (bit-high? "00" 0))
  (check-false (bit-high? "00" 1))

  (check-false (bit-low? "1111" 0))
  (check-false (bit-low? "1111" 1))
  (check-false (bit-low? "1111" 2))
  (check-false (bit-low? "1111" 3))
  (check-false (bit-low? "1001" 0))
  (check-false (bit-low? "1001" 3))
  (check-true (bit-low? "1001" 1))
  (check-true (bit-low? "1001" 2))
  (check-true (bit-low? "00" 0))
  (check-true (bit-low? "00" 1))

)
