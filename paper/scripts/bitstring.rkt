#lang racket/base

(provide
  log2
  natural->bitstring
  bitstring->natural
  in-reach
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/math exact-ceiling)
  (only-in racket/format ~r)
  (only-in racket/list remove-duplicates)
)

;; =============================================================================

;; log, base 2
;; (: log2 (-> Integer Flonum))
(define (log2 n)
  (exact-ceiling (/ (log n) (log 2))))

;; Convert a natural number to a binary string, padded to the supplied width
;; (: natural->bitstring (-> Index #:pad Index String))
(define (natural->bitstring n #:pad pad-width)
  (and (exact-positive-integer? pad-width)
       (~r n #:base 2 #:min-width pad-width #:pad-string "0")))

;; Convert a binary string to a natural number
;; (: bitstring->natural (-> String Index))
(define (bitstring->natural str)
  (define N (string-length str))
  (for/sum ([i (in-range N)])
    (define c (string-ref str (- N (add1 i))))
    (if (equal? #\1 c)
        (expt 2 i)
        0)))

;; Return a copy of `str` where the `i`-th bit is flipped.
;; (Flipped => 0 goes to 1 and 1 goes to 0)
;; (: bitstring-flip (-> String Index String))
(define (bitstring-flip str i)
  (define new (if (equal? #\0 (string-ref str i)) "1" "0"))
  (string-append (substring str 0 i)
                 new
                 (substring str (add1 i) (string-length str))))

;; Return all bitstrings reachable from `str`
;;  after incrementing at most `L` bits.
;; Result does NOT include the argument bitstring.
;; (: in-reach (-> String Index (Listof String)))
(define (in-reach str L)
  (cond [(zero? L) '()]
        [else
         (define res*
           (for/list ([i (in-range (string-length str))]
                      #:when (equal? #\0 (string-ref str i)))
             (define str+ (bitstring-flip str i))
             (cons str+ (in-reach str+ (sub1 L)))))
         (remove-duplicates (apply append res*) string=?)]))

;; =============================================================================

(module+ test
  (require rackunit)

  ;; -- log2
  (check-equal? (log2 2) 1)
  (check-equal? (log2 32) 5)
  (check-equal? (log2 1024) 10)

  ;; -- natural->bitstring
  (check-equal? (natural->bitstring 2 #:pad 0) #f)
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

  ;; -- bitstring-flip
  (check-equal? (bitstring-flip "0" 0) "1")
  (check-equal? (bitstring-flip "1" 0) "0")
  (check-equal? (bitstring-flip "0010" 2) "0000")
  (check-equal? (bitstring-flip "11011" 4) "11010")
  (check-equal? (bitstring-flip "000" 0) "100")
)
