#lang racket/base

(provide
  integer->word*
  integer->word
  rnd
)

(require
  racket/match
  racket/format
)

;; =============================================================================

(define SCALE '#(END "thousand" "million" "billion" "trillion"))

;; Serves as a map from small naturals to their string representations
;; (: N<20 (Vectorof String))
(define N<20
  '#("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
     "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen"
     "eighteen" "nineteen"))

;; (: TENS>10 (Vectorof String))
(define TENS>10
  '#("twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))

;; Convert a two-digit number to a list of lowercase words
(define (digit2->word* n)
  (cond
   [(< n 20)
    (list (vector-ref N<20 n))]
   [else
    (define q (quotient n 10))
    (define r (modulo n 10))
    (define ten-str (vector-ref TENS>10 (- q 2)))
    (define one-str (and (not (zero? r)) (vector-ref N<20 r)))
    (if one-str
        (list ten-str one-str)
        (list ten-str))]))

;; Split a natural number into 3-digit chunks
(define (natural->natural* N)
  (let loop ([acc '()]
             [n N]  ;; Starts as original & we remove 3 digits each step.
             [i 0]) ;; Index used to pick a scale
    (define q (quotient n 1000))
    (define r (modulo n 1000))
    (cond
     [(= n r)
      ;; Reached fixpoint, stop iteration
      (cons r acc)]
     [else
      ;; Repeat using the quotient
      (loop (cons r acc) q (add1 i))])))

;; Break a natural into chunks, attach a scale to each chunk
(define (natural->scaled* n)
  (define (add-scale n acc+i)
    (match-define (cons acc i) acc+i)
    (define s (vector-ref SCALE i))
    (define n+s (cons n s))
    (cons (cons n+s acc) (add1 i)))
  (car (foldr add-scale (cons '() 0) (natural->natural* n))))

(define (integer->word* N)
  ;; Break N into chunks, convert each chunk+scale to a string
  (define str*
    (for/list ([n+s (in-list (natural->scaled* (abs N)))])
      (match-define (cons n s) n+s)
      (define q (quotient n 100))
      (define r (modulo n 100))
      (define n-str*
        (cond
         [(zero? n)
          '()]
         [(< n 100)
          (digit2->word* r)]
         [else
          (define hd (vector-ref N<20 q))
          (define tl (let ([tmp (digit2->word* r)])
                       (if (equal? tmp '("zero")) '() tmp)))
          (list* hd "hundred" tl)]))
      ;; Don't print a scale for zeros or the last chunk
      (if (or (eq? s 'END) (zero? n))
          n-str*
          (append n-str* (list s)))))
  (cond ;; Check for special cases
   [(zero? N)
    (list "zero")]
   [(negative? N)
    (cons "negative" (apply append str*))]
   [else
    (apply append str*)]))

(define (integer->word N)
  (match (integer->word* N)
   [(cons w '())
    w]
   [w*
    (raise-user-error 'integer->word "Integer ~a produced multiple words ~a" N w*)]))

(define (rnd n)
  (~r n #:precision 2))
