#lang racket/base

(require
  "summary.rkt"
  (only-in racket/stream in-stream stream->list)
  (only-in racket/format ~r)
  racket/set)

(define NAMES '(
  "synth"
  "gregor"
  "snake"
  "suffixtree"
  "tetris"
))

(define ORIG '(
  "../data/synth-2015-07-02T01:47:43.rktd"
  "../data/gregor-2015-07-26T05:27:18.rktd"
  "../data/snake-2015-06-30T14:55:40.rktd"
  "../data/suffixtree-large-06-30.rktd"
  "../data/tetris-2015-07-01T16:39:46.rktd"
))

(define NODATA '(
  "../../unsafe/data/usynth.rktd"
  "../../unsafe/data/ugregor.rktd"
  "../../unsafe/data/usnake.rktd"
  "../../unsafe/data/usuffixtree.rktd"
  "../../unsafe/data/utetris.rktd"
))

;; Select `n` random variations from the summary object
(define (random-variations S n)
  (define N (get-num-variations S))
  (define i* (random-sample N n))
  (for/list ([v (in-stream (all-variations S))]
             [j (in-naturals)]
             #:when (set-member? i* j))
    v))

(define (random-sample limit count)
  (let loop ([seen (set)])
    (if (= count (set-count seen))
      seen
      (loop (set-add seen (random limit))))))

(define (rnd n)
  (~r n #:min-width 8 #:pad-string " " #:precision (list '= 2)))

(define (main)
  (for ([pn (in-list NAMES)]
        [o-rktd (in-list ORIG)]
        [n-rktd (in-list NODATA)])
    (define O (from-rktd o-rktd))
    (define N (from-rktd n-rktd))
    (printf "### ~a:\n" pn)
    (printf "NO DATA                 ORIGINAL RUNTIME\n")
    (for ([v (random-variations N 10)])
      ;; The "nodata" variations are a subset of the "orig"
      ;; so pick random things from there
      (define msg1 (format "~a : ~a" v (rnd (variation->mean-runtime N v))))
      (define t-vars (typed-modules N v))
      (define u-vars (untyped-modules N v))
      (define (typed/untyped-matches? v)
        (and (has-typed? O v t-vars)
             (has-untyped? O v u-vars)))
      (define v* (stream->list (predicate->variations O typed/untyped-matches?)))
      (define m* (for/list ([v (in-list v*)]) (rnd (variation->mean-runtime O v))))
      (printf "~a        ~a : ~a\n" msg1 (car v*) (car m*))
      (define spacer (make-string (string-length msg1) #\space))
      (for ([v (in-list (cdr v*))] [m (in-list (cdr m*))])
        (printf "~a        ~a : ~a\n" spacer v m)))))

;; =============================================================================

(module+ main
  (main))

;; =============================================================================

(module+ test
  (require rackunit)
  (define-syntax-rule (check-random-sample [lim count] ...)
    (begin
      (let ([rs (random-sample lim count)])
        (check-equal? (set-count rs) count)
        (check-true (for/and ([r (in-set rs)]) (and (<= 0 r) (< r lim)))))
        ...))
  (check-random-sample
    [1 1]
    [2 1]
    [2 1]
    [10 3]
    [5 2]
  )

  (define-syntax-rule (check-random-variations [rktd count] ...)
    (begin
      (let* ([S (from-rktd rktd)]
             [v* (random-variations S count)])
        (check-equal? (length v*) count)
        (check-equal? (set-count (list->set v*)) count)
        (check-true (for/and ([v (in-list v*)])
                      (and (string? v) (variation->mean-runtime S v) #t))))
      ...))
  (check-random-variations
    ["../data/suffixtree-large-06-30.rktd" 3])
)
