#lang racket

(require gtp-summarize/summary
         glob)

(define path (first (glob "../data/6.3/gregor*")))
(define data (from-rktd path))
(define num-mods (get-num-modules data))

(define untyped-conf (make-string num-mods #\0))
(define (string-set str k char)
  (define str2 (string-copy str))
  (string-set! str2 k char)
  str2)
(define (1-typed-conf k)
  (string-set untyped-conf k #\1))
(define (2-typed-conf k1 k2)
  (define s (1-typed-conf k1))
  (string-set! s k2 #\1)
  s)
(define (3-typed-conf k1 k2 k3)
  (define s (2-typed-conf k1 k2))
  (string-set! s k3 #\1)
  s)

(define (perf-at bs)
  (configuration->mean-runtime data bs))

(define base-perf (perf-at untyped-conf))

;; Π(mod i typed) = Π(none typed) + Δ(i typed)
;; Δ(i typed) = Π(mod i typed) - Π(none typed)
(define single-mod-costs
  (for/hash ([i (in-range num-mods)])
    (values i
            ((perf-at (1-typed-conf i)) . - . base-perf))))

;; Π(i and j) = Δ(i) + Δ(j) + Δ(i # j) + Π(untyped)
;; Δ(i # j) = Π(i and j) - (Δ(i) + Δ(j) + Π(untyped))
(define single-interaction-costs
  (for*/hash ([i (in-range num-mods)]
              [j (in-range num-mods)]
              #:when (< i j))
    (values (cons i j)
            ((perf-at (2-typed-conf i j))
             . - .
             (+ base-perf
                (hash-ref single-mod-costs i)
                (hash-ref single-mod-costs j))))))

(define (bs->typed-indices bs)
  (for/list ([i (in-naturals)]
             [b (in-string bs)]
             #:when (equal? b #\1))
    i))

(define (predict bs)
  (define typeds (bs->typed-indices bs))
  (+ base-perf
     (for/sum ([i (in-list typeds)])
       (hash-ref single-mod-costs i))
     (for*/sum ([i (in-list typeds)]
                [j (in-list typeds)]
                #:when (< i j))
       (hash-ref single-interaction-costs (cons i j)))))

(define (test bs)
  (define prediction (predict bs))
  (define actual (perf-at bs))
  (define difference (prediction . - . actual))
  (define the-error (difference . / . actual))
  `(prediction ,(* 1.0 prediction)
    actual     ,(* 1.0 actual)
    difference ,(* 1.0 difference)
    error      ,(* 1.0 the-error)))

(for*/hash ([i (in-range num-mods)]
            [j (in-range i)]
            [k (in-range j)])
  (values
   (3-typed-conf i j k)
   (test (3-typed-conf i j k))))
