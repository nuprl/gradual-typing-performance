#lang racket/base

;; Provides functions for drawing lattices that show the perf data
;; for each configuration

(require racket/contract
         racket/format
         racket/list
         data/bit-vector
         (except-in math/number-theory permutations)
         pict
         unstable/gui/pict)

(provide (contract-out
          [make-performance-lattice
           (-> (and/c (vectorof (cons/c number? number?))
                      power-of-two-length?)
               pict?)]))

(module+ test (require rackunit))

(define (power-of-two-length? vec)
  (define factors (factorize (vector-length vec)))
  (and (= 1 (length factors))
       (= (car (car factors)) 2)))

(define (two-expt n) (cadar (factorize n)))

(module+ test
  (check-true (power-of-two-length? (vector 1 2 3 4)))
  (check-false (power-of-two-length? (vector 1 2 3))))

(define (make-performance-lattice data-vec)
  (define total-bits (two-expt (vector-length data-vec)))
  (define pict-vec (make-vector (vector-length data-vec) #f))
  (define level-picts
    (for/list ([on-bits (in-range total-bits -1 -1)])
      (define perms (select (- total-bits on-bits) total-bits))
      (apply hc-append 5
       (for/list ([perm (in-list perms)])
         (define bv (apply bit-vector perm))
         (define num (string->number (bit-vector->string bv) 2))
         (define pict (make-point bv
                                  (vector-ref data-vec num)
                                  (vector-ref data-vec 0)))
         (vector-set! pict-vec num pict)
         pict))))
  (define no-lines-yet (apply vc-append 10 level-picts))
  no-lines-yet)

;; taken from MF's version
(define (select i L)
  (cond
    [(zero? i)  (list (build-list L (lambda (_) #t)))]
    [(zero? L) '()]
    [else (append (map (lambda (r) (cons #f r)) (select (sub1 i) (sub1 L)))
                  (map (lambda (r) (cons #t r)) (select i (sub1 L))))]))

;; constructs a pict for a point in the lattice, using the initial
;; configuration to normalize (for coloring)
(define (make-point bv data init-data)
  (define normalized-mean (/ (car data) (car init-data)))
  (define normalized-stdev (/ (cdr data) (car init-data)))
  (define style "Liberation Serif")
  (define box-pict
    (apply hc-append
           1
           (for/list ([bit (in-bit-vector bv)])
             (ellipse/border 3 8
                             #:border-width 1
                             #:color (if bit "black" "white")
                             #:border-color "black"))))
  (vc-append (blank 1 2)
             box-pict
             (blank 1 4)
             (text (~a (~r normalized-mean #:precision 1) "x") style 9)))

;; adds lines between elements in levels
(define (add-all-lines base vec bits)
  (for/fold ([pict base])
            ([(from-pict idx) (in-indexed (in-vector vec))])
    (define masks
      (for/list ([bools (in-list (select (- bits 1) bits))])
        (string->number (apply string-append (map (λ (x) (if x "1" "0")) bools))
                        2)))
    (define targets
      (remove* (list idx)
               (map (λ (x) (bitwise-ior x idx)) masks)))
    (for/fold ([pict pict])
              ([target-idx targets])
      (define target (vector-ref vec target-idx))
      (pin-arrow-line 2 pict from-pict ct-find target cb-find
                      #:line-width 0.5
                      #:under? #t))))

;; Driver submodule for rapid visualization
(module+ main
  (require racket/file
           racket/gui/base
           racket/match
           racket/vector
           math/statistics)
  (match (current-command-line-arguments)
    [(vector path)
     (define data (file->value path))
     (define averaged-results
       (vector-map (λ (times) (cons (mean times) (stddev times))) data))
     (show-pict (make-performance-lattice averaged-results))]))
