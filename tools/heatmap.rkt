#lang racket/base

;; Construct a "heatmap" from the lattice data

(require racket/class
         racket/cmdline
         racket/draw
         racket/match
         racket/math
         racket/vector
         data/bit-vector
         math/number-theory
         math/statistics
         pict
         unstable/gui/pict)

;; some of this code stolen from lattice code
(define (power-of-two-length? vec)
  (define factors (factorize (vector-length vec)))
  (and (= 1 (length factors))
       (= (car (car factors)) 2)))

(define (two-expt n) (cadar (factorize n)))

;; taken from MF's version
(define (select i L)
  (cond
    [(zero? i)  (list (build-list L (lambda (_) #t)))]
    [(zero? L) '()]
    [else (append (map (lambda (r) (cons #f r)) (select (sub1 i) (sub1 L)))
                  (map (lambda (r) (cons #t r)) (select i (sub1 L))))]))

(define *width* 400)
(define *height* 10)

(define (slowdown->color slowdown)
  ;; tried a reciprocal scale here, looked too "smushed" at high end
  ;(define proportion (/ (- 1 (/ 1 slowdown)) 1.5))
  (define proportion (/ (log slowdown) 3))
  (define intensity
    ;; we can't plot speedups with this, so cap it
    ;; FIXME: that is misleading, so we should think about how to do that
    ;; FIXME: proportions are sometimes bigger than 1 due to log, is the
    ;;        reciprocal scale better?
    (max 0 (min 255 (- 255 (exact-round (* 255 proportion))))))
  (make-color intensity intensity intensity))

(define (make-point data init-data num-in-row the-min the-max)
  ;; use reciprocal scale to do the heat intensity of a given point
  (define slowdown (/ (car data) (car init-data)))
  (define color (slowdown->color slowdown))
  (colorize (filled-rectangle (/ *width* num-in-row) *height*)
            color))

(define scale-pict
  (apply hc-append 15
         (for/list ([x (in-range 1 7 0.5)])
           (vc-append 3
                      (rectangle/border 10 10
                                        #:border-color "black"
                                        #:color (slowdown->color x))
                      (text (format "~ax" x))))))

(module+ main
  (match-define (list data-path output-path)
    (command-line #:program "heatmap"
                  #:args (dp op)
                  (list dp op)))

  (define input (open-input-file data-path))
  (define results (read input))

  (unless (and (vector? results)
               (power-of-two-length? results))
    (raise-user-error 'heatmap "input data in the wrong format"))

  (define averaged-results
    (vector-map (λ (times)
                  ;; allow old style data too
                  (if (list? times)
                      (cons (mean times) (stddev times))
                      times))
                results))

  (define min (car (vector-argmin car averaged-results)))
  (define max (car (vector-argmax car averaged-results)))

  (define total-bits (two-expt (vector-length averaged-results)))
  (define pict-vec (make-vector (vector-length averaged-results) #f))
  (define level-picts
    (for/list ([on-bits (in-range total-bits -1 -1)])
      (define perms (select (- total-bits on-bits) total-bits))
      (apply hc-append 0
       (for/list ([perm (in-list perms)])
         (define bv (apply bit-vector perm))
         (define num (string->number (bit-vector->string bv) 2))
         (define pict (make-point (vector-ref averaged-results num)
                                  (vector-ref averaged-results 0)
                                  (length perms)
                                  min max))
         (vector-set! pict-vec num pict)
         pict))))
  (define diagram (vc-append 10
                             (frame (apply vc-append 0 level-picts))
                             (text (format "~a" data-path))
                             scale-pict))
  (define pict (cc-superimpose diagram
                               (blank (+ (pict-width diagram) 20)
                                      (+ (pict-height diagram) 20))))

  (send (pict->bitmap pict)
        save-file
        output-path
        'png))
