#lang racket 

(module+ test
  (require test-engine/racket-tests))

(define M
  '((0 4 5)
    (1 2 3)))

(define M1
  '((1 2 3)
    (0 4 5)))


;; Matrix -> Matrix 
;; construct a matrix whose first row starts with a 

(module+ test
  (check-expect (rotate-until.v1 M) M1))

(define (rotate-until.v1 l0)
  (local (;; Matrix Row -> Matrix 
          ;; accumulator all rows between l0 and l, in reverse order 
          (define (rotate-until l seen)
            (cond
              [(empty? l) (error "rotate-until can't find a proper row")]
              [else 
               (if (= (first (first l)) 0)
                   (rotate-until (rest l) (cons (first l) seen))
                   (cons (first l) (append seen (rest l))))])))
    (rotate-until l0 '())))

;; Matrix -> Matrix 
;; generative find the first row that doesn't start with 0 and use it as the first one

(module+ test
  (check-expect (rotate-until.v2 M) M1))

(define (rotate-until.v2 l)
  (cond
    [(not (= (first (first l)) 0)) l]
    [else (rotate-until.v2 (append (rest l) (list (first l))))]))

(module+ test
  (test))

;; -----------------------------------------------------------------------------

(module+ main 
  
  (define (run N)
    (local ((define large 
              (append
               (build-list N (lambda (i) `(0 ,(+ (random (+ i 1)) 1) 0 0))) 
               '((1 2 3 0)))))
      (define-values (_1 cpu1 real1 gc1) (time-apply rotate-until.v1 (list large)))
      (define-values (_2 cpu2 real2 gc2) (time-apply rotate-until.v2 (list large)))
      (list (list cpu1 real1 gc1) (list cpu2 real2 gc2)) ))
  
  (define (timing n)
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (run n))
  
  (define (all-times)
    (define N 1000)
    (define matrix
      (for/list ((i (in-range N (* 9 N) N)))
        (displayln i)
        (cons i (timing i))))
    (values (map first matrix) (map second matrix) (map third matrix)))
  
  (all-times))



; (display (with-output-to-string (lambda () (time-it 10)))