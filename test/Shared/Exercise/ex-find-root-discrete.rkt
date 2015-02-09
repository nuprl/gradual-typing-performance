;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-find-root-discrete) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define-struct table (length array))
;; A @deftech{Table} is a 
;;   @racket[(make-table #, @tech{N} [#, @tech{N} -> #, @tech{Number}])]

;; @tech{Table} @tech{N} -> @tech{Number}
;; look up the @racket[i]th value in @racket[array] of @racket[t]
(define (table-ref t i)
  ((table-array t) i))

(define table1 (make-table 3 (lambda (i) i)))

;; N -> Number 
(define (a2 i)
  (if (= i 0) pi (error "table2 is not defined for i =!= 0")))
  
(define table2 (make-table 1 a2))

(define table=0 (make-table 3 (lambda (i) 0)))
(define table3 (make-table 3 (lambda (i) (cond [(= i 0) -1][(= 1 i) 1][(= 2 i) 3]))))
(define table4 (make-table 3 (lambda (i) (cond [(= i 0) -2][(= 1 i) -1][(= 2 i) .1]))))
(define table5 (make-table 4 (lambda (i) (cond [(= i 0) -2][(= 1 i) -1][(= 2 i) -.1][(= 3 i) .01]))))
(define table6 (make-table 3 (lambda (i) (cond [(= i 0) -.1][(= 1 i) -.1][(= 2 i) .2]))))

;; Table -> N
;; determine the left-most root index for t

(check-expect (find-root-linear table1) 0)
(check-expect (find-root-linear table2) 0)
(check-expect (find-root-linear table=0) 0)
(check-expect (find-root-linear table3) 0)
(check-expect (find-root-linear table4) 2)
(check-expect (find-root-linear table5) 3)
(check-expect (find-root-linear table6) 0)

(define (find-root-linear t)
  (local (;; N -> N 
          ;; find root of f in [0,n)
          (define (find n)
            (cond
              [(= (+ n 1) (table-length t)) n]
              [else (if (<= (abs (table-ref t n)) (abs (table-ref t (+ n 1))))
                        n
                        (find (+ n 1)))])))
    (find 0)))
                        
(define (find-root-binary t)
  (local ((define f (table-array t))
          ;; N N -> N 
          ;; find root in [left,right]
          ;; generative: divid interval in half, choose proper interval to recur
          ;; assume: (<= (f left) 0 (f right))
          (define (find left right)
            (cond
              [(= (- right left) 0) left]
              [else (local ((define mid (quotient (+ left right) 2)))
                      (if (<= (abs (table-ref t mid)) (abs (table-ref t (+ mid 1))))
                          (find left mid)
                          (find (+ mid 1) right)))])))
    (find 0 (- (table-length t) 1))))

(check-expect (find-root-binary table1) 0)
(check-expect (find-root-binary table2) 0)
(check-expect (find-root-binary table=0) 0)
(check-expect (find-root-binary table3) 0)
(check-expect (find-root-binary table4) 2)
(check-expect (find-root-binary table5) 3)
(check-expect (find-root-binary table6) 0)
