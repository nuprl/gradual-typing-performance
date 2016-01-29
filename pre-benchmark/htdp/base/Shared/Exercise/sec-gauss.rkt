;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sec-gauss) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; An @deftech{SOE} is a @tech{Matrix}
;; @bold{constraint} if its length is @racket[n], each item has length @racket[(+ n 1)]
;; @bold{constraint} an @tech{SOE} is a non-empty list
;; @bold{interpretation} an @tech{SOE} represents a system of linear equations

;; An @deftech{Equation} is [@List-of @tech{Number}]
;; @bold{constraint} an @tech{Equation} contains at least two numbers. 
;; @bold{interpretation} 
;; if @racket[(list #, @math{a_1} ... #, @math{a_n} #, @math{b})] is an @tech{Equation}, 
;; @math{a_1}, ..., @math{a_n} are the left-hand side variable coefficients and 
;; @math{b} is the right-hand side

;; A @deftech{Solution} is [@List-of @tech{Number}]

;; examples: 
(define M 
  (list (list 2 2  3 10)
        (list 2 5 12 31)
        (list 4 1 -2  1)))

(define S '(1 1 2))

(define M1 
  (list (list 2 2  3 10)
        (list 0 3  9 21)
        (list 4 1 -2  1)))

(define bad-M
  (list (list 2 2 2 6)
        (list 2 2 4 8)
        (list 2 2 1 2)))

;; A TM (triangular matrix) is [List-of Equation] 
;;   such that the Equations have decreasing lengths: 
;;   n+1, n, n-1, ..., 2.

(define T 
  (list (list 2 2 3 10)
        (list   3 9 21)
        (list   1 2)))

;; [List-of Number] -> [List-of Number]
;; extract the left-hand side from a equation in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))

;; [List-of Number] -> Number
;; extract the left-hand side from a equation in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

;; -----------------------------------------------------------------------------
;; ex:gauss-data

;; SOE Solution -> Boolean 
;; check whether S is a solution for the given SOE 
;; assert SOE is of length n, S is of length n 

(check-expect (check-solution M S) true)
(check-expect (check-solution M1 S) true)

(define (check-solution M S)
  (andmap (lambda (e) (= (plug-in (lhs e) S) (rhs e))) M))

;; [List-of Number] -> Number
(define (plug-in poly S)
  (foldr + 0 (map * poly S)))

;; -----------------------------------------------------------------------------
;; ex:gauss-subtract

;; Equation Equation -> Equation 
;; subtracte e2 from e1 so that the first coefficient of the result is 0
;; assert (not (= (/ (first e1) (first e2)) 0))

(check-expect (subtract (second M) (first M)) (rest (second M1)))

(define (subtract e1 e2)
  (local ((define factor (/ (first e1) (first e2))))
    (rest (map (lambda (f1 f2) (- f1 (* factor f2))) e1 e2))))

;; -----------------------------------------------------------------------------
;; ex:triangulate1

;; trivial problem: when M contains a single equation 
;; solution for trivial problem: the equation 
;; generate: subtract the first equation from the remaining ones to 
;;   eliminate first coefficient 
;; solve: add first equation to triangulation of generated SOE 

;; SOE -> TM
;; triangulate the system of equations 
;; generative subtract first equation from remaning ones 

(check-expect (triangulate M) T)

(define (triangulate M)
  (cond
    [(empty? (rest M)) M]
    [else
     (local ((define eq1 (first M))
             (define nxt (map (lambda (e) (subtract e eq1)) (rest M))))
       (cons eq1 (triangulate nxt)))]))

;; -----------------------------------------------------------------------------
;; ex:triangulate2

;; SOE -> TM
;; triangulate the system of equations 
;; generative rotate equations until one with non-0 shows up in first position; 
;;   then subtract this equation from remaining ones
;; termination the algorithm may not terminate if the rotator can't find a 
;;   "good" equation 

(check-expect (triangulate.v2 
               (list (list 2  3  3 8)
                     (list 2  3 -2 3)
                     (list 4 -2  2 4)))
              (list (list 2  3  3   8)
                    (list   -8 -4 -12)
                    (list      -5  -5)))

(define (triangulate.v2 M0)
  (cond
    [(empty? (rest M0)) M0]
    [else (local ((define mat (rotate-until-not-0 M0))
                  (define eq1 (first mat))
                  (define nxt (map (lambda (e) (subtract e eq1)) (rest mat))))
            (cons eq1 (triangulate.v2 nxt)))]))

;; SOE -> SOE
;; generative rotate equations until the first coefficient is not 0
;; termination this algorithm does not terminate if all equations start with 0

(check-expect (rotate-until-not-0 
               (list (list   0 -5  -5)
                     (list  -8 -4 -12)))
              (list (list  -8 -4 -12)
                    (list   0 -5  -5)))

(define (rotate-until-not-0 M)
  (local ((define e1 (first M)))
    (cond
      [(not (= (first e1) 0)) M]
      [else (rotate-until-not-0 (append (rest M) (list e1)))])))

;; NOTE this step is more expensive functionally than imperatively
;; I don't see how to make it less expensive with accumulators 

;; -----------------------------------------------------------------------------
;; ex:triangulate3

;; trivial problem 1: when M contains a single equation 
;; solution for trivial problem 1: the equation 
;; trivial problem 2: when M contains only leading coefficients of 0
;; solution for trivial problem 2: an error message
;; generate: subtract the first equation from the remaining ones to 
;;   eliminate first coefficient 
;; solve: add first equation to triangulation of generated SOE 

;; SOE -> TM
;; triangulate the system of equations 
;; generative rotate equations until one with non-0 shows up in first position; 
;;   then subtract this equation from remaining ones
;; termination the algorithm may terminate with an error if M0 is not solvable

(check-expect (triangulate.v3
               (list (list 2  3  3 8)
                     (list 2  3 -2 3)
                     (list 4 -2  2 4)))
              (list (list 2  3  3   8)
                    (list   -8 -4 -12)
                    (list      -5  -5)))

(check-error (triangulate.v3 bad-M) "triangulate failed")

(define (triangulate.v3 M0)
  (cond
    [(empty? (rest M0)) M0]
    [(all-0? M0) (error "triangulate failed")]
    [else (local ((define mat (rotate-until-not-0 M0))
                  (define eq1 (first mat))
                  (define nxt (map (lambda (e) (subtract e eq1)) (rest mat))))
            (cons eq1 (triangulate.v3 nxt)))]))

;; SOE -> Boolean 
;; determine whether all leading coefficients are 0
(define (all-0? M)
  (andmap (lambda (e) (zero? (first e))) M))

;; -----------------------------------------------------------------------------
;; ex:gauss-solve

;; TM -> Solution
;; solve a triangular system of equations

(check-expect (solve T) S)

(define (solve t)
  (foldr (lambda (e sol) (cons (solve-one e sol) sol)) '() t))

;; Equation Solution -> Number 
;; solve one equation, given the partial solution for all but the first variable 

(check-expect (solve-one '(1 2) '()) 2)
(check-expect (solve-one '(3 9 21) '(2)) 1)

(define (solve-one e s)
  (local ((define left (lhs e)))
    (/ (- (rhs e) (plug-in (rest left) s)) 
       (first left))))

;; -----------------------------------------------------------------------------
;; ex:gauss-complete

;; SOE -> Solution 
;; solve a system of equations 

(check-expect (gauss M) S)
(check-expect (check-solution M (gauss M)) true)
(check-error (gauss bad-M) "triangulate failed")

(define (gauss m)
  (solve (triangulate.v3 m)))


