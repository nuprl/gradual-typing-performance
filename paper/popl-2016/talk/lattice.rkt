#lang racket/gui

(require data/bit-vector
         math/statistics
         slideshow
         unstable/gui/pict)

(provide *label-size*
	 *show-ms*
	 *use-freeze?*
	 *bleach?*
	 modules->diagram
         suffixtree-diagram
         make-suffixtree-diagram
         lattice-edges?
         lattice-numbers?)

(define lattice-edges? (make-parameter #t))
(define lattice-numbers? (make-parameter #t))

(define SCALE .33)

;; these modules remain untyped and are lumped into one block
(define infra   (tt "libraries"))

(define *show-ms* (make-parameter #t))
(define *label-size* (make-parameter 50))
(define *use-freeze?* (make-parameter #f))
(define *bleach?* (make-parameter #f))

;; ---------------------------------------------------------------------------------------------------
;; the program

(define distance-between-configs 100)
(define distance-between-rows 740)
(define width 0)
(define height 0)

;; [List-of String] [Vector-of [List-of [Pair-of Int Int]]] -> Pict
;; FIXME: enable data again
(define (modules->diagram all-labels data
                          #:threshold [threshold +inf.0]
                          #:draw-node [draw-node pict-of-configuration]
                          #:filter [filter (λ (x) #t)]
			  #:hilite [hilite (λ (x) #f)])
  ;; determine uniform width and height of modules
  ;; (these texts are generated twice, oh well)
  (define all-modules (map label-of-module all-labels))
  (set! width (+ 6 (apply max (map pict-width all-modules))))
  (set! height (+ 6 (apply max (map pict-height all-modules))))
  ;; run
  (define lattice-as-matrix (generate-lattice draw-node filter hilite all-labels data
                                              threshold))
  (define rows-of-picts (map (lambda (row) (map first row)) lattice-as-matrix))
  (define picts-of-rows (map (lambda (r) (apply hc-append distance-between-configs r)) rows-of-picts))
  (define base-pict (apply vc-append distance-between-rows picts-of-rows))
  (define-values (pict-with-arrows _1)
    (for/fold ([mats base-pict] [top (first lattice-as-matrix)]) ([row (rest lattice-as-matrix)])
      (define new-mats
        (for/fold ([mats mats]) ([cell row])
          (for/fold ([mats mats]) ([to top])
            (if (and (same-up-to-one-module? (rest cell) (rest to))
                     (filter (rest cell))
                     (filter (rest to)))
                (if (lattice-edges?)
                    (pin-line #:color "darkgray" #:line-width 5 #:under? #t
                              mats (first cell) ct-find (first to) cb-find)
                    mats)
                mats))))
      (values new-mats row)))
  (ht-append 20 (scale pict-with-arrows SCALE)))

;; Proc Proc Proc [List-pf Pict] [Vector-of [List-of [Pair-of Int Int]]] Boolean -> [List-of [Cons Pict [List-of N]]]
(define (generate-lattice draw-node filter hilite all-modules data threshold)
  (define L (length all-modules))
  (for/list ((i (in-range (add1 L))))
    (define row (select i L))
    (for/list ((x row))
      (define idx (string->number (bit-vector->string (list->bit-vector x)) 2))
      (match-define (cons baseline-mean baseline-stddev)
                    (vector-ref data 0))
      (match-define (cons mean stddev) (vector-ref data idx))
      (define normalized-mean (/ mean baseline-mean))
      (define label
        (let ()
          (define normalized-stddev (/ stddev baseline-mean))
          (define style
	    (if #f #;(> normalized-mean 1.5)
	        '(bold . "Fira Mono OT")
		"Fira Mono OT"))
          (define base
            (text (~a (if (*show-ms*)
			  (~a (round mean) " ms / ")
			  "")
		      (~a (~r normalized-mean #:precision 2) "x"))
                  style (*label-size*)))
          (if (hilite idx)
              (backdrop base #:color "lightyellow")
              base)))
      (define threshold? (>= normalized-mean threshold))
      (define pict (draw-node label all-modules x threshold?))
      (cons (if (filter x) pict (ghost pict)) x))))

;; Pict (Listof Pict) (Listof B) Boolean *-> Pict
(define (pict-of-configuration label all-mods row threshold?)
  (define (color idx)
    (if (list-ref row idx) "powderblue" "moccasin"))
  (define base
    (apply hc-append
           10
           (for/list ([i (length row)])
             (cc-superimpose (filled-rectangle 150 210 #:color (color i) #:border-width 5 #:border-color "black")
                             (label-of-module (list-ref all-mods i))))))
  (define the-pict
    (tag-pict
     ((if (*use-freeze?*) freeze (λ (x) x))
      (vc-append
        (if (*bleach?*)
            (refocus (cc-superimpose base
                                     (rectangle/border (+ 30 (pict-width base))
                                                       (+ 120 (pict-height base))
                                                       #:color (if threshold? "dim gray" "lime")
						       #:border-width 2
						       #:border-color "black"))
                     base)
            base)
        (if (lattice-numbers?)
            label
            (ghost label))))
     (string->symbol
      (apply string-append (map (λ (b) (if b "O" "X")) row)))))
  the-pict)

;; String -> Pict
(define (label-of-module l #:rotate (r #f))
  (define t (tt l))
  (colorize
   (if r (rotate t (/ pi 2)) t)
   "black"))

;; ---------------------------------------------------------------------------------------------------
;; auxiliaries 

;; [List-of Boolean] [List-of Boolean] -> Boolean 
;; (= (length x) (length y))
;; is the module composition of x the same as y except for one untyped -> typed
(module+ test  ;; okay, I made a mistake 
  (require rackunit)
  (check-true (same-up-to-one-module? '(#f #f) '(#f #t)))
  (check-false (same-up-to-one-module? '(#f #f) '(#f #f))))
(define (same-up-to-one-module? x y)
  (let loop ((from x) (to y) (differ-so-far #f))
    (cond
      [(and (empty? from) (empty? to)) differ-so-far]
      [(and (typed? (first from)) (typed? (first to))) 
       (loop (rest from) (rest to) differ-so-far)]
      [(and (untyped? (first from)) (typed? (first to))) 
       (if differ-so-far #f (loop (rest from) (rest to) #t))]
      [(and (typed? (first from)) (untyped? (first to))) 
       #f]
      [(and (untyped? (first from)) (untyped? (first to))) 
       (loop (rest from) (rest to) differ-so-far)])))

;; Boolean -> Boolean 
(define typed? values)
(define untyped? not)

;; N N -> [List-of Boolean]
;; pick i places where the 'vector' of L booleans is #f, otherwise #t
;; (= (length (select i L)) L)
(define (select i L)
  (cond
    [(zero? i)  (list (build-list L (lambda (_) #t)))]
    [(zero? L) '()]
    [else (append (map (lambda (r) (cons #f r)) (select (sub1 i) (sub1 L)))
                  (map (lambda (r) (cons #t r)) (select i (sub1 L))))]))

(define (mean+stddev xs)
  (vector-map (λ (runs) (cons (mean runs) (stddev runs))) xs))

(define suffixtree-labels   '("data" "label" "lcs" "main" "structs" "ukkonen"))
(define suffixtree-data     (mean+stddev (file->value "data/suffixtree-large-06-30.rktd")))
(define suffixtree-diagram  (modules->diagram suffixtree-labels suffixtree-data))
(define (make-suffixtree-diagram #:filter [filter (λ (x) #t)]
                                 #:threshold [threshold +inf.0])
  (modules->diagram suffixtree-labels suffixtree-data
                    #:filter filter
                    #:threshold threshold))
