#lang typed/racket/base

(require typed/racket/class
         racket/sequence ; workaround: to replace shared
         (prefix-in mred: typed/racket/gui)
         "typed-base.rkt")

(require "require-typed-check.rkt") ; benchmark instrumentation
(require/typed/if "region-typed.rkt" "region.rkt")

(provide card%)

;; partial inline of list-hash.rkt here

(provide hash-ref hash-set!)

(: hash-ref (All (a b c) 
                 (case-> ((Hash a b) a -> (Option b))
                         ((Hash a b) a (-> c) -> (U b c)))))
(define (hash-ref bhash key [failure-result (λ () #f)])
  (let* ([hash (unbox bhash)]
         [res (assoc key hash)])
    (if res
        (cdr res)
        (failure-result))))

(: hash-set! (All (a b) ((Hash a b) a b -> Void)))
(define (hash-set! bhash key v)
  (set-box! bhash (update-hash-list (unbox bhash) key v)))

(: update-hash-list (All (a b) ((Listof (Pairof a b)) a b -> (Listof (Pairof a b)))))
(define (update-hash-list hash key v)
  (cond
    [(null? hash) (list (cons key v))]
    [(equal? (car hash) key) (cons (cons key v) (cdr hash))]
    [else (cons (car hash) (update-hash-list (cdr hash) key v))]))

;; ------------------------------------

;; snipclass.rkt inlined here

(provide sc)
(define sc (make-object mred:snip-class%))
(send sc set-classname "card")
(send (mred:get-the-snip-class-list) add sc)

;; --------------------------
  
(: prev-regions (Option (Pairof (Instance mred:Region%) (Instance mred:Region%))))
(define prev-regions #f)
(: prev-region-dc (Option (Instance mred:DC<%>)))
(define prev-region-dc #f)

(: rotate-bm ((Instance mred:Bitmap%) Boolean -> (Instance mred:Bitmap%)))
(define (rotate-bm bm cw?)
  (let: ([w : Exact-Positive-Integer (send bm get-width)]
         [h : Exact-Positive-Integer (send bm get-height)])
    (let: ([bm2 : (Instance mred:Bitmap%) (mred:make-bitmap h w)]
           [s : Bytes (make-bytes (* w h 4))]
           [s2 : Bytes (make-bytes (* h w 4))])
      (send bm get-argb-pixels 0 0 w h s)
      (for ([i (in-range w)])
        (for ([j (in-range h)])
          (let ([src-pos (* (+ i (* j w)) 4)])
            (bytes-copy! s2 
                         (if cw?
                             (* (+ (- (- h j) 1) (* i h)) 4) 
                             (* (+ j (* (- (- w i) 1) h)) 4))
                         s src-pos (+ src-pos 4)))))
      (let ([dc (make-object mred:bitmap-dc% bm2)])
        (send dc set-argb-pixels 0 0 h w s2)
        (send dc set-bitmap #f))
      bm2)))

(: orientations (Sequenceof Dir))
(define orientations (in-cycle (list 'n 'e 's 'w))) ;; Workaround: `shared` isn't supported in TR
(: find-head (All (A) ((Sequenceof A) A -> (Sequenceof A))))
(define (find-head l s)
  (if (eq? (sequence-ref l 0) s)
      l
      (find-head (sequence-tail l 1) s)))

(: card% Card%)
(define card%
  (class mred:snip%
    (init -suit-id -value -width -height -front -back -mk-dim-front -mk-dim-back -rotated-bms)
    (inherit set-snipclass set-count get-admin)
    (define: suit-id : Any -suit-id)
    (define: value : Any -value)
    (define: width : Natural -width)
    (define: height : Natural -height)
    (define: rotated : Dir 'n)
    (define: front : (Instance mred:Bitmap%) -front)
    (define: back : (Instance mred:Bitmap%) -back)
    (define: mk-dim-front : (-> (Instance mred:Bitmap%)) -mk-dim-front)
    (define: mk-dim-back : (-> (Instance mred:Bitmap%)) -mk-dim-back)
    (define: dim-front : (Option (Instance mred:Bitmap%)) #f)
    (define: dim-back : (Option (Instance mred:Bitmap%)) #f)
    (define: is-dim? : Boolean #f)
    (define: flipped? : Boolean #f)
    (define: semi-flipped? : Boolean #f)
    (define: can-flip? : Boolean #t)
    (define: can-move? : Boolean #t)
    (define: snap-back? : Boolean #f)
    (define: stay-region : (Option Region) #f)
    (define: home-reg : (Option Region) #f)
    (define: rotated-bms : (Hash (Pairof Dir (Instance mred:Bitmap%)) (Instance mred:Bitmap%)) -rotated-bms)
    (: refresh (-> Void))
    (: refresh-size (-> Void))
    (: check-dim (-> Void))
    (: get-rotated ((Instance mred:Bitmap%) Dir -> (Instance mred:Bitmap%)))
    (private*
      [refresh
       (lambda ()
         (let ([a (get-admin)])
           (when a
             (send a needs-update this 0 0 width height))))]
      [refresh-size
       (lambda ()
         (let ([a (get-admin)])
           (when a
             (send a resized this #f)))
         (refresh))]
      [check-dim
       (lambda ()
         (when is-dim?
           (if flipped?
      	 (unless dim-back
      	   (set! dim-back (mk-dim-back)))
      	 (unless dim-front
      	   (set! dim-front (mk-dim-front))))))]
      [get-rotated
       (lambda (bm dir)
         (if (eq? dir 'n)
             bm
             (or (hash-ref rotated-bms (cons dir bm) (lambda () #f))
                 (let ([rotated-bm (case dir
                                     [(w) (rotate-bm bm #f)]
                                     [(e) (rotate-bm bm #t)]
                                     [(s) (rotate-bm (rotate-bm bm #t) #t)])])
                   (hash-set! rotated-bms (cons dir bm) rotated-bm)
                   rotated-bm))))])
    (public*
      [face-down? (lambda () flipped?)]
      [flip
       (lambda ()
         (set! flipped? (not flipped?))
         (refresh))]
      [semi-flip
       (lambda ()
         (set! semi-flipped? (not semi-flipped?))
         (refresh))]
      [face-up (lambda () (when flipped? (flip)))]
      [face-down (lambda () (unless flipped? (flip)))]
      [dim (case-lambda 
            [() is-dim?]
            [(v)
             (unless (eq? is-dim? (and v #t))
      	 (set! is-dim? (and v #t))
      	 (refresh))])]
      [orientation (lambda () (case rotated
                                [(n) 0]
                                [(e) 270]
                                [(w) 90]
                                [(s) 180]))]
      [rotate (lambda (mode)
                (let ([delta (case mode
                               [(0 360) 0]
                               [(cw -90 270) 1]
                               [(ccw 90 -270) 3]
                               [(180 -180) 2]
                               [else (error 'rotate "bad mode: ~e" mode)])])
                  (set! rotated (sequence-ref (find-head orientations rotated) delta))
                  (if (odd? delta)
                      (let ([w width])
                        (set! width height)
                        (set! height w)
                        (refresh-size))
                      (refresh))))]
      [get-suit-id
       (lambda () suit-id)]
      [get-suit
       (lambda ()
         (case suit-id
           [(1) 'clubs]
           [(2) 'diamonds]
           [(3) 'hearts]
           [(4) 'spades]
           [else 'unknown]))]
      [get-value
       (lambda () value)]
      [user-can-flip
       (case-lambda
        [() can-flip?]
        [(f) (set! can-flip? (and f #t))])]
      [user-can-move
       (case-lambda
        [() can-move?]
        [(f) (set! can-move? (and f #t))])]
      [snap-back-after-move
       (case-lambda
        [() snap-back?]
        [(f) (set! snap-back? (and f #t))])]
      [stay-in-region
       (case-lambda
        [() stay-region]
        [(r) (set! stay-region r)])]
      [home-region
       (case-lambda
        [() home-reg]
        [(r) (set! home-reg r)])]
      [card-width (lambda () width)]
      [card-height (lambda () height)])
    (override*
      [resize
       (lambda (w h) #f)] ; NB: potential bug in this implementation (was void)
      [get-extent
       (lambda (dc x y [w #f] [h #f] [descent #f] [space #f] [lspace #f] [rspace #f])
         ((inst map Void (Option (Boxof Nonnegative-Real)))
          (lambda: ([b : (Option (Boxof Nonnegative-Real))])
            (when b
      	      ((inst set-box! Nonnegative-Real) b 0)))
          (list descent space lspace rspace))
         (when w (set-box! w width))
         (when h (set-box! h height)))]
      [draw
       (lambda (dc x y left top right bottom dx dy draw-caret)
         (check-dim)
         (let ([do-draw
                (lambda: ([x : Real] [y : Real])
                  (send dc draw-bitmap
                        (let ([bm (if flipped?
                                      (if is-dim? dim-back back)
                                      (if is-dim? dim-front front))])
                          (get-rotated (assert bm) rotated)) ; NB: typestate needed on bm (check-dim)
                        x y)
                  (void))])
           (if semi-flipped?
               (let-values ([(sx sy) (send dc get-scale)])
                 (case rotated
                   [(n s)
                    (send dc set-scale (/ sx 2) sy)
                    (do-draw (+ (* 2 x) (/ width 2)) y)
                    (send dc set-scale sx sy)]
                   [(e w)
                    (send dc set-scale sx (/ sy 2))
                    (do-draw x (+ (* 2 y) (/ height 2)))
                    (send dc set-scale sx sy)]))
               (do-draw x y))))]
      [copy (lambda () 
              (let ([rotated? (memq rotated '(e w))])
                (make-object card% suit-id value
                                   (if rotated? height width)
                                   (if rotated? width height )
                                   front back
                                   (lambda ()
                                     (unless dim-front
                                       (set! dim-front (mk-dim-front)))
                                     (assert dim-front))
                                   (lambda ()
                                     (unless dim-back
                                       (set! dim-back (mk-dim-back)))
                                     (assert dim-back))
                                   rotated-bms)))])
    (define: save-x : (Boxof Real) (box 0))
    (define: save-y : (Boxof Real) (box 0))
    (public*
      [remember-location
       (lambda (pb)
         (send pb get-snip-location this save-x save-y))]
      [back-to-original-location
       (lambda (pb)
         (when snap-back?
           (send pb move-to this (unbox save-x) (unbox save-y)))
         (when home-reg
           (let: ([xbox : (Boxof Real) (box 0)]
      	          [ybox : (Boxof Real) (box 0)])
             (send pb get-snip-location this xbox ybox #f)
             ;; Completely in the region?
             (let* ([l (unbox xbox)]
      	            [rl (region-x (assert home-reg))]
      	            [r (+ l width)]
      	            [rr (+ rl (region-w (assert home-reg)))]
      	            [t (unbox ybox)]
      	            [rt (region-y (assert home-reg))]
      	            [b (+ t height)]
      	            [rb (+ rt (region-h (assert home-reg)))])
      	 (when (or (< l rl) (> r rr)
      		   (< t rt) (> b rb))
      	   ;; Out of the region - completely or partly?
      	   (if (and (or (<= rl l rr) (<= rl r rr))
      		    (or (<= rt t rb) (<= rt b rb)))
      	       ;; Just slightly out
      	       (send pb move-to this
      		     (min (max l rl) (- rr width))
      		     (min (max t rt) (- rb height)))
      	       ;; Completely out
      	       (send pb move-to this (unbox save-x) (unbox save-y))))))))])

    (super-make-object)
    (set-count 1)
    (set-snipclass sc)
    (flip)))
