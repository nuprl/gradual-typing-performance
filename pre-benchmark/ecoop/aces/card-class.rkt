
(module card-class typed/racket
  (require
    (prefix-in mred: typed/racket/gui)
    "snipclass.rkt"
    "list-hash.rkt"
    "typed-base.rkt")
  (require (only-in racket/base case-lambda))
  (provide card%) 
  
  (: prev-regions (Option (Pairof (Instance mred:Region%) (Instance mred:Region%))))
  (define prev-regions #f)
  (: prev-region-dc (Option (Instance mred:DC<%>)))
  (define prev-region-dc #f)
  (: with-card-region ((Instance mred:DC<%>) Real Real Nonnegative-Real Nonnegative-Real (-> Any) -> Void))
  (define (with-card-region dc x y width height thunk)
    (let ([rs (if #f ; (eq? prev-region-dc dc) <- assumes the same xform matrix
                  prev-regions
		  (cons (new mred:region% [dc dc])
                   #;(make-object mred:region% dc)
                   (new mred:region% [dc dc])     
			#;(make-object mred:region% dc)))])
      (set! prev-regions rs)
      (set! prev-region-dc dc)
      (send (car rs) set-rectangle x (add1 y) width (- height 2))
      (send (cdr rs) set-rectangle (add1 x) y (- width 2) height)
      (send (car rs) union (cdr rs))
      (let ([r (send dc get-clipping-region)])
	(when r
	  (send (car rs) intersect r))
	(send dc set-clipping-region (car rs))
	(thunk)
	(send dc set-clipping-region r))))

  (: rotate-bm ((Instance mred:Bitmap%) Boolean -> (Instance mred:Bitmap%)))
  (define (rotate-bm bm cw?)
    (let: ([w : Natural (send bm get-width)]
           [h : Natural (send bm get-height)])
      (let: ([bm2 : (Instance mred:Bitmap%) (make-object mred:bitmap% h w)]
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
        (let ([dc (new mred:bitmap-dc% [bitmap bm2])#;(make-object mred:bitmap-dc% bm2)])
          (send dc set-argb-pixels 0 0 h w s2)
          (send dc set-bitmap #f))
        bm2)))
  (: orientations (Sequenceof Dir))
  (define orientations 
    (sequence-map 
     (lambda: ([n : Natural]) (list-ref (list 'n 'e 's 'w) (modulo n 4)))
     (in-naturals)))
  (: find-head (All (A) ((Sequenceof A) A -> (Sequenceof A))))
  (define (find-head l s)
    (if (eq? (sequence-ref l 0) s)
        l
        (find-head (sequence-tail l 1) s)))
  #;(define orientations (shared ([o (list* 'n 'e 's 'w o)]) o))
  #;(define (find-head l s)
    (if (eq? (car l) s)
        l
        (find-head (cdr l) s)))
  
  
           
           
  (: card% Card%)
  (define card%
    (class mred:snip%
      (init -suit-id -value -width -height -front -back -mk-dim-front -mk-dim-back -rotated-bms)
      (inherit set-snipclass set-count get-admin)
      (: suit-id Real)
      (define suit-id -suit-id)
      (: value Real)
      (define value -value)
      (: width Natural)
      (define width -width)
      (: height Natural)
      (define height -height)
      (: rotated Dir)
      (define rotated 'n)
      (: front (Instance mred:Bitmap%))
      (define front -front)
      (: back (Instance mred:Bitmap%))
      (define back -back)
      (: mk-dim-front (-> (Instance mred:Bitmap%)))
      (define mk-dim-front -mk-dim-front)
      (: mk-dim-back (-> (Instance mred:Bitmap%)))
      (define mk-dim-back -mk-dim-back)
      (: dim-front (Option (Instance mred:Bitmap%)))
      (define dim-front #f)
      (: dim-back (Option (Instance mred:Bitmap%)))
      (define dim-back #f)
      (: is-dim? Boolean)
      (define is-dim? #f)
      (: flipped? Boolean)
      (define flipped? #f)
      (: semi-flipped? Boolean)
      (define semi-flipped? #f)
      (: can-flip? Boolean)
      (define can-flip? #t)
      (: can-move? Boolean)
      (define can-move? #t)
      (: snap-back? Boolean)
      (define snap-back? #f)
      (: stay-region (Option Region))
      (define stay-region #f)
      (: home-reg (Option Region))
      (define home-reg #f)
      (: rotated-bms (Hash (Pairof Dir (Instance mred:Bitmap%)) (Instance mred:Bitmap%)))
      (define rotated-bms -rotated-bms)
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
                                  [(s) 180]
                                  [else 0]))]
        [rotate (lambda (mode)
                  (let ([delta (case mode
                                 [(0 360) 0]
                                 [(cw -90 270) 1]
                                 [(ccw 90 -270) 3]
                                 [(180 -180) 2]
                                 [else (error 'rotate "bad mode: ~e" mode)])])
                    (set! rotated (sequence-ref #;list-ref (find-head orientations rotated) delta))
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
	 (lambda (w h) #f #;(void))] ;potential bug in this implementation
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
                    (with-card-region
                     dc x y width height
                     (lambda ()
                       (send dc draw-bitmap 
                             (let: ([bm : (Option (Instance mred:Bitmap%)) (if flipped? 
                                           (if is-dim? dim-back back)
                                           (if is-dim? dim-front front))])
                               (get-rotated (assert bm) rotated))
                             x y))))])
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
                  (new card% [-suit-id suit-id] [-value value] 
                               [-width (if rotated? height width)] 
                               [-height (if rotated? width height )]
                               [-front front] [-back back]
                               [-mk-dim-front (lambda () 
                                 (unless dim-front 
                                   (set! dim-front (mk-dim-front)))
                                 (let ([dim-front : (U #f (Instance mred:Bitmap%)) dim-front])
                                   (assert dim-front)))]
                               [-mk-dim-back (lambda () 
                                 (unless dim-back 
                                   (set! dim-back (mk-dim-back)))
                                 (let ([dim-back : (U #f (Instance mred:Bitmap%)) dim-back])
                                   (assert dim-back)))]
                               [-rotated-bms rotated-bms])))])
      (: save-x (Boxof Real))
      (define save-x (ann (box 0) (Boxof Real)))
      (: save-y (Boxof Real))
      (define save-y (ann (box 0) (Boxof Real)))
      (public*
	[remember-location
	 (lambda (pb)
	   (send pb get-snip-location this save-x save-y))]
	[back-to-original-location
	 (lambda (pb)
	   (when snap-back?
	     (send pb move-to this (unbox save-x) (unbox save-y)))
           (define home-reg* home-reg)
	   (when home-reg*
	     (let: ([xbox : (Boxof Real) (box 0)]
		   [ybox : (Boxof Real) (box 0)])
	       (send pb get-snip-location this xbox ybox #f)
	       ;; Completely in the region?
	       (let* ([l (unbox xbox)]
		      [rl (region-x (assert home-reg*))]
		      [r (+ l width)]
		      [rr (+ rl (region-w (assert home-reg*)))]
		      [t (unbox ybox)]
		      [rt (region-y (assert home-reg*))]
		      [b (+ t height)]
		      [rb (+ rt (region-h (assert home-reg*)))])
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
      
      #;(super-make-object)
      (super-new)
      (set-count 1)
      (set-snipclass sc)
      (flip))))
