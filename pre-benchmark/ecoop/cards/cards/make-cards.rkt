(module make-cards typed/racket
  (require #;racket/class
           "list-hash.rkt"
           (prefix-in mred: typed/racket/gui)
           (prefix-in card-class: "card-class.rkt")
           "typed-base.rkt")

  (provide back deck-of-cards make-card)
  
  (: get-bitmap ((U #;Path-For-Some-System Path-String Input-Port) -> (Instance mred:Bitmap%)))
  (define (get-bitmap file)
    (make-object mred:bitmap% file))

  (: make-dim ((Instance mred:Bitmap%) -> (Instance mred:Bitmap%)))
  (define (make-dim bm-in)
    (let ([w (send bm-in get-width)]
	  [h (send bm-in get-height)])
      (let* ([bm (make-object mred:bitmap% w h)]
	     [mdc (new #;make-object mred:bitmap-dc% [bitmap bm])])
	(send mdc draw-bitmap bm-in 0 0)
	(let* ([len (* w h 4)]
	       [b (make-bytes len)])
	  (send mdc get-argb-pixels 0 0 w h b)
	  (let loop ([i 0])
	    (unless (= i len)
	      (bytes-set! b i (quotient (* 3 (bytes-ref b i)) 4))
	      (loop (add1 i))))
	  (send mdc set-argb-pixels 0 0 w h b))
	(send mdc set-bitmap #f)
	bm)))

  (: here ((U Path-For-Some-System Path-String 'up 'same) -> Path-String))
  (define here 
    (let ([cp (collection-path "games" "cards")])
      (lambda (file)
        (assert (build-path cp 
                    (if ((mred:get-display-depth)  . <= . 8)
			"locolor"
			"hicolor")
                    file) path-string?))))

  (: back (Instance mred:Bitmap%))
  (define back (get-bitmap (here "card-back.png")))

  (define dim-back
    (make-dim back))

  (define deck-of-cards
    (let* ([w (send back get-width)]
	   [h (send back get-height)])
      (let: sloop : (Listof (Instance Card%)) ([suit : Integer 4])
	(if (zero? suit)
	    null
	    (let: vloop : (Listof (Instance Card%)) ([value : Integer 13])
	      (sleep)
	      (if (zero? value)
		  (sloop (sub1 suit))
		  (let ([front (get-bitmap
				(here 
				 (format "card-~a-~a.png"
					 (sub1 value)
					 (sub1 suit))))])
		    (cons (new #;make-object card-class:card%
			    [-suit-id suit]
			    [-value value]
			    [-width w] [-height h]
			    [-front front] [-back back]
			    [-mk-dim-front (lambda () (make-dim front))]
			    [-mk-dim-back (lambda () dim-back)]
                            [-rotated-bms (make-hash)])
			  (vloop (sub1 value))))))))))
  
  (: make-card ((Instance mred:Bitmap%) (Instance mred:Bitmap%) Any Any -> (Instance Card%)))
  (define (make-card front-bm back-bm suit-id value)
    (let ([w (send back get-width)]
	  [h (send back get-height)])
      (new #;make-object card-class:card%
		   [-suit-id suit-id]
		   [-value value]
		   [-width w] [-height h]
		   [-front front-bm] [-back (or back-bm back)]
		   [-mk-dim-front (lambda () (make-dim front-bm))]
		   [-mk-dim-back (lambda ()
		     (if back-bm 
			 (make-dim back)
			 dim-back))]
                   [-rotated-bms (make-hash)]))))
