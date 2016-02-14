#lang at-exp racket/gui

;; POPL 2016 talk. Given a 25-minute slot from 15:35-16:00

(require "helper.rkt"
         "lattice.rkt"
         "timeline.rkt"
         "zoom.rkt"
         "seven-segment.rkt"
         "lnm-picts.rkt"
         racket/draw
         images/icons/misc
         images/icons/symbol
         (prefix-in p: plot/pict)
         ppict-slide-grid
         slideshow
         slideshow/code
         slideshow/play
         slideshow/repl
         unstable/gui/pict
         unstable/gui/ppict
         unstable/gui/pslide
         unstable/gui/slideshow)

(current-code-font "Inconsolata")

(define guide? #f)

(set-margin! 0)
(when guide? (set-grid-base-pict!))

;; no slide nums
(set-page-numbers-visible! #f)

(pslide-base-pict
 (λ () (colorize (filled-rectangle client-w client-h)
                 "white")))

(get-current-code-font-size (thunk 30))

(pslide
 #:go (coord 0.5 0 'ct)
 (screenbar (blank 1 1))
 #:go (coord 0.5 0.22)
 (t/bebas-bold "Is Sound Gradual Typing Dead?"
          80
          #:color "white")
 #:go (coord 0.5 0.75)
 (bitmap "northeastern-seal.png")
 #:go (coord 0.5 0.39 'ct)
 (hc-append 35
            (t/roboto* "Asumu Takikawa" 35 #:color nu-red)
            (t/roboto* "Daniel Feltey" 35 #:color title-2)
            (t/roboto* "Ben Greenman" 35 #:color title-2))
 (blank 1 20)
 (hc-append 35
            (t/roboto* "Max S. New" 35 #:color title-2)
            (t/roboto* "Jan Vitek" 35 #:color title-2)
            (t/roboto* "Matthias Felleisen" 35 #:color title-2))
 (blank 1 10))

;; Introduction section. (5m)

(pslide
  #:go (coord 0.5 0.13)
  (t/bebas-bold "Gradual typing thesis" 55)
  #:go (coord 0.5 0.5)
  (vc-append
   (screenbar (lc-superimpose (blank 760 1)
                              (t/roboto* "1. People write untyped code"))
              #:color "gainsboro"
              #:height (* 1/7 client-h))
   (screenbar (lc-superimpose (blank 760 1)
                              (t/roboto* "2. Static types help maintain software"))
              #:color "silver"
              #:height (* 1/7 client-h))
   (screenbar (lc-superimpose (blank 760 1)
                              (hbl-append
                               (t/roboto* "3. ")
                               (t/roboto*-italic "Sound types" #:color "midnightblue")
                               (t/roboto* " can be added ")
                               (t/roboto*-italic "incrementally" #:color "midnightblue")))
              #:color "gainsboro"
              #:height (* 1/7 client-h))
   (screenbar (lc-superimpose (blank 760 1)
                              (hbl-append
                               (t/roboto* "4. Types ")
                               (t/roboto*-italic "respect existing code" #:color "midnightblue")
                               (t/roboto* " & the result is ")
                               (t/roboto*-italic "runnable" #:color "midnightblue")))
              #:color "silver"
              #:height (* 1/7 client-h))))

(slide (t/bebas "Sound types" 90))

(let ()
  (define (err-msg strs)
    (lbox #:color "whitesmoke"
	  #:padding 30
     (apply vl-append
            (map (λ (str)
 	           (colorize ((current-code-tt) str) "red"))
 	         strs))))
  (define untyped-bg "mistyrose")
  (define typed-bg "powderblue")
  (define (code-pane code-pict color label)
    (ct-superimpose
     (ct-superimpose (filled-rectangle (/ client-w 2)
				       client-h
 				       #:border-color color
 				       #:color color)
 		     (vc-append (blank 1 50)
				(backdrop #:color "whitesmoke"
					  (if (pict? label)
					      label
					      (t/bebas label 50)))))
     (vc-append (blank 1 200)
		code-pict)))
  (pslide/staged [s1 result]
    #:go (coord 0.5 0.2)
    (body-text "The meaning of soundness" 40
  	     #:color "firebrick")
    #:go (coord 0.5 0.5)
    (hc-append
     (code-pane
       (vl-append ((current-code-tt) "#lang typed/racket/unsound")
		  (code code:blank)
		  (code (code:comment "fact.rkt"))
		  (code code:blank)
		  (code (provide fact))
		  (code code:blank)
		  (code (: fact (-> Integer Integer)))
		  (code (define (fact n)
			  (if (zero? n)
			      1
			      (* n (sub1 n))))))
       typed-bg
       "Unsound Typed")
     (linestyle 'long-dash (linewidth 3 (colorize (vline 1 client-h) "gray")))
     (code-pane
       (vl-append ((current-code-tt) "#lang racket")
		  (code code:blank)
		  (code (code:comment "use.rkt"))
		  (code code:blank)
		  (code (require "fact.rkt"))
		  (code code:blank)
		  (code (fact "ill-typed call")))
       untyped-bg
       "Untyped"))
    #:go (coord 0.5 0.5)
    (show (cc-superimpose
	   (grayout-pict client-w client-h)
	   (err-msg (list "; zero?: contract violation"
		          ";   expected: number?"
		          ";   given: \"ill-typed call\""
		          "; [,bt for context]")))
	  (>= stage result)))
  (pslide/staged [s1 result]
    #:go (coord 0.5 0.2)
    (body-text "The meaning of soundness" 40
  	     #:color "firebrick")
    #:go (coord 0.5 0.5)
    (hc-append
     (code-pane
       (vl-append ((current-code-tt) "#lang typed/racket")
		  (code code:blank)
		  (code (code:comment "fact.rkt"))
		  (code code:blank)
		  (backdrop #:color "whitesmoke"
			    (code (provide fact)))
		  (code code:blank)
		  (code (: fact (-> Integer Integer)))
		  (code (define (fact n)
			  (if (zero? n)
			      1
			      (* n (sub1 n))))))
       typed-bg
       (hc-append (cross-out (t/bebas "Unsound" 50)
			     #:width 3
			     #:color "red")
		  (t/bebas " Typed" 50)))
     (linestyle 'long-dash (linewidth 3 (colorize (vline 1 client-h) "gray")))
     (code-pane
       (vl-append ((current-code-tt) "#lang racket")
		  (code code:blank)
		  (code (code:comment "use.rkt"))
		  (code code:blank)
		  (code (require "fact.rkt"))
		  (code code:blank)
		  (code (fact "ill-typed call")))
       untyped-bg
       "Untyped"))
    #:go (coord 0.5 0.5)
    (show (cc-superimpose
	   (grayout-pict client-w client-h)
	   (err-msg (list "; fact: contract violation"
			  ";   expected: Integer"
			  ";   given: \"ill-typed call\""
			  ";   in: the 1st argument of"
			  ";       (-> Integer any)"
			  ";   contract from: \"fact.rkt\""
			  ";   blaming: \"use.rkt\"")))
	  (>= stage result))))

(slide (t/bebas "Results are runnable" 90))

(module tmp typed/racket
  (define-namespace-anchor t-anchor)
  (provide t-anchor))
(require 'tmp)
(parameterize ([get-current-code-font-size (thunk 15)])
  (define (code-pane code-pict color label)
    (ct-superimpose
     (ct-superimpose (filled-rectangle (/ client-w 2)
				       client-h
 				       #:border-color color
 				       #:color color)
 		     (vc-append (blank 1 50)
				(backdrop #:color "whitesmoke"
					  (t/bebas label 50))))
     (vc-append (blank 1 200)
		code-pict)))
  (define code-1
    (vl-append
     ((current-code-tt) "#lang racket/base")
     (code
      code:blank
      (provide (struct-out stream)
               make-stream stream-unfold
               stream-get stream-take)
      (struct stream (first rest))
      code:blank
      (define (make-stream hd thunk)
        (stream hd thunk))
      code:blank
      (define (stream-unfold st)
        (values (stream-first st)
                ((stream-rest st))))
      code:blank
      (define (stream-get st i)
        (define-values (hd tl)
          (stream-unfold st))
        (cond [(= i 0) hd]
              [else (stream-get tl (sub1 i))]))
      code:blank
      (define (stream-take st n)
        (cond [(= n 0) '()]
              [else
               (define-values (hd tl) (stream-unfold st))
               (cons hd (stream-take tl (sub1 n)))])))))
  (define code-2
    (vl-append
     ((current-code-tt) "#lang racket/base")
     (code
      code:blank
      (require "streams.rkt")
      code:blank
      (define (count-from n)
        (make-stream
         n (lambda () (count-from (add1 n)))))
      code:blank
      (define (sift n st)
        (define-values (hd tl) (stream-unfold st))
        (cond [(= 0 (modulo hd n)) (sift n tl)]
              [else
               (make-stream
                hd (lambda () (sift n tl)))]))
      code:blank
      (define (sieve st)
        (define-values (hd tl)
          (stream-unfold st))
        (make-stream hd (lambda () (sieve (sift hd tl)))))
      code:blank
      (define primes (sieve (count-from 2)))
      code:blank
      (define (main)
        (printf "The ~a-th prime number is: ~a\n" 100
                (stream-get primes 99)))
      code:blank
      (time (main)))))
  (define code-3
    (vl-append
     ((current-code-tt) "#lang typed/racket/base")
     (code
      code:blank
      (provide (struct-out stream)
               make-stream stream-unfold
               stream-get stream-take)
      (struct: stream ([first : Natural]
                       [rest : (-> stream)]))
      code:blank
      (: make-stream (-> Natural (-> stream) stream))
      (define (make-stream hd thunk)
        (stream hd thunk))
      code:blank
      (: stream-unfold (-> stream (values Natural stream)))
      (define (stream-unfold st)
        (values (stream-first st)
                ((stream-rest st))))
      code:blank
      (: stream-get (-> stream Natural Natural))
      (define (stream-get st i)
        (define-values (hd tl)
          (stream-unfold st))
        (cond [(= i 0) hd]
              [else (stream-get tl (sub1 i))]))
      code:blank
      (: stream-take (-> stream Natural (Listof Natural)))
      (define (stream-take st n)
        (cond [(= n 0) '()]
              [else
               (define-values (hd tl) (stream-unfold st))
               (cons hd (stream-take tl (sub1 n)))])))))

  (define untyped-bg "mistyrose")
  (define typed-bg "powderblue")

  (pslide/play #:steps 12 #:delay 0.01 [n]
    #:go (coord 0.5 0.5)
    (hc-append
     (code-pane code-1 untyped-bg "")
     (linestyle 'long-dash (linewidth 3 (colorize (vline 1 client-h) "gray")))
     (code-pane code-2 untyped-bg ""))
    #:go (coord 0.5 0.5)
    (scale (string-display (~r #:min-width 3 #:pad-string "0" (exact-round (* n 12))))
	   0.8)
    #:go (coord 0.5 0.1 'ct)
    (buffer (t/roboto-bold "Prime number sieve" #:color "white")))
  (pslide/play #:steps 100 #:delay 0.01 [n]
    #:go (coord 0.1 0.1 'lt)
    (buffer (t/roboto-bold "A Typed Racket demo - prime number sieve" #:color "white"))
    #:go (coord 0.5 0.5)
    (hc-append
     (code-pane code-3 typed-bg "")
     (linestyle 'long-dash (linewidth 3 (colorize (vline 1 client-h) "gray")))
     (code-pane code-2 untyped-bg ""))
    #:go (coord 0.5 0.1 'ct)
    (buffer (t/roboto-bold "Prime number sieve" #:color "white"))
    #:go (coord 0.5 0.5)
    (scale (string-display (~r #:min-width 3 #:pad-string "0" (exact-round (* n 100))))
	   0.8)))

(pslide
  (screenbar
   #:color "firebrick"
   (t/roboto* "10x slowdown could make the software undeliverable"
	      45 #:color "white")))

(pslide
  #:go (coord 0.1 0.1 'lt)
  (buffer (t/roboto-bold "Anecdotes from users" 40 #:color "white"))
  #:go (coord 0.1 0.3 'lt)
  (vl-append (hbl-append (body-text "“The end-product appears to be a " 35)
                         (body-text "50%-performance" #:color "black" 35))
             (body-text "  hybrid due to boundary contracts”" 35))
  #:go (coord 0.83 0.35 'lc)
  (t/roboto "2x" 55 #:color "firebrick")
  #:go (coord 0.23 0.5 'lt)
  (vl-append (body-text "“At this point, about one-fifth of my code is now typed." 35)
             (hbl-append (body-text " Unfortunately, this version is " 35)
                         (body-text "2.5 times slower" #:color "black" 35)
                         (body-text "”" 35)))
  #:go (coord 0.1 0.55 'lc)
  (t/roboto "2.5x" 55 #:color "firebrick")
  #:go (coord 0.1 0.7 'lt)
  (vl-append (hbl-append (body-text "“On my machine, it takes " 35)
                         (body-text "*twelve seconds*" 35 #:color "black")
                         (body-text " ..." 35))
             (hbl-append (body-text " ... the time taken is " 35)
                         (body-text "1ms" 35 #:color "black")
                         (body-text "”" 35)))
  #:go (coord 0.73 0.75 'lc)
  (t/roboto "12,000x" 55 #:color "firebrick"))

(pslide
  #:go (coord 0.5 0.5)
  (vr-append
   (vl-append (body-text "“As a practitioner, there are costs" 50)
	      (body-text "associated with using TR, therefore it" 50)
 	      (body-text "has to provide equivalent performance" 50)
	      (body-text "improvements to be worthwhile at all.”" 50))
   (body-text " — Matthew Butterick"
	      50 #:color "black")))
 
(pslide
  #:go (coord 0.5 0.5)
  (t/museo* "Why is it slow?" 75 #:color "dim gray")
  (blank 1 50)
  (vl-append 30
   (t/roboto-light "Bad programming / isolated incidents?" 40)
   (t/roboto-light "Bad implementation / design?" 40)
   (t/roboto-light "Fundamental issue with gradual typing?" 40)))

(pslide
  (screenbar
   #:color "firebrick"
   (t/roboto* "To answer, we need an evaluation method" 55 #:color "white")))

(pslide
  #:go (coord 0.5 0 'ct)
  (screenbar (vc-append (blank 1 20)
                        (t/bebas "Contributions of our paper" 70 #:color "white"))
             #:height (* 0.2 client-h))
  #:go (coord 0.5 0.4 'ct)
  (vl-append
    30
    (hbl-append (body-text "‣ " 40)
		(body-text "Evaluation method" 40 #:color "black")
		(body-text " for language implementors" 40))
    (hbl-append (body-text "‣ Idea for " 40)
		(body-text "graphically summarizing" 40 #:color "black")
		(body-text " evaluation results" 40))
    (hbl-append (body-text "‣ Results of " 40)
		(body-text "evaluating Typed Racket" #:color "black" 40)
		(body-text " using the method" 40))))

;; Key Concepts (5m)
;;
;; Introduce whatever key concepts of our gradual typing we
;; need to lay out here.
(section-slide "Key Concepts")

;; This section should introduce the terminology. First is
;;   - Macro-level gradual typing, then
;;   - Performance lattice, configuration
;;   - LNM metrics
;;   - "Usable", etc.

(pslide
  #:go (coord 0.5 0.5)
  (screenbar #:color "midnightblue"
	     (vc-append (hbl-append (t/roboto* "Programmers add types " 45 #:color "white")
				    (t/roboto* "incrementally" #:color "white" 45))
			(blank 1 30)
			(t/roboto-light "so should the evaluation method"
					45 #:color "white"))))

(parameterize ([lattice-edges? #t]
	       [*use-freeze?* #t]
	       [*show-ms* #f])
   (define w (* client-w 0.8))
   (define h (* client-h 0.6))
   (define spict (make-suffixtree-diagram))
   (define anim1 (tag-to-tag spict
                             'OOOOOO
                             'OOXOOO
                             w h))
   (define anim2 (tag-to-tag spict
                             'OOXOOO
                             'OXXOOO
                             w h))
   (define anim3 (tag-to-tag spict
                             'OXXOOO
                             'OXXXOO
                             w h))
   (define anim4 (tag-to-tag spict
                             'OXXXOO
                             'OXXXXO
                             w h))
   (define anim5 (tag-to-tag spict
                             'OXXXXO
                             'OXXXXX
                             w h))
   (define anim6 (tag-to-tag spict
                             'OXXXXX
                             'XXXXXX
                             w h))
   (define anim7 (tag-to-full spict 'XXXXXX
                              w h))
   (pslide
     #:go (coord 0.5 0.2)
     (t/roboto* "Suffixtree benchmark with 6 modules"
                40 #:color "dim gray")
     #:go (coord 0.5 0.5)
     (parameterize ([lattice-edges? #f]
                    [lattice-numbers? #f])
       (clip (inset (zoom-to (make-suffixtree-diagram
                              #:filter (λ (cell) (andmap not cell)))
                             'OOOOOO w h)
                    0 400))))
   (pslide
     #:go (coord 0.5 0.5)
     (parameterize ([lattice-edges? #f]
                    [lattice-numbers? #t]
		    [*show-ms* #t])
       (clip (inset (zoom-to (make-suffixtree-diagram
                              #:filter (λ (cell) (andmap not cell)))
                             'OOOOOO w h)
                    0 400))))
   (pslide/staged [s1 s2]
     #:go (coord 0.5 0.5)
     (parameterize ([lattice-edges? #f]
                    [lattice-numbers? #t])
       (clip (inset (zoom-to (make-suffixtree-diagram
                              #:filter (λ (cell) (andmap values cell)))
                             'XXXXXX w h)
                    0 400)))
     #:go (coord 0.5 0.2)
     (show
       (hbl-append (body-text "Reminder: " 40)
  		 (body-text "incremental" 40 #:color "black")
		 (body-text " addition of types" 40))
       (>= stage s2)))
   (pslide/play [n] #:go (coord 0.5 0.5) (anim1 n))
   (pslide/play [n] #:go (coord 0.5 0.5) (anim2 n))
   (pslide/play [n] #:go (coord 0.5 0.5) (anim3 n))
   (pslide/play [n] #:go (coord 0.5 0.5) (anim4 n))
   (pslide/play [n] #:go (coord 0.5 0.5) (anim5 n))
   (pslide/play [n] #:go (coord 0.5 0.5) (anim6 n))
   (pslide/play [n] #:go (coord 0.5 0.5) (anim7 n))
   (pslide
     #:go (coord 0.5 0.5)
     (scale (make-suffixtree-diagram) 
            0.1187)
     #:go (coord 0.5 0.2)
     (t/roboto* "The performance lattice"
                45 #:color "dim gray")
     (t/roboto-light "Paths in lattice are gradual migration paths"
		     40 #:color "dim gray")))

(pslide
  #:go (coord 0.5 0.5)
  (t/museo* "Why are the configs useful?" 65 #:color "dim gray")
  (blank 1 50)
  (hbl-append (t/roboto* "Reveals the cost of " 40 #:color "dim gray")
              (t/roboto* "boundaries" #:color "midnight-blue" 40)
              (t/roboto* " in gradual programs" 40 #:color "dim gray"))
  (blank 1 50)
  (hbl-append (body-text "Shows " 40)
	      (body-text "paths" #:color "midnight-blue" 40)
	      (body-text " from untyped to typed" 40)))

(let ()
  (define w (* client-w 0.8))
  (define h (* client-h 0.6))
  (pslide
    #:go (coord 0.5 0.25)
    (parameterize ([lattice-edges? #f]
                   [lattice-numbers? #t])
      (zoom-to (make-suffixtree-diagram
                 #:filter
                 (λ (cell) (equal? cell (list #f #t #f #f #f #f))))
               'OXOOOO w h))
    #:go (coord 0.5 0.65)
    (parameterize ([lattice-edges? #f]
                   [lattice-numbers? #t])
      (zoom-to (make-suffixtree-diagram
                 #:filter (λ (cell) (equal? cell (list #t #f #f #f #f #f))))
               'XOOOOO w h))
    #:go (coord 0.5 0.9)
    (body-text "Data / Label boundary is costly" 40))

  (pslide
    #:go (coord 0.5 0.25)
    (parameterize ([lattice-edges? #f]
                   [lattice-numbers? #t])
      (zoom-to (make-suffixtree-diagram
                 #:filter
                 (λ (cell) (equal? cell (list #t #t #f #f #f #f))))
               'XXOOOO w h))
    #:go (coord 0.5 0.65)
    (parameterize ([lattice-edges? #f]
                   [lattice-numbers? #t])
      (zoom-to (make-suffixtree-diagram
                 #:filter (λ (cell) (equal? cell (list #f #f #f #f #f #t))))
               'OOOOOX w h))
    #:go (coord 0.5 0.9)
    (body-text "When Data / Label have same color, it's more ok" 40)))

(pslide
  (screenbar
   #:color "firebrick"
   (t/roboto* "The visualization has some limitations" 60 #:color "white")))

(let ()
  (define (random-lattice)
    (parameterize ([*label-size* 250]
		   [*show-ms* #f])
      (modules->diagram '("a" "b" "c" "d")
  		      (for/vector ([i (in-range 16)])
  			(cons (random 3 100)
  			      (random))))))
  (define l1 (random-lattice))
  (define l2 (random-lattice))
  (pslide/staged [s1 s2]
    (body-text "Which one is better?" 50)
    (blank 1 50)
    (hc-append 30
	       (vc-append 30
			  (scale l1 0.25)
			  (body-text "Version 1" 40
				     #:color "black"))
	       (vc-append 30
			  (scale l2 0.25)
			  (body-text "Version 2" 40
				     #:color "black")))
    (blank 1 50)
    #:go (coord 0.5 0.5)
    (show
     (cc-superimpose
       (grayout-pict client-w client-h)
       (lbox #:color "white"
 	     #:padding 30
  	     (body-text "Summarize by proportion of “deliverable” configurations"
			40)))
     (>= stage s2))))

(pslide
  (vl-append (hbl-append (body-text "A configuration is " 60)
			 (body-text "N-deliverable" #:color "midnightblue" 60))
	     (body-text "if its overhead factor ≤ Nx" 60)))

(pslide/staged [1.1x 3x 5x 10x 20x]
  #:go (coord 0.5 0.5)
  (parameterize ([lattice-numbers? #f]
		 [lattice-edges? #f]
		 [*bleach?* #t])
    (scale (make-suffixtree-diagram
            #:threshold
            (cond [(= stage 1.1x) 1.1]
                  [(= stage 3x) 3.0]
                  [(= stage 5x) 5.0]
                  [(= stage 10x) 10.0]
                  [else 20.0]))
           0.135))
  (blank 1 30)
  (cond [(= stage 1.1x)
         (body-text "1.1-deliverable proportion: 6%" 50
		    #:color "black")]
        [(= stage 3x)
         (body-text "3-deliverable proportion: 9%" 50
		    #:color "black")]
        [(= stage 5x)
         (body-text "5-deliverable proportion: 19%" 50
		    #:color "black")]
        [(= stage 10x)
         (body-text "10-deliverable proportion: 22%" 50
		    #:color "black")]
        [else
         (body-text "20-deliverable proportion: 38%" 50
		    #:color "black")])
  #:go (coord 0.5 0.8)
  (show (body-text "Even at 20x, no paths from untyped to typed" 40)
	(>= stage 20x)))

(pslide
  (screenbar
   #:color "midnightblue"
   (t/roboto* "Visualize N-deliverable parameter with a CDF" 50
              #:color "white")))

(pslide
  #:go (coord 0.5 0.5)
  (scale lnm-suffixtree 3.5)
  #:go (coord 0.85 0.1 'rt)
  (t/roboto-bold "Suffixtree CDF" 50)
  #:go (coord 0.85 0.2 'rt)
  (hbl-append (body-text "Green" 40 #:color "forest green")
	      (body-text (format " line is at ~ax-deliverable" 3)
			 40))
  (body-text "Shallow slope = bad" 40))

(pslide
  #:go (coord 0.5 0.5)
  (scale lnm-gregor 3.5)
  #:go (coord 0.85 0.1 'rt)
  (t/roboto-bold "Gregor CDF" 50)
  #:go (coord 0.85 0.2 'rt)
  (body-text "Steep slope = good" 40))

(let ()
  (define (random-lattice)
    (parameterize ([*label-size* 250]
		   [*show-ms* #f])
      (modules->diagram '("a" "b" "c" "d")
  		      (for/vector ([i (in-range 16)])
  			(cons (random 3 100)
  			      (random))))))
  (define l1 (random-lattice))
  (define l2 (random-lattice))
  (pslide
    (hc-append 30 (scale l1 0.2) (scale l2 0.2))
    (blank 1 25)
    (colorize (linewidth 3 (hline client-w 1))
	      "dark gray")
    (blank 1 25)
    (hc-append 30
	       (scale lnm-kcfa 1.3)
	       (scale lnm-new-kcfa 1.3))))

(pslide
  #:go (coord 0.5 0 'ct)
  (screenbar (vc-append (blank 1 20)
                        (t/bebas "Summary of approach" 70 #:color "white"))
             #:height (* 0.2 client-h))
  #:go (coord 0.5 0.4 'ct)
  (vl-append 30
   (hbl-append (body-text "‣ Construct " 40)
	       (body-text "performance lattices"
			  40 #:color "black")
	       (body-text " for benchmarks" 40))
   (hbl-append (body-text "‣ Inspect lattices manually when feasible"
			  40))
   (hbl-append (body-text "‣ Compare lattices with " 40)
	       (body-text "N-deliverable CDF"
			  40 #:color "black"))))

;; Results (8m)
;;
;; In this section, show the actual LNM plots for some subset of the
;; benchmarks (or all of them?) and explain implications.
(section-slide "Results")

(pslide
  #:go (coord 0.5 0.2)
  (t/roboto* "Measured 12 curated benchmarks on all configs"
             #:color "dim gray" 45)
  #:go (coord 0.5 0.6)
  (vl-append
   (t/roboto* "5 are user-written libraries & programs"
              45)
   (blank 1 15)
   (t/roboto* "5 are educational programs" 45)
   (blank 1 15)
   (t/roboto* "2 were written for this paper" 45)))

(pslide
  #:go (coord 0.5 0.5)
  (hbl-append (body-text "Ran a total of " 50)
	      (body-text "75844 configurations" 50
			 #:color "firebrick"))
  (blank 1 30)
  (hbl-append (body-text "Took " 50)
	      (body-text "3 months"
			 50 #:color "firebrick")
	      (body-text " to run" 50)))

(let ([x 0.8])
  (define deliv-color (make-parameter "midnightblue"))
  (define (lc label percent-str pict)
    (rt-superimpose
     (scale pict x)
     (vc-append (blank 1 10)
		(hc-append (backdrop #:color "white"
				     (vr-append (t/roboto-bold label 20)
						(t/roboto-bold percent-str 23
							       #:color (deliv-color))))
			   (blank 10 1)))))

  (pslide
    #:go (coord 0.95 0.8 'rt)
    (backdrop #:color "white"
	      (t/roboto-bold "3-deliverable proportions" 50))
    #:go (coord 0.1 0.1 'lt)
    (vl-append (hc-append 20
                          (lc "Synth" "1%" lnm-synth)
                          (lc "Snake" "2%" lnm-snake)
                          (lc "Quad" "3%" lnm-quad)
                          (lc "Suffixtree" "9%" lnm-suffixtree))
               (hc-append 20
                          (lc "Tetris" "25%" lnm-tetris)
                          (lc "KCFA" "25%" lnm-kcfa)
                          (lc "Sieve" "50%" lnm-sieve)
                          (lc "Zordoz" "62%" lnm-zordoz))
               (hc-append 20
                          (lc "Gregor" "69%" lnm-gregor)
                          (lc "MBTA" "100%" lnm-mbta)
                          (lc "LNM" "100%" lnm-lnm)
			  (lc "Morse code" "100%" lnm-morse-code)))))

(let ()
  (define (fraction n1 n2)
    (vc-append (t/roboto-bold (~a n1) 70)
	       (linewidth 3 (hline 300 1))
	       (t/roboto-bold (~a n2) 70)))
(pslide
  #:go (coord 0.5 0.2)
  (body-text "1.1-deliverable configs over all benchmarks" 50)
  #:go (coord 0.5 0.5)
  (hc-append 30
	     (fraction 283 75844)
	     (t/roboto-bold "≈" 70)
	     (t/roboto-bold "0.4%" 70)))
(pslide
  #:go (coord 0.5 0.2)
  (body-text "3-deliverable configs over all benchmarks" 50)
  #:go (coord 0.5 0.5)
  (hc-append 30
	     (fraction 7992 75844)
	     (t/roboto-bold "≈" 70)
	     (t/roboto-bold "10.5%" 70))))

(pslide
  #:go (coord 0.5 0.5)
  (hbl-append (body-text "Bottom line: most configs not deliverable"
			 #:color "firebrick" 50))
  (blank 1 20)
  (t/roboto-light "Even with liberal 3x-deliverable criterion" 45)
  #:go (coord 0.5 0.5))

;; Conclusion (5m)
;;
;; Some remarks on how we're comparing new vs. old and maybe on some
;; of the analysis we are doing to narrow down what is slow.
(section-slide "So, is there hope?")

(let ()
  (define (label pict text)
    (rt-superimpose
     pict
     (vc-append (blank 1 10)
		(hc-append (backdrop #:color "white"
				     (t/roboto-bold text 40))
			   (blank 20 1)))))
  (pslide
    #:go (coord 0.5 0.2)
    (body-text "Suffixtree improvement" 50)
    #:go (coord 0.5 0.5)
    (hc-append 50
     	       (label (scale lnm-suffixtree 1.8)
		      "6.2")
	       (label (scale lnm-new-suffixtree 1.8)
		      "6.4.0.4"))
    #:go (coord 0.5 0.8)
    (hbl-append (body-text "9%" 40 #:color "black")
		(body-text " to " 40)
		(body-text "19%" 40 #:color "black")
		(body-text " improvement in 3-deliverability"
			   40))))

(let ()
  (define (label pict text)
    (rt-superimpose
     pict
     (vc-append (blank 1 10)
		(hc-append (backdrop #:color "white"
				     (t/roboto-bold text 40))
			   (blank 20 1)))))
  (pslide
    #:go (coord 0.5 0.2)
    (body-text "KCFA improvement" 50)
    #:go (coord 0.5 0.5)
    (hc-append 50
     	       (label (scale lnm-kcfa 1.8)
		      "6.2")
	       (label (scale lnm-new-kcfa 1.8)
		      "6.4.0.4"))
    #:go (coord 0.5 0.8)
    (hbl-append (body-text "25%" 40 #:color "black")
		(body-text " to " 40)
		(body-text "29%" 40 #:color "black")
		(body-text " improvement in 3-deliverability"
			   40))))

(pslide
  #:go (coord 0.5 0 'ct)
  (screenbar (vc-append (blank 1 20)
                        (t/bebas-bold "Hope" 70 #:color "white"))
             #:height (* 0.2 client-h)
             #:color "midnightblue")
  #:go (coord 0.5 0.25 'ct)
  (body-text "Evaluation method helps implementors" 50
	     #:color "firebrick")
  #:go (coord 0.5 0.40 'ct)
  (body-text "Helps measure improvements between versions" 40)
  (blank 1 30)
  (hc-append 30 (scale lnm-suffixtree 0.6) (scale lnm-new-suffixtree 0.6))
  (blank 1 30)
  (body-text "Can inspect lattice for bad configs" 40)
  #:go (coord 0.5 0.87)
  (parameterize ([lattice-edges? #f]
                 [lattice-numbers? #t])
    (zoom-to (make-suffixtree-diagram
              #:filter (λ (cell) (equal? cell (list #t #f #f #f #f #f))))
              'XOOOOO 300 100)))

(pslide
  #:go (coord 0.5 0 'ct)
  (screenbar (vc-append (blank 1 20)
                        (t/bebas-bold "Hope" 70 #:color "white"))
             #:height (* 0.2 client-h)
             #:color "midnightblue")
  #:go (coord 0.5 0.25 'ct)
  (body-text "Tools for avoiding GT performance pitfalls" 50
	     #:color "firebrick")
  #:go (coord 0.5 0.40 'ct)
  (body-text "Initial steps: contract profiler [St-Amour et al 2015]" 40)
  (blank 1 50)
  (lbox (scale (bitmap "contract-profile.png")
	       0.9)))

(pslide/staged [base thanks]
  #:go (coord 0.5 0 'ct)
  (screenbar (vc-append (blank 1 20)
                        (t/bebas-bold "Hope" 70 #:color "white"))
             #:height (* 0.2 client-h)
             #:color "midnightblue")
  #:go (coord 0.5 0.3 'ct)
  (body-text "Evaluation method helps GT system implementors" 40)
  (blank 1 30)
  (body-text "Tools for avoiding GT performance pitfalls" 40)
  #:go (coord 0.5 0.7)
  (hc-append 50
             (scale racket-logo 0.3)
             (vl-append 10
                        (body-text "Paper & Datasets:" 35 #:color "black")
                        (t/inc "http://www.ccs.neu.edu/racket/pubs/#popl16-tfgnvf" 25)))
  #:go (coord 0.5 0.85)
  (show (t/kau "Thank you!" 85)
        (>= stage thanks)))

(pslide)

(pslide
  #:go (coord 0.5 0.1 'ct)
  (body-text "Other research implementations of gradual typing"
	     #:color "firebrick" 40)
  #:go (coord 0.5 0.35 'ct)
  (scale timeline-pict 1.3)
  #:go (coord 0.5 0.8)
  (lbox (t/roboto* "Challenge: adapt this method to your chosen sound GT system" 35
                   #:color "firebrick")
	#:color "whitesmoke"))
