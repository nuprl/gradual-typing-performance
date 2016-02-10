#lang racket/base

(require "data.rkt"
         "render-lnm.rkt"
         pict)

(provide lnm-suffixtree
	 lnm-suffixtree-10x
	 lnm-suffixtree-20x
         lnm-sieve
         lnm-morse-code
         lnm-mbta
         lnm-zordoz
         lnm-lnm
         lnm-kcfa
         lnm-snake
         lnm-tetris
         lnm-synth
         lnm-gregor
         lnm-quad
         lnm-new-kcfa
         lnm-new-snake
         lnm-new-synth
         lnm-new-zordoz
         lnm-new-mbta
         lnm-new-suffixtree
         lnm-new-tetris
	 lnm-combined-suffixtree)

(define (do-lnm tag data #:n [n "3"]
		#:extra [extra #f])
  (render-lnm `#("--stats" "#f" "--legend" "#f" "-N" ,n "-M" "#f"
                 "--cutoff" "#f"
                 "--tag" ,tag
                 ,data)
	      #:extra extra))

(define lnm-suffixtree
  (do-lnm "suffixtree" "data/suffixtree-large-06-30.rktd"))
(define lnm-suffixtree-10x
  (do-lnm "suffixtree10x" "data/suffixtree-large-06-30.rktd" #:n "10"))
(define lnm-suffixtree-20x
  (do-lnm "suffixtree20x" "data/suffixtree-large-06-30.rktd" #:n "20"))

(define lnm-sieve
  (do-lnm "sieve" SIEVE-DATA))
(define lnm-morse-code
  (do-lnm "morse" MORSECODE-DATA))
(define lnm-mbta
  (do-lnm "mbta" MBTA-DATA))
(define lnm-zordoz
  (do-lnm "zordoz" ZORDOZ-DATA))
(define lnm-lnm
  (do-lnm "lnm" LNM-DATA))
(define lnm-kcfa
  (do-lnm "kcfa" KCFA-DATA))
(define lnm-snake
  (do-lnm "snake" SNAKE-DATA))
(define lnm-tetris
  (do-lnm "tetris" TETRIS-DATA))
(define lnm-synth
  (do-lnm "synth" SYNTH-DATA))
(define lnm-gregor
  (do-lnm "gregor" GREGOR-DATA))
(define lnm-quad
  (do-lnm "quad" QUAD-DATA))

(define lnm-new-kcfa
  (do-lnm "neo-kcfa" "data/6.4.0.4/kcfa-2016-01-17T15:59:32.rktd"))
(define lnm-new-snake
  (do-lnm "neo-snake" "data/6.4.0.4/snake-2016-01-17T12:24:11.rktd"))
(define lnm-new-synth
  (do-lnm "neo-synth" "data/6.4.0.4/synth-2016-01-17T13:56:20.rktd"))
(define lnm-new-zordoz
  (do-lnm "neo-zordoz" "data/6.4.0.4/zordoz.6.2.900.15-2016-01-17T15:57:49.rktd"))
(define lnm-new-mbta
  (do-lnm "neo-mbta" "data/6.4.0.4/mbta-2016-01-17T12:23:15.rktd"))
(define lnm-new-suffixtree
  (do-lnm "neo-suffixtree" "data/6.4.0.4/suffixtree-2016-01-17T12:40:38.rktd"))
(define lnm-new-tetris
  (do-lnm "neo-tetris" "data/6.4.0.4/tetris-2016-01-17T15:14:17.rktd"))

(define lnm-combined-suffixtree
  (do-lnm "suffixtree" "data/suffixtree-large-06-30.rktd"
	  #:extra "data/6.4.0.4/suffixtree-2016-01-17T12:40:38.rktd"))
