#lang racket/gui

(require "npict.rkt"
         "typed-helper.rkt"
         ;"lattice.rkt"
         images/compile-time
         (for-syntax images/icons/misc
                     images/icons/symbol
		     syntax/parse)
         images/icons/misc
         pict/balloon
         slideshow
         slideshow/flash
         slideshow/play
         unstable/gui/pict
         (prefix-in pp: unstable/gui/ppict)
         (only-in unstable/gui/ppict ppict-do coord)
         unstable/sequence
         unstable/gui/pslide
         unstable/gui/slideshow)

(provide (all-defined-out)
         (all-from-out pict/balloon)
         (all-from-out "typed-helper.rkt")
         (all-from-out "npict.rkt"))

(define (grayout-pict w h) (cellophane (filled-rectangle w h) 0.3))

;; make font-specific t functions
(define ((make-font-t font) content
                            [size (current-font-size)]
                            [angle 0]
                            #:color [color #f]
                            #:extra-style [extra-style '()])
  (define text-pict
    (text content (append extra-style font) size angle))
  (if color
      (colorize text-pict color)
      text-pict))

(define nu-red (make-color #xcc 00 00))
(define plt-blue "midnight blue")
(define title-1 (make-color 70 70 70))
(define title-2 (make-color 90 90 90))

;; images
(define racket-logo (bitmap "plt-logo.png"))

;; Bitstream Charter
(define t/btc (make-font-t "Bitstream Charter"))

;; Century Schoolbook
(define t/century (make-font-t "Century Schoolbook L"))

;; Gentium
(define t/gentium (make-font-t "Gentium"))

;; Liberation Serif
(define t/libs (make-font-t "Liberation Serif"))

;; corbert
(define t/corbert (make-font-t "Corbert"))

;; museo sans / slab
(define t/museo (make-font-t "Museo Sans"))
(define t/museo* (make-font-t "Museo Slab"))
(define t/museo*-bold (make-font-t "Museo Slab, Bold"))
(define t/museo*-italic (make-font-t "Museo Slab, Italic"))

;; sina nova
(define t/sina (make-font-t "Sina Nova"))

;; calluna
(define t/calluna (make-font-t "Calluna"))

;; inconsolata
(define t/inc (make-font-t "Inconsolata"))

;; Chunk Five
(define t/chunk (make-font-t "Chunkfive"))

;; kaushan
(define t/kau (make-font-t "Kaushan Script"))

;; Quicksand
(define t/quick (make-font-t "Quicksand"))

;; Quattrocento
(define t/quat (make-font-t "Quattrocento"))

;; Paladio
(define t/palladio (make-font-t "URW Palladio L"))

;; Utopia
(define t/utopia (make-font-t "Utopia"))

;; Aller
(define t/aller (make-font-t "Aller"))
(define t/aller* (make-font-t "Aller Light"))

;; Amble
(define t/amble (make-font-t "Amble"))

;; Fira Sans
(define t/fira (make-font-t "Fira Sans OT"))
(define t/fira-mono (make-font-t "Fira Mono OT"))
(define t/fira-mono-light (make-font-t "Fira Mono OT, Light"))
(define t/fira-italic (make-font-t "Fira Sans OT, Italic"))
(define t/fira-bold (make-font-t "Fira Sans OT, Bold"))
(define t/fira-bold-italic (make-font-t "Fira Sans OT, Bold Italic"))
(define t/fira-light (make-font-t "Fira Sans OT, Light"))
(define t/fira-light-italic (make-font-t "Fira Sans OT, Light Italic"))

;; Nobile
(define t/nobile (make-font-t "Nobile"))
(define t/nobile-bold (make-font-t "Nobile, Bold"))
(define t/nobile-italic (make-font-t "Nobile, Italic"))
(define t/nobile-medium (make-font-t "Nobile, Medium"))
(define t/nobile-bold-italic (make-font-t "Nobile, Bold Italic"))

;; Questa
(define t/questa (make-font-t "Questa Sans"))

;; Bebas
(define t/bebas (make-font-t "Bebas Neue"))
(define t/bebas-light (make-font-t "Bebas Neue, Light"))
(define t/bebas-italic (make-font-t "Bebas Neue, Italic"))
(define t/bebas-bold (make-font-t "Bebas Neue, Bold"))

;; Roboto
(define t/roboto (make-font-t "Roboto"))
(define t/roboto* (make-font-t "Roboto Condensed"))
(define t/roboto*-italic (make-font-t "Roboto Condensed, Italic"))
(define t/roboto-light (make-font-t "Roboto, Light"))
(define t/roboto-bold (make-font-t "Roboto, Bold"))
(define t/roboto-italic (make-font-t "Roboto, Italic"))
(define t/roboto*-bold (make-font-t "Roboto Condensed, Bold"))

;; Defaults
(define (body-text str [size (current-font-size)]
                   #:color [color "dim gray"])
  (t/roboto* str size #:color color))

;; pslide helper
(define-syntax-rule (pslide/staged [name ...] arg ...)
  (staged [name ...] (pslide arg ...)))

;; animations with pslide
(define-syntax (pslide/play stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:steps steps)
		   #:defaults ([steps #'20]))
	(~optional (~seq #:delay delay)
		   #:defaults ([delay #'0.01]))
	[n ...] body ...)
     #'(play-n (λ (n ...) (ppict-do ((pslide-base-pict)) body ...))
	       #:steps steps
	       #:delay delay)]))

(define-syntax-rule (pslide/play/staged [stage ...] [n ...] body ...)
  (staged [stage ...] (pslide/play [n ...] body ...)))

;; slide that goes in between sectionr
(define (section-slide name)
  (pslide #:go (coord 0.9 0.8)
          (scale racket-logo 1)
          #:go (coord 0.1 0.1 'lt)
          (ht-append 30
                     (colorize (linewidth 4 (vline 1 120)) title-1)
                     (t/bebas name 90 #:color title-1))))

(define (buffer pict
		#:color [color "firebrick"]
		#:border-color [border-color "firebrick"]
		#:border-width [border-width 1]
		#:padding [padding 10])
  (cc-superimpose (filled-rectangle #:color color
                                    #:border-color border-color
				    #:border-width 3
                                    (+ (pict-width pict) padding)
                                    (+ (pict-height pict) padding))
                  pict))

(define (screenbar pict #:color [color "firebrick"] #:height [height (* 0.3 client-h)])
  (cc-superimpose (filled-rectangle #:color color
                                    #:border-color color
                                    client-w height)
                  pict))

;; for formatting "bullet" text
(define (wrap1 pict)
  (lbox pict #:color "powderblue" #:shadow-color "white" #:padding 10))
(define (wrap2 pict)
  (lbox pict #:color "moccasin" #:shadow-color "white" #:padding 10))
