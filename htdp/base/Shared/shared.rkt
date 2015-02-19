#lang at-exp racket

(require "latex.rkt")
(provide (all-from-out "latex.rkt"))

(define is-draft? (make-parameter #false))
(provide is-draft?)

(define note-on-teaching "../base/Notes/A_Note_on_Teaching_part_I.html")
(define note-on-mice-and-men "../base/Notes/A_Note_on_Mice_and_Characters.html")

(define (bsl) "BSL")
(define (bsl+) "BSL+")
(define (isl) "ISL")
(define (isl+) "ISL+")
(define (asl) "ASL")
(define (htdc) @emph{How to Design Components})

(define *op (current-output-port))
(define (tee x) (displayln x *op) x)

(define prompt (tt ">"))

(define ALT "|")
(define IS "=")
(define DOTS "...")

(define Posn (tech "Posn"))
(define Boolean (tech "Boolean"))

(define (dr) "DrRacket")
(define (sch) "Racket")

(require 2htdp/batch-io)
(define (file-is f [name "name of file"])
  (define x (read-file f))
  (filebox (tt name) (verbatim x)))

;; -> String
;; the current year 
(define (year)
  (number->string (date-year (seconds->date (current-seconds)))))

(require
  scribble/manual
  (except-in scribble/core make-table make-paragraph)
  (prefix-in core: (only-in scribble/core make-table))
  (only-in scribble/struct
           make-paragraph
           make-blockquote
           flow-paragraphs
           make-flow  
           make-table
           make-with-attributes)
  scribble/eval
  racket/sandbox
  scribble/scheme
  scribble/decode
  scriblib/figure
  scriblib/footnote
  scribble/html-properties
  scribble/latex-properties
  mzlib/etc
  mzlib/pconvert
  file/convertible
  (for-syntax scribble/scheme))

;; Any -> String 
(define (pretty-decimal s)
  (parameterize ([pretty-print-exact-as-decimal #t])
    (pretty-format s)))

(provide
 pretty-decimal
 (all-defined-out)
 (all-from-out scribble/manual)
 (all-from-out scribble/struct)
 (all-from-out scribble/core)
 (all-from-out scribble/eval)
 (all-from-out scriblib/figure)
 (all-from-out scriblib/footnote)
 ; (all-from-out scribble/html-variants)
 (for-syntax (all-from-out scheme)))

(define-syntax-rule
  (bq word ...)
  (nested-flow
   (make-style 'inset '())
   (list (paragraph (make-style #f '()) '(word ...)))))

(define book-style
  (make-style "a bunch of things"
              (list
               'toc
               (make-css-addition "../base/Shared/shared.css")
               (make-tex-addition "../base/Shared/shared.tex"))))

(define p make-paragraph)

(define (pright l)
  (paragraph (make-style "Right" '()) l))

;; -----------------------------------------------------------------------------

(define-syntax-rule (def-name name)
  (define-syntax name
    (make-element-id-transformer (lambda (stx) (syntax 'name)))))

(def-name name)

;; -----------------------------------------------------------------------------
(define (help) "HelpDesk")

(define (stepper) "stepper")

;; button label (string) 
(define (button b) (emph (string-upcase b)))

;; String -> Element 
;; path to a file or library or teachpack
(define (tp x) (element 'roman `("the " ,(filepath x) " library")))
(define (Tp x) (element 'roman `("The " ,(filepath x) " library")))

(define (key b) (format "~s" b))

(define (menu m) (tt m))

(define redx values)

;; -----------------------------------------------------------------------------
(require
  (for-syntax 
   scribble/scheme
   scheme
   (only-in redex/reduction-semantics apply-reduction-relation)
   redex/examples/beginner))

;; (listof BSLDef) BSLExp -> ...
(define-syntax (computation stx)
  (syntax-case stx ()
    [(_ def ... exp)
     (let ()
       (define p (syntax->datum (syntax (def ...))))
       (define e (syntax exp))
       (define l (syntax-line e))
       (define (computation e line#)
         (define t (apply-reduction-relation reductions `(,@p ,e)))
         ; (define _ (printf "~s\n ~s\n ~s\n" p e t))
         (define f (first t))
         (define e@t+1 (last (first t)))
         (cons e
               (cons '=
                     (if (final? e@t+1)
                         (list e@t+1)
                         (computation e@t+1 (+ line# 2))))))
       ;; BSLExp -> Boolean 
       (define (final? e)
         (or (number? e) (string? e) (boolean? e) (symbol? e)))
       (define b0 (computation (syntax->datum e) l))
       (define b (map (lambda (x) #`(to-element #,x)) b0))
       #`(schemeblock #,@b))]))

(define-syntax (foo stx)
  #`(schemeblock
     (unsyntax #'#,(to-element '(+ 1 2))) (unsyntax #'#,(to-element '=))
     (unsyntax #'#,(to-element '3))))

; @schemeblock[
;  #,(to-element '(+ 1 2))
;  #,(to-element '=)
;  #,(to-element '3)
;  ]

#;
(computation (define (ff a) (* 10 a))
             (ff (+ 1 1)))

;; -----------------------------------------------------------------------------

#|
#lang scribble/manual

@(require scribble/eval)

@(define ex-eval (make-base-eval))
@(interaction-eval
 #:eval ex-eval
 (begin
   (require racket/pretty)
   (current-print pretty-print-handler)))

@interaction[ 
#:eval ex-eval
(define (f x)
  `(,(build-list x add1)
    ,(build-list x add1)))
(f 20)
]

@(close-eval ex-eval)
|#

; (define-namespace-anchor anchor)
; (define orig-ns (namespace-anchor->namespace anchor))

(require teachpack/2htdp/scribblings/img-eval)

(define-syntax *sl-eval
  (syntax-rules ()
    [(_ module-lang reader [def ...] exp ...)
     ;; ===>>>
     (let ()
       (define me (parameterize ([sandbox-propagate-exceptions #f])
                    (make-img-eval)))
       (me '(require (only-in racket empty? first rest cons? sqr true false)))
       (me '(require racket/pretty))
       (me '(current-print pretty-print-handler))
       (me '(pretty-print-columns 65))
       (me 'def) ... ;; <--- too early ?
       (call-in-sandbox-context me (lambda () (error-print-source-location #f)))
       (call-in-sandbox-context me (lambda () (sandbox-output 'string)))
       (call-in-sandbox-context me (lambda () (sandbox-error-output 'string)))
       (call-in-sandbox-context me (lambda () (sandbox-propagate-exceptions #f)))
       (call-in-sandbox-context me (lambda ()
                                     (current-print-convert-hook
                                      (let ([prev (current-print-convert-hook)])
                                        ;; tell `print-convert' to leave images as themselves:
                                        (lambda (v basic sub)
                                          (if (convertible? v)
                                              v
                                              (prev v basic sub)))))
                                     
                                     (pretty-print-size-hook
                                      (let ([prev (pretty-print-size-hook)])
                                        ;; tell `pretty-print' that we'll handle images specially:
                                        (lambda (v w? op)
                                          (if (convertible? v) 1 (prev v w? op)))))
                                     
                                     (pretty-print-print-hook
                                      (let ([prev (pretty-print-print-hook)])
                                        ;; tell `pretty-print' how to handle images, which is
                                        ;; by using `write-special':
                                        (lambda (v w? op)
                                          (if (convertible? v) (write-special v op) (prev v w? op)))))
                                     
                                     (error-display-handler
                                      (lambda (msg exn)
                                        (if (exn? exn)
                                            (display (get-rewriten-error-message exn) (current-error-port))
                                            (eprintf "uncaught exception: ~e" exn))))
                                     
                                     ((dynamic-require 'htdp/bsl/runtime 'configure)
                                      (dynamic-require reader 'options))))
       (call-in-sandbox-context me (lambda () (namespace-require module-lang)))
       (me 'exp) ...
       (interaction-eval #:eval me (require lang/posn))
       (interaction-eval #:eval me (require 2htdp/image))
       (interaction-eval #:eval me (require 2htdp/batch-io))
       me)]
    [(_ module-lang reader)
     (*sl-eval module-lang reader ())]))

(require lang/private/rewrite-error-message)

(define-syntax-rule
  (bsl-eval def ...)
  (*sl-eval 'lang/htdp-beginner 'htdp/bsl/lang/reader def ...))

(define-syntax-rule
  (bsl-eval+ def ...)
  (*sl-eval 'lang/htdp-beginner-abbr 'htdp/bsl+/lang/reader def ...))

(define-syntax-rule
  (isl-eval def ...)
  (*sl-eval 'lang/htdp-intermediate 'htdp/isl/lang/reader def ...))

(define-syntax-rule 
  (isl-eval+ def ...)
  (*sl-eval 'lang/htdp-intermediate-lambda 'htdp/isl/lang/reader def ...))

#;
(define-syntax-rule
  (bsl-eval-x=2 d ...)
  (*sl-eval 'lang/htdp-beginner 'htdp/bsl/lang/reader d ...))

#;
(define-syntax-rule
  (bsl-eval-x=2+ d ...)
  (*sl-eval 'lang/htdp-beginner-abbr 'htdp/bsl+/lang/reader d ...))

#;
(define-syntax-rule
  (isl-eval-x=2 d ...)
  (*sl-eval 'lang/htdp-intermediate 'htdp/isl/lang/reader d ...))

#;
(define-syntax-rule
  (isl-eval+-x=2 d ...)
  (*sl-eval 'lang/htdp-intermediate 'htdp/isl/lang/reader d ...))

;; -----------------------------------------------------------------------------

;; [Listof String] -> Blockquote
#;
(define (exercise . stuff)
  (make-blockquote 'exercise
                   (flow-paragraphs
                    (decode-flow (cons (bold "Exercise: ") stuff)))))
(require "Exercise/ex.ss")
(provide exercise exref Exref eop)

;; Style
(define inset (make-style 'inset '()))

;; [Listof String] -> Blockquote
(define (slogan . stuff)
  (make-nested-flow inset (list (make-paragraph (map italic stuff)))))

;; String *-> NestedFlow (inset)
(define (blockquote . str*)
  (make-nested-flow (make-style 'inset '())
                    (list (paragraph plain str*))))

;; [Listof String] -> Blockquote
(define (sample-problem . stuff)
  (make-nested-flow
   inset
   (flow-paragraphs (decode-flow (append (list (bold "Sample Problem: ")) stuff)))))

;; [Listof String] -> Blockquote
(define (domain . stuff)
  (make-nested-flow inset (flow-paragraphs (decode-flow (append (list (bold "Domain Knowledge: ")) stuff)))))

(define (block* . stuff)
  (make-blockquote 'blockquote (list (make-paragraph (map tt stuff)))))

;; Nat String -> Blockquote
(define (big-block h stuff)
  (define (flow* x) (make-flow (list x)))
  (make-blockquote 'blockquote
                   (list 
                    (make-table (make-with-attributes 'centered
                                                      `((cellspacing . "6")
                                                        (height . ,(number->string h))))
                                ;list
                                (list (list (flow* stuff)))))))

;; equation*[ [racket racket] ...]
(define-syntax (equation* stx)
  (syntax-case stx ()
    [(_ (left right) ...)
     #`(let ()
         (define (side x) (compound-paragraph plain (list (paragraph plain (list x)))))
         (table (eq-table 30) (map make-flow (list (list (side left) == (side right)) ...))))]))

;; equation[ racketblock racketblock ]
(define-syntax (equation stx)
  (syntax-case stx ()
    [(_ left right)
     #`(let ()
         (define (side x) (compound-paragraph plain (list x)))
         (table (eq-table 30) (map make-flow (list (list (side left) == (side right))))))]))

;; short-for[ racketblock racketblock ]
(define-syntax (short-for stx)
  (syntax-case stx ()
    [(_ left right)
     #`(let ()
         (define (side x) (compound-paragraph plain (list x)))
         ;; -- IN -- 
         (table (eq-table 90) (map make-flow (list (list (side left) short-txt (side right))))))]))

(define (eq-table w)
  (define space
    (style #f `(,(attributes `((width . "275") (align . "center") (valign . "bottom"))))))
  (define eqsp
    (style #f `(,(attributes `((width . ,(number->string w)) (align . "center") (valign . "top"))))))
  (style #f
         (list (attributes `((border . "0") (cellpadding . "0")))
               (table-columns (list space eqsp space)))))

(define == (paragraph plain (list (racket ==))))
(define short-txt (para "is short for"))

;; Schemeblock Schemeblock Schemeblock -> Blockquote
;; (Header H L R)
;;     H
;;  L     R
(define (header-table header left right)
  (make-table
   'centered
   #;
   '((row-styles
      ;; row 1
      ()
      ;; row 2
      ()))
   (list
    (list (make-flow (list header)) 'cont)
    (list (make-flow (list left)) (make-flow (list right))))))

;; orient stuff vertically in table 
;; stuff = [Listof X] [Listof Y] [Listof Z]
;; out   = X1 Y1 Z1 
;;         X2 Y2 Z2 
;;         X3 Y3 Z3 
;;         ...
(define (v-table . stuff)
  (define space
    (make-style #f (list (make-attributes '((width . "400") (align . "left") (valign . "top"))))))
  (core:make-table
   (make-style #f
               (list
                (make-attributes '((cellpadding . "1")))
                (make-table-columns (make-list (length stuff) space))))
   (apply map (compose make-flow list) stuff))
  #;
  (define tstuff (transpose stuff))
  #;
  (tabular
    #:sep @hspace[5]
    #:cell-properties (for/list ((t tstuff)) (for/list ((s t)) 'left))
    #:row-properties (for/list ((t tstuff)) 'top)
    tstuff))

;; Matrix -> Matrix 
(define (transpose stuff)
  (apply map list stuff))

(define (v-table/short . stuff)
  (define space
    (make-style #f (list (make-attributes '((align . "left") (valign . "top"))))))
  (core:make-table
   (make-style #f
               (list
                (make-attributes '((cellpadding . "1")))
                (make-table-columns (make-list (length stuff) space))))
   (apply map (compose make-flow list) stuff)))

;; orient stuff vertically in table 
;; stuff = [Listof X] [Listof Y] [Listof Z]
;; out   = X1 Y1 Z1 
;;         X2 Y2 Z2 
;;         X3 Y3 Z3 
;;         ...
(define (v-table* . stuff)
  (define space (make-style #f (list (make-attributes '((width . "500"))))))
  (define stuff* (map (lambda (s) (map p s)) stuff))
  (make-blockquote 
   'blockquote
   (list 
    (core:make-table
     (make-style 'boxed
                 (list
                  (make-attributes '((cellpadding . "1")))
                  (make-table-columns (make-list (length stuff) space))))
     (apply map (compose make-flow list) stuff*)))))

;; horizontally oriented table from stuff
;; stuff = [Listof X] [Listof Y] [Listof Z]
;; out   = X1 X2 X3 ...
;;         Y1 Y2 Y3 ...
;;         Z1 Z2 Z3 ...
(define (h-table #:border [border "0"] #:width [width '[width . "500"]] . stuff)
  (define space
    (make-style #f (list (make-attributes `((align . "left") ,width (valign . "top"))))))
  (core:make-table
   (make-style #f
               (list
                (make-attributes `((cellpadding . "1") (border . ,border)))
                (make-table-columns (make-list (length (first stuff)) space))))
   (let loop ([stuff stuff])
     (if (null? stuff)
         '()
         (cons (map make-flow (first stuff))
               (loop (rest stuff)))))))

;; Nat -> Table 
(define (world-table i)
  (define (cw i)
    (make-paragraph
     (list
      (scheme #,(string->symbol (string-append "cw" (number->string i)))))))
  (define (im i)
    (make-paragraph
     (list
      (scheme (render #,(string->symbol (string-append "cw" (number->string i))))))))
  (define ((hdl h) i)
    (make-paragraph
     (list
      (if (eq? h 'tock)
          (scheme (#,h  #,(string->symbol (string-append "cw" (number->string i)))))
          (scheme (#,h  #,(string->symbol (string-append "cw" (number->string i))) ...))))))
  (define (+... x l)
    (append (cons (make-paragraph x) l) (list (t "..."))))
  (h-table
   ;;   (+... (list "tick #") (build-list i (compose (lambda (x) (t x)) number->string add1)))
   (+... (list "current world") (build-list i cw))
   (+... (list "its image")     (build-list i im))
   (+... (list "on clock tick")   (build-list i (hdl 'tock)))
   (+... (list "on key stroke")   (build-list i (hdl 'ke-h)))
   (+... (list "on mouse event")  (build-list i (hdl 'me-h)))))

(define (rtable i)
  (h-table
   (cons (t "x = ") (build-list (+ i 1) (lambda (i) (t (number->string (+ i 1))))))
   (append
    (cons (t "y = ") (build-list i (lambda (x) (t (number->string (* (+ 1 x) (+ 1 x)))))))
    (list (t "?")))))

(define (mrtable i)
  (h-table
   (cons (t "x = ") (build-list (+ i 1) (lambda (i) (t (number->string (+ i 1))))))
   (append
    (cons (t "y = ") (build-list i (lambda (x) (t (img (format "Mrtable/i~a.png" (+ 1 x)))))))
    (list (t "?")))))

;; -----------------------------------------------------------------------------
;; images 
(define (img x) (image (string-append "../base/Images/" x)))
(define (img/p x) (image/plain (string-append "../base/Images/" x)))

(define (dr-world) (img "drscheme-world.png"))

(define (landing-interval) (img "landing-interval.png"))

(define-syntax-rule
  (def-int key)
  ;; ==>>
  (define (key) (img (format "~a.png" 'key))))

(def-int closed-closed)
(def-int open-closed)
(def-int closed-open)
(def-int open-open)

(define (bracket-down) (img "bracket-down.png"))
(define (bracket-up) (img "bracket-up.png"))

; (define (simple-mouse) (img/p "simple-mouse.png"))
; (define (simple-key x) (img/p (format "SimpleKeys/i~a.png" x)))
(define (car x) (img/p (format "car~a.png" x)))
(define (rocket) (img/p "rocket-s.jpg"))
(define (rocketsunk) (img/p "rocket-sunk.png"))
(define (red) (img "red-circle.png"))
(define (blue) (img "blue-rectangle.png"))
(define (bluesq) (img "blue-square.png"))
(define (redonblue) (img "red-on-blue.png"))
(define (green-scene) (img "green-scene.png"))
(define (rocketi i) (img (format "Mrockets/i~a.png" (+ i 1))))

(def-int traffic-real)
(def-int traffic-rep)

(define (tl-red) (img/p "traffic-red.png"))
(define (tl-green) (img/p "traffic-green.png"))
(define (tl-yellow) (img/p "traffic-yellow.png"))

(def-int door-real)
(def-int door-simu)

(def-int distance0)
(def-int math-distance0)
(def-int distanceM)

(def-int peterlee)
(def-int bobh)
(def-int 2dball)

(def-int dd0)
(def-int dd1)
(def-int dd-posn)
(def-int dd2-posn)
(def-int cat1)
(def-int cat2)
(def-int chameleon)
(def-int edit1)
(def-int edit2)

(def-int sigs-aim)
(def-int sigs-fired)
(def-int sigs-fired2)

(def-int grid-scene)

(def-int blocks1)
(def-int blocks2)
(def-int blocks3)

(def-int mercury)
(def-int venus)
(def-int earth)
(def-int list-boxes)
(def-int list-boxes2)

(def-int def-data-arrows)
(def-int def-data-mutual-arrows)
(def-int def-fun-arrows)
(def-int def-fun-mutual-arrows)

(def-int riot)

(def-int russian-doll-from-wikipedia)
(def-int rd)
(def-int rd-red)

(def-int quadratic)
(def-int quadratic+)
(def-int quadratic-)

(def-int editor-all-good)

(def-int triangle)
(def-int polygon)
(def-int polygon2)

(def-int worm1)
(def-int worm2)
(def-int worm3)

(def-int scope1)
(def-int scope2)
(def-int scope3)
(def-int scope4)
(def-int scope5)
(def-int scope6)
(def-int scope7)
(def-int scope8)
(def-int scope9)
(def-int xml-example)

(def-int distance-between)

(def-int family-tree)

(def-int dir-and-files)

(def-int ex-poly1)
(def-int ex-poly2)
(def-int ex-poly3)

(def-int ex-integrate)

(def-int newton-tangent-line)
(def-int newton-tangent-root)
(def-int newton-tangent-root0)
(def-int newton0)
(def-int newton1)
(def-int newton2)
(def-int newton3)

(def-int dagger)

(def-int eq)
(def-int gauss1)
(def-int gauss2)
(def-int gauss3)
(def-int bad-gauss3)
(def-int gauss4)
(def-int gauss5)
(def-int gauss6)
(def-int gauss7)
(def-int gauss8)
(def-int gauss9)
(def-int gaussA)
(def-int gaussB)
(def-int gaussC)
(def-int gaussD)
(def-int gaussE)
(def-int gaussF)
(def-int gaussG)
(def-int gaussGT)
(def-int gaussH)
(def-int gaussHY)
(def-int gaussHX)
(def-int gaussHXS)
(def-int gaussI)
(def-int gaussJ)
(def-int gaussK)
(def-int gaussL)
(def-int gaussM)
(def-int gaussN)



;; -----------------------------------------------------------------------------

(require (only-in 2htdp/image frame bitmap))

(define (web-page1) (frame (bitmap "../../base/Images/web-page1.png")))
(define (web-page2) (frame (bitmap "../../base/Images/web-page2.png")))

(define red-color (style #f (list (color-property "red"))))

;; ----------------------------------------

(provide intermezzo isection)

(define (intermezzo #:tag [t #f] 
                    #:unnumbered? [unnumbered? #f]
                    . c)
  (list
   (apply title #:style '(grouper unnumbered) #:tag t c)
   (section #:tag (and t (string-append "chap:" t))
            #:style (append
                     (if unnumbered? '(unnumbered) null)
                     '(hidden toc-hidden)))))

(define (isection #:tag [t #f] . c)
  (apply subsection #:tag t #:style 'unnumbered c))


(define (tabular-right-aligned tax-table #:sep [space (hspace 5)])
  (tabular
   #:sep (hspace 5)
   #:cell-properties
   (for/list ((row tax-table))
     (for/list ((cell row))
       'right))
   ;; the body of the table
   (map (lambda (row) (map list row)) tax-table)))

