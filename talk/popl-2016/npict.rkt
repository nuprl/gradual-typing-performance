#lang racket

;; Library for high-level pict construction

(require (except-in slideshow/pict arrow)
         unstable/gui/pict)

(provide
 (contract-out
  [npict (->* () (#:background-color string?)
              #:rest (listof (or/c line? node/c))
              pict?)]
  [align (-> node-name/c symbol? align?)]
  ;; avoid conflict with ppict
  (rename make-coord n:coord
          (->* (real? real?)
               (align/c)
               coord?))
  (rename make-node node
          (->* ()
               (#:at location/c
                #:name node-name/c
                #:pict pict?
                #:style style/c
                #:text string?)
               node?))
  (rename make-line line line/c)
  (rename arrow n:arrow procedure?)
  (rename make-style style
          (->* ()
               (#:color string?
                #:text-color string?
                #:background-color string?)
               style?))))

;;; Data definitions

;; a NodeName is one of
;;  - Symbol
;;  - String
(define node-name/c (or/c string? symbol?))

;; a Location is one of
;;  - NodeName
;;  - (align NodeName Symbol)
;;  - (coord Number Number [Symbol])
(struct align (name align))
(struct coord (x y align))

(define (make-coord x y [align 'cc])
  (coord x y align))

;; a Style is a (make-style Maybe<String> String Maybe<String>)
(struct style (color text-color background-color))

(define (make-style #:color [color #f]
                    #:text-color [text-color "black"]
                    #:background-color [background-color #f])
  (style color text-color background-color))

;; a Node is a (make-node Location 
;;                        Maybe<Symbol> Maybe<Pict>
;;                        Maybe<String> Maybe<Style>)
(struct node (loc name pict text style))

(define (make-node #:at [loc (coord 0 0 'cc)]
                   #:name [name #f]
                   #:pict [pict #f]
                   #:style [style #f]
                   #:text [text #f])
  (node loc name pict text style))

;; Line - for drawing between nodes
(struct line (from to arrow? label
              start-align end-align
              start-angle end-angle
              start-pull end-pull
              line-width color style under?
              solid?
              ;; only used with label
              x-adjust y-adjust))

(define (make-line #:from from
                   #:to to
                   #:label [label #f]
                   #:arrow? [arrow? #f]
                   #:start-angle [start-angle #f]
                   #:end-angle [end-angle #f]
                   #:start-align [start-align 'cc]
                   #:end-align [end-align 'cc]
                   #:start-pull [start-pull 1/4]
                   #:end-pull [end-pull 1/4]
                   #:line-width [line-width #f]
                   #:color [color #f]
                   #:style [style 'solid]
                   #:under? [under? #f]
                   #:solid? [solid? #t]
                   #:x-adjust [x-adjust 0]
                   #:y-adjust [y-adjust 0])
  (line from to arrow? label start-align end-align start-angle
        end-angle start-pull end-pull line-width color style
        under? solid? x-adjust y-adjust))

(define (arrow [size 10]
               #:from from
               #:to to
               #:label [label #f]
               #:start-angle [start-angle #f]
               #:end-angle [end-angle #f]
               #:start-align [start-align 'cc]
               #:end-align [end-align 'cc]
               #:start-pull [start-pull 1/4]
               #:end-pull [end-pull 1/4]
               #:line-width [line-width #f]
               #:color [color #f]
               #:style [style 'solid]
               #:under? [under? #f]
               #:solid? [solid? #t]
               #:x-adjust [x-adjust 0]
               #:y-adjust [y-adjust 0])
  (line from to size label start-align end-align start-angle
        end-angle start-pull end-pull line-width color style
        under? solid? x-adjust y-adjust))

;;; Contracts
(define align/c 
  (one-of/c 'lt 'ct 'rt 'lc 'cc 'rc 'lb 'cb 'rb))

(define location/c 
  (or/c node-name/c (struct/c align node-name/c symbol?) coord?))

(define style/c
  (struct/c style (or/c #f string?) (or/c #f string?) (or/c #f string?)))

(define node/c
  (struct/c node location/c 
            (or/c #f node-name/c) (or/c #f pict?)
            (or/c #f string?) (or/c #f style/c)))

(define line/c
  (->* (#:from (or/c coord? node-name/c pict?)
        #:to (or/c coord? node-name/c pict?))
       (#:arrow? any/c #:label (or/c #f pict?)
        #:start-angle real? #:end-angle real?
        #:start-align align/c #:end-align align/c
        #:start-pull real? #:end-pull real? #:line-width real?
        #:color string? #:under? any/c #:solid? any/c
        #:style (or/c 'transparent 'solid 'xor 'hilite
                      'dot 'long-dash 'short-dash 'dot-dash
                      'xor-dot 'xor-long-dash 'xor-short-dash
                      'xor-dot-dash)
        #:x-adjust number? #:y-adjust number?)
       line?))

;; listof<Node> -> Pict
(define (npict #:background-color [background-color #f]
               . clauses)
  
  ;; sort into nodes, lines, and so on
  (define nodes (filter node? clauses))
  (define lines (filter line? clauses))
  
  ;; build a dict mapping node names to coordinates
  (define name-mapping
    (for/hash ([n nodes]
               #:when (node-name n))
      (when (not (coord? (node-loc n)))
        (error "Nodes can't have both a named location and a name"))
      (values (node-name n) (node-loc n))))
  
  ;; a map for finders, for use with arrows and lines
  (define finder-mapping
    (hash 'lt lt-find 'ct ct-find 'rt rt-find
          'lc lc-find 'cc cc-find 'rc rc-find
          'lb lb-find 'cb cb-find 'rb rb-find))
  
  ;; -> coord?
  ;; given a node, extract its coordinates (or consult name table)
  (define (get-coord n)
    (define loc (node-loc n))
    (match loc
      [(? node-name/c) (dict-ref name-mapping loc)]
      [(struct align (name align))
       (define c (dict-ref name-mapping name))
       (coord (coord-x c) (coord-y c) align)]
      [else loc]))
  
  ;; to build a pict for each node
  (define (draw-one n)
    (define sty (or (node-style n) (style #f #f #f)))
    ;; get the base pict for this node, or start with blank
    (define base-node-pict (or (node-pict n) (blank 0 0)))
    ;; build a text pict
    (define text-pict (text (or (node-text n) "")))
    ;; color the text if necessary
    (define colored-text
      (if (style-text-color sty)
          (colorize text-pict (style-text-color sty))
          text-pict))
    ;; stick the text (if present) on the base pict
    (define pict-with-text
      (cond [(and (node-text n) base-node-pict)
             (cc-superimpose colored-text base-node-pict)]
            [else base-node-pict]))
    ;; if a background color is needed, add a backdrop
    (define pict-with-backdrop
      (if (style-background-color sty)
          (backdrop pict-with-text #:color (style-background-color sty))
          pict-with-text))
    pict-with-backdrop)
  
  ;; pict-offsets : pict? pos? -> (values int? int?)
  ;; find the offsets used to draw or size the scene
  (define (pict-offsets p pos)
    (define w (pict-width p))
    (define h (pict-height p))
    (case pos
      [(lt) (values 0 h)]
      [(ct) (values (/ w 2) h)]
      [(rt) (values w h)]
      [(lc) (values 0 (/ h 2))]
      [(cc) (values (/ w 2) (/ h 2))]
      [(rc) (values w (/ h 2))]
      [(lb) (values 0 0)]
      [(cb) (values (/ w 2) 0)]
      [(rb) (values w 0)]))

  ;; define fake nodes for all the coordinates that are
  ;; used for lines in this npict. these nodes are *only*
  ;; used for size calculation purposes
  ;;
  ;; lines* contains new lines where coords are turned into picts
  (define-values (line-nodes lines*)
    (for/lists (new-nodes new-lines)
               ([a-line lines])
      (define from (line-from a-line))
      (define to (line-to a-line))
      ;; construct dummy picts so lines can be drawn later
      (define from-pict (blank 1 1))
      (define to-pict (blank 1 1))
      (cond [(and (coord? from) (coord? to))
             (values (list (make-node #:at from #:pict from-pict)
                           (make-node #:at to #:pict to-pict))
                     (struct-copy line a-line [from from-pict] [to to-pict]))]
            [(coord? from)
             (values (make-node #:at from #:pict from-pict)
                     (struct-copy line a-line [from from-pict]))]
            [(coord? to)
             (values (make-node #:at to #:pict to-pict)
                     (struct-copy line a-line [to to-pict]))]
            [else (values #f a-line)])))
  
  ;; flatten since this could have two-length lists in it too
  (define line-nodes* (flatten (filter values line-nodes)))

  ;; the nodes should now also contain the fake nodes
  (define nodes* (append nodes line-nodes*))

  ;; the picts that we'll draw on the final picture
  (define picts (map draw-one nodes*))

  ;; mapping of node names to picts
  (define pict-mapping
    (for/hash ([n nodes*] [p picts]
               #:when (node-name n))
      (values (node-name n) p)))

  ;; first figure out the size of the base pict
  (define-values (xp xn yp yn)
    (for/fold ([x-pos-max 0] [x-neg-max 0]
               [y-pos-max 0] [y-neg-max 0])
              ([n nodes*])
      ;; p is the pict if the given node was drawn
      (define p (draw-one n))
      (define w (pict-width p))
      (define h (pict-height p))
      (define c (get-coord n))
      (define x (coord-x c))
      (define y (coord-y c))
      (define-values (dx dy) (pict-offsets p (coord-align c)))
      (values (max x-pos-max (+ x (- w dx)))
              (min x-neg-max (- x dx))
              (max y-pos-max (+ y (- h dy)))
              (min y-neg-max (- y dy)))))
  
  (define-values (w h) (values (+ xp (- xn)) (+ yp (- yn))))
  
  ;; gets the translated coords for each node
  ;; TODO: simplify this calculation
  (define (draw-coords n p)
    (define c (get-coord n))
    (define w (pict-width p))
    (define h (pict-height p))
    (define x (coord-x c))
    (define y (coord-y c))
    (define-values (dx dy) (pict-offsets p (coord-align c)))
    (values (if (> x 0)
                (- (+ (- xn) x) dx)
                (- (- (- xn) (- x)) dx))
            (if (> y 0)
                (- (- yp y) (- h dy))
                (- (+ yp (- y)) (- h dy)))))
  
  ;; then draw on a blank pict of the right size
  (define background-pict
    (if background-color
        (colorize (filled-rectangle w h) background-color)
        (blank w h)))
  (define initial-pict
    (for/fold ([p background-pict])
              ([pict-for-node picts] [n nodes*])
      (define-values (x y) (draw-coords n pict-for-node))
      (if pict-for-node
          (pin-over p x y pict-for-node)
          p)))
  
  ;; then draw the arrows on the image
  (define pict-with-arrows
    (for/fold ([pict initial-pict])
              ([current-line lines*])
      (match-define
        (struct line 
          (from to arrow? label
           start-align end-align
           start-angle end-angle
           start-pull end-pull
           line-width color style under?
           solid?
           x-adjust y-adjust))
        current-line)
      (define line-function
        (cond [(and arrow? label) pin-arrow-label-line]
              [arrow? pin-arrow-line]
              [label pin-label-line]
              [else pin-line]))
      (define from-pict
        (if (pict? from)
            from
            (hash-ref pict-mapping from)))
      (define to-pict
        (if (pict? to)
            to
            (hash-ref pict-mapping to)))
      (define from-find (hash-ref finder-mapping start-align))
      (define to-find (hash-ref finder-mapping end-align))
      (keyword-apply 
       line-function
       `(#:color #:end-angle #:end-pull #:line-width
         ,@(if arrow? '(#:solid?) '())
         #:start-angle #:start-pull #:style #:under?
         ,@(if label '(#:x-adjust #:y-adjust) '()))
       `(,color ,end-angle ,end-pull ,line-width
         ,@(if arrow? (list solid?) '())
         ,start-angle ,start-pull ,style ,under?
         ,@(if label (list x-adjust y-adjust) '()))
       `(,@(if label (list label) (list))
         ,@(if arrow?
               (if (number? arrow?) (list arrow?) (list 5))
               (list))
         ,pict ,from-pict ,from-find ,to-pict ,to-find))))
  
  ;; final image
  pict-with-arrows)
      
