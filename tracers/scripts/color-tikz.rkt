#lang racket/base

;; Combine an existing TiKZ picture with collected contract output
;; Produce a colored TiKZ picture, labeled with the number of contract checks

(provide
  parse-data
  ;; (-> (U String (Listof String)) (Listof (Listof Boundary)))
  ;; Parse a data file into a list of lists of boundaries.
  ;; Each sub-list is the collected contracts crossing a pair of files.

  ;; A Boundary is a:
  ;; (Vector Symbol String String String Natural)
  boundary-from
  boundary-to
  boundary-val
  boundary-checks
  ;; -- getters for the boundary type
 )

(require
 (only-in racket/file file->value)
 (only-in racket/string string-replace string-trim)
 (only-in "trace-run.rkt" filename=?/adapted)
 "tikz-parser.rkt"
)

;; =============================================================================

(define (starts-with? str prefix)
  (and
    (<= (string-length prefix) (string-length str))
    (for/and ([c1 (in-string str)]
              [c2 (in-string prefix)])
      (char=? c1 c2))))

(define (ends-with? str suffix)
  (define offset (- (string-length str) (string-length suffix)))
  (and
    (not (negative? offset))
    (for/and ([c1 (in-string str offset)]
              [c2 (in-string suffix)])
      (char=? c1 c2))))

(define (ensure-tikz fname*)
  (or
   (for/first ([fname (in-list fname*)] #:when (ends-with? fname ".tex")) fname)
   (error 'color-tikz (format "Expected a TiKZ file, got '~a' on command line" fname*))))

(define (parse-data fname*)
  (define fname
    (or
     (and (string? fname*) fname*)
     (for/first ([fname (in-list fname*)] #:when (ends-with? fname ".rktd")) fname)
     (error 'color-tikz (format "Expected a .rktd file, got '~a' on command line" fname*))))
  ;; List of Lists of Boundary
  (for/list ([bs  (in-list (file->value fname))])
    (for/list ([b   (in-list bs)])
      ;; Not sure how to work with prefabs, so we're just doing struct->vector
      (struct->vector b))))

;; Ignore index 0, because it just says 'struct:boundary
(define (boundary-from v)
  (vector-ref v 1))
(define (boundary-to v)
  (vector-ref v 2))
(define (boundary-val v)
  (vector-ref v 3))
(define (boundary-checks v)
  (vector-ref v 4))

;; Get a file's name and its ID from a line of TiKZ
;; (: parse-id+fname (-> String (U #f (Pairof Natural String))))
(define (parse-id+fname line)
  (define tn (string->texnode line))
  (and tn
       (cons (texnode-id tn) (texnode-name tn))))

;; Parse a boundary (from/to pair) from a line of TiKZ
;; (: line->from+to (-> String #:map (Listof (Pairof Natural String)) (Values String String)))
(define (line->from+to line #:map nat+fname*)
  (define te (string->texedge line))
  (define from (cdr (assoc (texedge-from te) nat+fname*)))
  (define to (cdr (assoc (texedge-to te) nat+fname*)))
  (values (string-append from ".rkt")
          (string-append to ".rkt")))

;; Read the data for all boundaries between `from` and `to`.
;; Return two values:
;; - the number of contracts between the pair
;; - and the total number of times these contracts were checked
(define (from+to->contracts+checks boundary** #:from from #:to to)
  (for/first ([boundary* (in-list boundary**)]
              #:when (and (filename=?/adapted from (boundary-from (car boundary*)))
                          (filename=?/adapted to   (boundary-to (car boundary*)))))
    (cons (length boundary*) (for/sum ([b (in-list boundary*)]) (boundary-checks b)))))

;; Sum up all contracts created
(define (count-all-contracts boundary**)
  (for/sum ([boundary* (in-list boundary**)])
    (length boundary*)))

;; Sum up all the checks in all the boundaries
(define (count-all-checks boundary**)
  (for*/sum ([boundary* (in-list boundary**)]
             [b         (in-list boundary*)])
    (boundary-checks b)))

;; Generate a color from a the total number of checks (this is a simulated colormap)
;; #:total-checks should be the total number of contract checks across the program,
;;                this would be used to normalize colors, but as of (2015-08-06) we
;;                are just coloring by the number of digits
(define (checks->color num-checks #:total-checks [total-param #f])
  (define num-digits (string-length (number->string num-checks)))
  (for/first ([c   (in-list '("green!48!white" "yellow!45!orange" "blue!43!white" "purple!64!white" "red!87!black"))]
              [val (in-list '(2       4             6              7          #f))]
              [wth (in-list '(1       2             2.5            3          3.5))]
              #:when (or (not val) (< num-digits val)))
    (format "~a, line width=~apt" c wth)))

;; Process one line of TiKZ
;; - if the line is a \node, add the ID and filename to a map
;; - if the line is an edge, try to color it
;; - otherwise do nothing with the line
;; Return either the same line, or a colored version of the same
;; #:total-checks is the cumulative number of checks across the entire file
;; #:map associates TiKZ identifiers to filenames (an S.E. artifact of TiKZ)
(define (color-edge tikz-line boundary** #:total-checks total #:map nat+fname*)
  (define line (string-trim tikz-line))
  (cond
   [(and (starts-with? line "\\node") (parse-id+fname line))
    => (lambda (id+fname)
    ;; Extend the map, return the line
    (define id (car id+fname))
    (define fname (cdr id+fname))
    (values (cons (cons id fname) nat+fname*) tikz-line))]
   [(starts-with? line "\\draw")
    ;; Annotate the edge with a color
    (define-values (from to) (line->from+to line #:map nat+fname*))
    (define contracts+checks (from+to->contracts+checks boundary** #:from from #:to to))
    (cond
     [contracts+checks
      (define contracts (car contracts+checks))
      (define checks (cdr contracts+checks))
      (define new-line (string-replace tikz-line "->" (string-append "->," (checks->color checks #:total-checks total))))
      (values nat+fname* new-line)]
     [else
      (printf "%% WARNING: no data for boundary '~a' ==> '~a'\n" from to)
      (values nat+fname* tikz-line)])]
   [else
    ;; Ignore the line
    (values nat+fname* tikz-line)]))

(define (format-title total-contracts total-checks)
  ;; TODO print a title that'll appear in the tex
  (format "%% Total contracts = ~a\n%% Total checks = ~a\n" total-contracts total-checks))

;; =============================================================================

(define DEFAULT-OUTPUT "./tikz-colored.tex")

(module+ main
  (require racket/cmdline)
  (define out-file (make-parameter DEFAULT-OUTPUT))
  (command-line
   #:program "color-tikz"
   #:once-any
   [("-o" "--output") o-param "Output file" (out-file o-param)]
   #:args (FILE.rktd TIKZ.tex)
   (begin
     (define arg* (list FILE.rktd TIKZ.tex))
     (define tikz (ensure-tikz arg*))
     (define boundary** (parse-data arg*))
     (printf "Parsed data...\n")
     (define total-checks (count-all-checks boundary**))
     (define total-contracts (count-all-contracts boundary**))
     (printf "Writing to file '~a'...\n" (out-file))
     (with-output-to-file (out-file) #:exists 'replace ;; TODO
       (lambda ()
         (printf (format-title total-contracts total-checks))
         (with-input-from-file tikz
           (lambda ()
             (for/fold ([nat+fname* '()])
                 ([t-line     (in-lines)])
               (define-values (new-map new-line)
                 (color-edge t-line boundary** #:total-checks total-checks #:map nat+fname*))
               (displayln new-line)
               new-map))))))
   (void))
)
