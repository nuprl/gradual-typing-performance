#lang racket/base

;; Combine an existing TiKZ picture with collected contract output
;; Produce a colored TiKZ picture, labeled with the number of contract checks

(provide
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

(define (ensure-tikz fname)
  (unless (ends-with? fname ".tex")
    (error 'color-tikz (format "Expected a TiKZ file, got '~a' on command line" fname))))

(define (parse-data fname)
  (unless (ends-with? fname ".rktd")
    (error 'color-tikz (format "Expected a .rktd file, got '~a' on command line" fname)))
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

(define (parse-id+fname line)
  (define tn (string->texnode line))
  (and tn
       (cons (texnode-id tn) (texnode-name tn))))

(define (line->from+to line #:map nat+fname*)
  (define te (string->texedge line))
  (define from (cdr (assoc (texedge-from te) nat+fname*)))
  (define to (cdr (assoc (texedge-to te) nat+fname*)))
  (values from to))

;; Read the data for all boundaries between `from` and `to`.
;; Return two values:
;; - the number of contracts between the pair
;; - and the total number of times these contracts were checked
(define (from+to->contracts+checks boundary** #:from from #:to to)
  (for/first ([boundary* (in-list boundary**)]
              #:when (and (filename=?/adapted from (boundary-from (car boundary*)))
                          (filename=?/adapted to   (boundary-to (car boundary*)))))
    (cons (length boundary*) (for/sum ([b (in-list boundary*)]) (boundary-checks b)))))

;; Sum up all the checks in all the boundaries
(define (count-all-checks boundary**)
  (for*/sum ([boundary* (in-list boundary**)]
             [b         (in-list boundary*)])
    (boundary-checks b)))

(define (percent->color pct)
  (define N (* 100 pct))
  (define colors '("green" "lime" "yellow" "orange" "red"))
  (define step (/ 100 (length colors)))
  (for/first ([c   (in-list colors)]
              [val (in-range step 101 step)]
              #:when (< N val))
    c))

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
      (define pct (/ checks total))
      (define new-line (string-replace tikz-line "->" (string-append "->," (percent->color pct))))
      (values nat+fname* new-line)]
     [else
      (printf "%% WARNING: no data for boundary '~a' ==> '~a'\n" from to)
      (values nat+fname* tikz-line)])]
   [else
    ;; Ignore the line
    (values nat+fname* tikz-line)]))

(define (format-title total)
  ;; TODO do something that works with tikz
  (format "%% Total checks = ~a\n" total))

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
     (ensure-tikz TIKZ.tex)
     (define boundary** (parse-data FILE.rktd))
     (printf "Parsed data...\n")
     (define total (count-all-checks boundary**))
     (printf "Writing to file '~a'...\n" (out-file))
     (with-output-to-file (out-file) #:exists 'replace ;; TODO
       (lambda ()
         (printf (format-title total))
         (with-input-from-file TIKZ.tex
           (lambda ()
             (for/fold ([nat+fname* '()])
                 ([t-line     (in-lines)])
               (define-values (new-map new-line)
                 (color-edge t-line boundary** #:total-checks total #:map nat+fname*))
               (displayln new-line)
               new-map))))))
   (void))
)
