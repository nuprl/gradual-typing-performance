#lang racket/base

(require
  racket/string
  (only-in racket/match match-define)
  (only-in racket/port with-input-from-string)
  math/statistics
  racket/format
)

;; Parse contract-profile output, produce a table of results

(struct cparse (
  >symbol
  match?
) #:property prop:procedure
  (struct-field-index match?))

(define adaptor?
  (cparse 'adaptor
          (let ([r (regexp "adapt")])
            (lambda (ln)
              (regexp-match? r ln)))))

;; A most dangerous regexp, but works pretty often
(define codomain-only?
  (cparse 'codomain-only
          (let ([r (regexp "^\\(-> any/c")])
            (lambda (ln)
              (regexp-match? r ln)))))
    ;; ;; Get all things in the arrow, make sure all but the last are any/c
    ;; (let ([r (regexp "^\\(-> (.*?)\\) @")])
    ;;   (lambda (ln)
    ;;     (define m (regexp-match r ln))
    ;;     (and m
    ;;          (for/and ([d (in-list (cdr (reverse (string-split (cadr m)))))])
    ;;            (string=? d "any/c")))))))

(define domain-only?
  (cparse 'domain-only
    ;; Get the last thing from the arrow contract
    (let ([r1 (regexp "any/c\\) @")]
          [r2 (regexp "any\\) @")])
      (lambda (ln) (or (regexp-match? r1 ln) (regexp-match? r2 ln))))))

(define higher-order?
  (cparse 'higher-order
    (let ([r (regexp "^\\(-> (.*?)\\) @")])
      (lambda (ln)
        (define m (regexp-match r ln))
        (and m
             (for/or ([d (in-list (string-split (cadr m)))])
               (regexp-match? "->" d)))))))

;; Depends on the project, but "lib" catches built-in Racket libraries
(define library?
  (cparse 'library
          (let ([r (regexp "\\(lib ")])
            (lambda (ln)
              (regexp-match? r ln)))))

(define predicate?
  (cparse 'predicate
    (let ([r1 (regexp "^\\(-> any/c boolean\\?\\)")]
          [r2 (regexp "^\\(-> Any Boolean\\)")])
      (lambda (ln)
        (or (regexp-match? r1 ln)
            (regexp-match? r2 ln))))))

(define ALL-FILTERS (list adaptor? codomain-only? domain-only? higher-order? library? predicate?))

;; -----------------------------------------------------------------------------

;; Parse the %contract time from the first line of contract-profile output
(define (parse-c-percent ln)
  (define m (regexp-match (regexp "^Running time is ([0-9]*\\.?[0-9]*)% contracts$") ln))
  (unless m
    (raise-user-error "Could not parse %contract time from file"))
  (cadr m))

;; Parse the total contract/all time from the second line of contract-profile output
(define (parse-alltime ln)
  (map string->number (string-split (car (string-split ln)) "/")))

;; Create an empty "table" of results from a list of filters
(define (init-results filter* d)
  (for/list ([p (in-list filter*)])
    (cons (cparse->symbol p) d)))

;; Get the time (in ms) from a line
(define (parse-time ln)
  (define s (car (string-split ln)))
  (define v (string->number s))
  (unless (number? v)
    (raise-user-error (format "Could not parse time from line '~a'" ln)))
  v)

(define (done? ln)
  (string=? ln "BY CALLEE"))

(define (rnd n)
  (~r n #:precision '(= 2)))

(define (align s W)
  (~a s #:align 'right #:min-width W))

;; -----------------------------------------------------------------------------

(module+ main
  (require racket/cmdline)
  ;; --
  (define *A* (make-parameter #f))
  (define *C* (make-parameter #f))
  (define *D* (make-parameter #f))
  (define *H* (make-parameter #f))
  (define *L* (make-parameter #f))
  (define *P* (make-parameter #f))
  (define *output* (make-parameter #f))
  ;; --
  (command-line
   #:program "contract-profile-parser"
   #:once-each
   [("-a" "--adaptor")
     "Record the time spent in contracts from an adaptor module." (*A* #t)]
   [("-c" "--codomain-only")
     "Record the time spent in contracts with 'any' as their domain. (These are type contracts checked in a typed module.)" (*C* #t)]
   [("-d" "--domain-only")
     "Record the time spent in contracts with 'any' as their codomain. (These are type contracts checked in an untyped module.)" (*D* #t)]
   [("-f" "--higher-order")
     "Record the time spent in higher-order contracts" (*H* #t)]
   [("-l" "--library")
     "Record the time spent in library contracts" (*L* #t)]
   [("-p" "--predicate")
     "Record the time spent in predicate contracts" (*P* #t)]
   [("-o" "--output") o-param
     "Redirect output to this file" (*output* o-param)]
  #:args FILE*
   (begin
     (when (null? FILE*)
       (raise-user-error "Expected non-empty list of arguments"))
     ;; -- Choose which columns to track, based on input parameters
     (define filter* ;; By default, use all filters
       (let ([pre-filter (for/list ([use? (in-list (list (*A*) (*C*) (*D*) (*H*) (*L*) (*P*)))]
                                    [p (in-list ALL-FILTERS)] #:when use?) p)])
         (if (null? pre-filter) ALL-FILTERS pre-filter)))
     ;; -- Iterate over all input files
     (define result**
       (for/list ([file (in-list FILE*)])
         (unless (file-exists? file)
           (raise-user-error (format "File '~a' does not exist, cannot proceed" file)))
         ;; -- For all lines in the contract profiler file
         (with-input-from-file file
           (lambda ()
             ;; -- First, read the % running time due to contracts
             (define c-pct (parse-c-percent (read-line)))
             ;; -- And read the measured total time & total contract time
             (match-define (list c-time alltime)
                           (parse-alltime (read-line)))
             (list* c-time alltime
               (let loop ([result* (init-results filter* 0)])
                 (define ln (read-line))
                 (cond
                  [(done? ln)
                   result*]
                  [else
                   ;; -- Get the predicates this line matches, if any
                   (define match* (for/list ([p (in-list filter*)] #:when (p ln)) (cparse->symbol p)))
                   ;; -- Any matches? If so, get the time (on next line)
                   ;;    and attribute it to the filters
                   (cond
                    [(null? match*)
                     (loop result*)]
                    [else
                     ;; Add time to matching filters' counters
                     (define t (parse-time (read-line)))
                     (loop (for/list ([r (in-list result*)])
                             (if (member (car r) match*)
                                 (cons (car r) (+ (cdr r) t))
                                 r)))])])))))))
     ;; -- Got results for all files, aggregate them
     (define-values [ctime* alltime* col**]
       (for/fold ([ctime* '()]
                  [alltime* '()]
                  [col** (init-results filter* '())])
                 ([r* (in-list result**)])
         (values (cons (car r*) ctime*)
                 (cons (cadr r*) alltime*)
                 (for/list ([t+v* (in-list col**)]
                            [t2+v (in-list (cddr r*))])
                   (match-define (cons t v*) t+v*)
                   (match-define (cons t2 v) t2+v)
                   (unless (eq? t t2)
                     (raise-user-error (format "Tag mismatch! '~a' <> '~a'" t t2)))
                   (cons t (cons v v*))))))
     ;; -- Print
     (define TITLE ";; Data rows have mean,stddev,percent-of-contract-time,stderr")
     (define conclusions
       (list*
        (list 'total-time (rnd (mean alltime*))
                          (rnd (stddev alltime*)))
        (list 'contract-time (rnd (mean ctime*))
                             (rnd (stddev ctime*))
                             (rnd (mean (map (lambda (c t) (* 100 (/ c t))) ctime* alltime*)))
                             (rnd (stddev (map (lambda (c t) (* 100 (/ c t))) ctime* alltime*))))
        (for/list ([col (in-list col**)])
          (define v* (map (lambda (v c) (* 100 (/ v c))) (cdr col) ctime*))
          (list (car col) (rnd (mean (cdr col)))
                          (rnd (stddev (cdr col)))
                          (rnd (mean v*))
                          (rnd (stddev v*))))))
     (define row
       (list*
        (cons (mean (map (lambda (c t) (* 100 (/ c t))) ctime* alltime*))
              (stddev (map (lambda (c t) (* 100 (/ c t))) ctime* alltime*)))
        (for/list ([col (in-list col**)])
          (define v* (map (lambda (v c) (* 100 (/ v c))) (cdr col) ctime*))
          (cons (car col) (mean v*)))))
     (if (*output*)
         (with-output-to-file (*output*) #:exists 'replace
           (lambda () (pretty-print conclusions)))
         (pretty-print conclusions))
     (newline)
     (print-tex row))))

;; Requires all
(define (print-tex c*)
  (define col* '("C / R (S.E.)"
                 "adapt"
                 "hoc"
                 "lib"
                 "\\tt{(T->any)}"
                 "\\tt{(any->T)}"
                 "[ \\tt{(any->bool)} ]"))
  (define val*
    (for/list ([tag (map cparse->symbol (list adaptor?
                                              higher-order?
                                              library?
                                              domain-only?
                                              codomain-only?
                                              predicate?))])
      (or (findf (lambda (x) (eq? tag (car x))) (cdr c*))
          (raise-user-error 'print-tex (format "Cannot generate row, missing data for column '~e'" tag)))))
  (define mw (string-length "suffixtree"))
  ;; -- important, print the title
  (display (~a "Project" #:min-width (+ 2 (string-length "suffixtree"))))
  (display " & ")
  (for ([c (in-list col*)])
    (display c)
    (display " & "))
  (newline)
  (display (~a "X" #:min-width (+ 2 (string-length "suffixtree"))))
  (display " & ")
  (display (align (format "~a (~a)" (round (caar c*)) (rnd (cdar c*)))
                  (string-length (car col*))))
  (display " & ")
  (for ([c (in-list (cdr col*))]
        [v (in-list val*)])
    (display (align (round (cdr v))
                    (string-length c)))
    (display " & "))
  (newline))

;; Nicely format summary results
(define (pretty-print conclusions)
  ;; (write conclusions))
  (org-print conclusions))

(define (org-print c*)
  (displayln "|---")
  (displayln "| | MEAN | STD | %total | STE |")
  (displayln "|---")
  (let ([row (car c*)])
    (printf "| ~a | ~a | ~a | | |\n" (car row) (cadr row) (caddr row)))
  (print-row (cadr c*))
  (displayln "|---")
  (newline)
  (displayln "|---")
  (displayln "| c-type | MEAN | STD | %contract | STE |")
  (displayln "|---")
  (for ([r (in-list (cddr c*))])
    (print-row r))
  (displayln "|---"))

(define (print-row r)
  (display "|")
  (for ([v (in-list r)])
    (display " ")
    (display v)
    (display " |"))
  (newline))
