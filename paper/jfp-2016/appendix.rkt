#lang racket/base

(provide
  annotated-bib
  render-annotated-bib
  (rename-out [make-module-description module-description])
  render-module-descriptions
  module
  render-pathologies
)

(require
  (only-in racket/list add-between)
  (only-in racket/string string-join)
  (only-in gtp-summarize/modulegraph
    modulegraph->untyped-loc
    modulegraph->typed-loc
    boundaries
    boundary-to
    boundary-from
    boundary-provided*)
  racket/serialize
  with-cache
  "benchmark.rkt"
  "common.rkt"
  "typed-racket.rkt"
)

;; =============================================================================

(define month*
  '( "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))

(struct abib [title author url date desc])

(define (abib<? ab1 ab2)
  (string<? (abib-date ab1)
            (abib-date ab2)))

(define (abib->elem ab)
  (list (elem (noindent) (emph (abib-title ab)) (exact ".~~") (abib-author ab) (exact ".~~") (abib-date ab)
              (exact "\n\n")
              (noindent 1.2) (smaller (url (abib-url ab))))
        #;(exact "\\vspace{-1ex}")
        #;(inset (abib-desc ab))))

(define (render-annotated-bib ab*)
  (cons (parag)
        (add-between (map abib->elem (sort ab* abib<?))
                     (parag))))

(define (annotated-bib #:title t #:author a #:url u #:date d . descr)
  (abib t a u d descr))

;; -----------------------------------------------------------------------------
(define MODULES-TABLE-TITLE* '(
  ""
  "Untyped LOC"
  "Ann. LOC"
  "Adaptor?"
  "\\# Imports"
  "\\# Exports"
))

(define (module-description<? m1 m2)
  (benchmark<? (car m1) (car m2)))

(define (module name has-adaptor? . descr)
  (list* name has-adaptor? descr))

(define (make-module-description bm #:scale [scale 1] #:cache? [cache? #f] . m*-unsorted)
  (define name (benchmark-name bm))
  (define m* (sort m*-unsorted string<? #:key car))
  (define tikz (benchmark->tex-file bm scale))
  (assert-module-names bm m*)
  (list* bm tikz cache? m*))

(define (assert-module-names bm m*)
  (define given-module-names (map car m*))
  (define infer-module-names (sort (benchmark->module-names bm) string<?))
  (unless (equal? given-module-names infer-module-names)
    (raise-user-error 'appendix "Expected '~a' modules do not match given.~n  Expected: ~a~n  Given: ~a" (benchmark-name bm) infer-module-names given-module-names)))

(define (render-module-description md)
  (define bm (car md))
  (define mg (benchmark-modulegraph bm))
  (define B (boundaries mg))
  (define tikz (cadr md))
  (define cache? (caddr md))
  (define m* (cdddr md))
  ;; -- print:
  ;;  benchmark-name
  (list (bold (symbol->string (benchmark-name bm)))
        (exact "\n\\vspace{-2ex}\n")
        (exact "\\begin{multicols}{2}\\begin{enumerate}\\setcounter{enumi}{-1}\n"
               (for/list ([m+a+d (in-list m*)])
                 (format "\\item {\\tt ~a}~n" (car m+a+d) #;(string-join (cddr m+a+d))))
               "\\end{enumerate}\\end{multicols}")
        (exact "\n\\vspace{-1ex}\n")
        (parameterize ([*use-cache?* #f])
          (render-table
            #:sep 0.5
            #:title MODULES-TABLE-TITLE*
            #:cache (cachefile (format "module-table-~a.rktd" (benchmark-name bm)))
            (lambda ()
              (for/list ([m+a+descr (in-list m*)]
                         [i (in-naturals)])
                (define m (car m+a+descr))
                (define uloc
                  (modulegraph->untyped-loc mg m))
                (define tloc
                  (modulegraph->typed-loc mg m))
                (define ada?
                  (cadr m+a+descr))
                (define num-imports
                  (for/sum ([b (in-list B)]
                            #:when (string=? m (boundary-to b)))
                    (length (boundary-provided* b))))
                (define num-exports
                  (for/sum ([b (in-list B)]
                            #:when (string=? m (boundary-from b)))
                    (length (boundary-provided* b))))
                (tex-row
                  (number->string i)
                  (number->string uloc)
                  (number->string (- tloc uloc))
                  (if ada? "\\checkmark" "")
                  (number->string num-imports)
                  (number->string num-exports))))))
        (exact "\n\\vspace{-1ex}\n")
        (centered tikz)
        (exact "\n\\vspace{2ex}\n")))

(define (module-description? m)
  (and (pair? m)
       (benchmark? (car m))))

(define (render-module-descriptions . m*)
  (define key
    (for/list ([m (in-list m*)])
      (if (module-description? m)
        (benchmark-name (car m))
        0)))
  (parameterize ([*current-cache-keys* (list (lambda () key))])
    (with-cache (build-path "cache" "cache-modulegraph-appendix.rktd")
      #:read deserialize
      #:write serialize
      #:fasl? #f
      (lambda ()
        (cons (noindent)
              (for/list ([m (in-list m*)])
                (if (module-description? m)
                  (render-module-description m)
                  m)))))))

(define (render-pathologies)
  (render-table new-pathologies
    #:title '("Benchmark" "Max Boundary" "Max Wraps" "Contract \\%" "Library \\%" "\\#GC")
    #:cache (build-path "cache" "cache-devils.rktd")))

(define (new-pathologies)
  (list
    ;;                        BM    Most.Freq.B  #|Most.Cost|# Most.Layr    C%  Lib.Time    nGC 
    (tex-row      "{\\tt sieve}"  "201,692,321"  #|      "X"|#       "0"  "59"       "-" "4,705")
    (tex-row      "{\\tt forth}"           "16"  #|      "X"|#       "3"  "50"       "-"    "15")
    (tex-row        "{\\tt fsm}"      "361,100"  #|      "X"|#    "4000"  "25"       "-" "2,329")
    (tex-row      "{\\tt fsmoo}"        "1,100"  #|      "X"|#       "2"  "93"       "-"   "856")
    (tex-row       "{\\tt mbta}"          "801"  #|      "X"|#       "2"  "27"      "23"    "38")
    (tex-row  "{\\tt morsecode}"      "821,060"  #|      "X"|#       "2"  "18"       "-"    "21")
    (tex-row     "{\\tt zombie}"       "13,502"  #|      "X"|#      "67"  "78"       "-"    "73")
    (tex-row    "{\\tt dungeon}"        "7,392"  #|      "X"|#       "2"  "84"       "-"    "28")
    (tex-row     "{\\tt zordoz}"      "116,167"  #|      "X"|#       "2"  "32"       "6"    "33")
    (tex-row        "{\\tt lnm}"       "13,064"  #|      "X"|#       "2"  "21"      "96"    "27")
    (tex-row "{\\tt suffixtree}"  "145,826,484"  #|      "X"|#       "2"  "95"       "-"   "572")
    (tex-row       "{\\tt kcfa}"       "70,670"  #|      "X"|#       "2"  "79"       "-" "8,485")
    (tex-row      "{\\tt snake}"   "18,856,600"  #|      "X"|#       "2"  "95"       "-"   "123")
    (tex-row      "{\\tt take5}"       "39,780"  #|      "X"|#       "3"  "66"       "-"    "42")
    (tex-row    "{\\tt acquire}"       "13,240"  #|      "X"|#       "3"  "40"       "-"    "20")
    (tex-row     "{\\tt tetris}"   "82,338,320"  #|      "X"|#       "2"  "89"       "-"   "212")
    (tex-row      "{\\tt synth}"      "445,637"  #|      "X"|#      "13"  "67"       "-"   "203")
    (tex-row     "{\\tt gregor}"      "925,521"  #|      "X"|#       "2"  "49"       "5"    "20")
    (tex-row     "{\\tt quadBG}"      "339,674"  #|      "X"|#     "390"  "60"       "0"    "69")
    (tex-row     "{\\tt quadMB}"      "281,030"  #|      "X"|#     "543"  "81"       "0"   "191")))

;; =============================================================================

(module+ test
  (require rackunit)

  (define sieve-desc
    '(("main" "main module") ("stream" "library module")))

  (test-case "assert-module-names"
    (check-equal?
      (assert-module-names sieve sieve-desc)
      (void))

    (check-exn #rx"appendix"
      (lambda () (assert-module-names sieve '())))
    (check-exn #rx"appendix"
      (lambda () (assert-module-names sieve (cons '("yo" "lo") sieve-desc)))))
)
