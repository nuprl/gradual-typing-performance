#lang racket/base

;; Supporting code for `typed-racket.scrbl`
;; - Render & organize benchmarks
;; - Make L-N/M figures

(provide
  benchmark-name*
  ;; (Listof Symbol)
  ;; Names of all benchmarks used in the paper

  NUM-BENCHMARKS
  ;; Natural
  ;; Not really a constant -- length of `benchmark-name*`

  (rename-out [make-benchmark benchmark])
  ;; (->* [] [#:name String
  ;;          #:author String
  ;;          #:num-adaptor Natural
  ;;          #:origin String
  ;;          #:purpose String
  ;;          #:external-libraries (Listof String)]
  ;;     #:rest (Listof String)
  ;;     Benchmark)

  benchmark-descriptions
  ;; (-> (Listof Benchmark) Any)
  ;; Render a list of Benchmark structures.
  ;; Use the `benchmark` constructor to make a `Benchmark`

  benchmark-characteristics
  ;; (-> Any)
  ;; 
)

(require
 racket/match
 scribble/core
 scribble/base
)

;; -----------------------------------------------------------------------------
;; -- Organizing the benchmarks

(define benchmark-name* '(
  forth
  fsm
  gregor
  kcfa
  lnm
  mbta
  morsecode
  quad
  sieve
  snake
  suffixtree
  synth
  tetris
  zombie
  zordoz
))
(define NUM-BENCHMARKS (length benchmark-name*))

;; -----------------------------------------------------------------------------

(struct benchmark (name author num-modules num-adaptor origin purpose lib* description))
(define (make-benchmark #:name name
                        #:author author
                        #:num-adaptor num-adaptor
                        #:origin origin
                        #:purpose purpose
                        #:external-libraries [lib* #f]
                        description)
  ;(define MG (project-name->modulegraph name))
  (define num-modules 0);(modulegraph->num-modules MG))
  (benchmark name author num-modules num-adaptor origin purpose lib* description))

(define (render-benchmark b)
  (match-define
    (benchmark name author num-modules num-adaptor origin purpose lib* description)
    b)
  (paragraph plain
   (list
    (elem #:style 'bold (symbol->string name))
    (element 'newline "")
    (list "Author : " author)
    (element 'newline "")
    (list "Origin : " origin)
    (element 'newline "")
    (list "Purpose : " purpose)
    (element 'newline "")
    (append
     (if lib*
       (list "External Libraries: " lib*
             (element 'newline ""))
       '())
     (list description)))))

(define (benchmark<? b1 b2)
  (< (benchmark-num-modules b1)
     (benchmark-num-modules b2)))

(define (missing-benchmark-error name*)
  (raise-user-error 'benchmark
                    "Missing descriptions for benchmark(s) '~a'" name*))

(define (unknown-benchmark-error name*)
  (raise-user-error 'benchmark
    "Got descriptions for unknown benchmarks '~a'. Register them at the top of 'typed-racket.rkt'" name*))

(define (check-missing-benchmarks b*)
  (define name* (map benchmark-name b*))
  (let loop ([expect* (sort benchmark-name* symbol<?)]
             [given*  (sort name* symbol<?)])
    (cond
     [(null? expect*)
      (if (null? given*)
        (void)
        (unknown-benchmark-error given*))]
     [(null? given*)
      (missing-benchmark-error expect*)]
     [else
      (define expect (car expect*))
      (define given (car given*))
      (cond
       [(eq? expect given)
        (loop (cdr expect*) (cdr given*))]
       [(symbol<? expect given)
        (missing-benchmark-error expect)]
       [else
        (unknown-benchmark-error given)])])))

(define (benchmark-descriptions . b*)
  (check-missing-benchmarks b*)
  (map render-benchmark (sort b* benchmark<?)))

(define (benchmark-characteristics)
  (elem "TODO"))

  ;(define MG (project-name->modulegraph MG))
  ;(define loc (modulegraph->untyped-loc MG))
  ;(define ann-loc (modulegraph->ann-loc MG))
  ;(define other-loc (modulegraph->other-loc MG))
  ;(define num-modules (modulegraph->num-modules MG))
