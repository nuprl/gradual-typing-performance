#lang racket/base

;; Convert Python-generated data files to Racket

(require
  (only-in racket/date current-date)
  data/heap
  racket/set
  racket/string
  racket/port
)

;; =============================================================================

(define (read-string str)
  (with-input-from-string str read))

;; Returns (CFG NUM-TYPED SECONDS)
(define (read-line str)
  (read-string (string-append "(" str ")")))

(define line-key car)
(define line-num-types cadr)
(define line-avg-sec caddr)

(define (line<? l1 l2)
  ;; 0-0-0 is fully typed
  ;; N-N-N is fully untyped
  ;; Want to sort from untyped to typed, so use >
  (define x* (key-split (line-key l1)))
  (define y* (key-split (line-key l2)))
  (for/fold ([acc 'undef])
            ([x (in-list x*)]
             [y (in-list y*)])
    (cond
     [(boolean? acc)
      acc]
     [(> x y)
      #t]
     [(= x y)
      acc]
     [else
      #f])))

(define (key-split k)
  (map string->number (string-split (symbol->string k) "-")))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "rktd-of-txt"
   #:args (txt)
   (define H (make-heap line<?))
   (with-input-from-file txt
     (lambda ()
       (for ((ln (in-lines)))
         (heap-add! H (read-line ln)))))
   (with-output-to-file (string-append txt ".rktd") #:exists 'replace
     (lambda ()
       (printf ";; Generated from '~a' on '~a'\n" txt (current-date))
       (printf "#(\n")
       (for ((v (in-heap H)))
         (printf "(~a)\n" (line-avg-sec v)))
       (printf ")\n")))))
