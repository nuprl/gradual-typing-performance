#lang racket/base

;; Given .rktd output from a traced execution,
;;  list all boundaries and the checks they acount for.
;; Prints results to console.

(provide
)

;; -----------------------------------------------------------------------------

(require
  (only-in "color-tikz.rkt"
    parse-data
    boundary-from
    boundary-to
    boundary-val
    boundary-checks)
  (only-in racket/match match-define)
)

;; =============================================================================

;; Aggregate boundary
(struct aggregate (
 from  ; String
 to    ; String
 vals  ; (Listof (Pairof String Natural))
 total ; Natural
) #:transparent)

;; Convert a list of boundaries into one `aggregate` representing all checks
(define (collapse-boundary* boundary*)
  (when (eq? '() boundary*)
    (error 'list-boundaries "Empty boundary list, cannot collapse"))
  (define from (boundary-from (car boundary*)))
  (define to   (boundary-to   (car boundary*)))
  (define-values (vals total)
    (for/fold ([vals '()]
               [total 0])
              ([b (in-list boundary*)])
      (define v (boundary-val b))
      (define c (boundary-checks b))
      (values (cons (cons v c) vals)
              (+ c total))))
  (aggregate from to vals total))

;; count all contracts represented by a list of `aggregate`
(define (count-contracts aggregate*)
  (for/sum ([a (in-list aggregate*)])
    (length (aggregate-vals a))))

;; count all checks represented by a list of `aggregate`
(define (count-checks aggregate*)
  (for/sum ([c (in-list aggregate*)])
    (aggregate-total c)))

;; Print a title for the file
(define (print-title fname total-contracts total-checks)
  (define s (format "Results for '~a'" fname))
  (define underline (make-string (string-length s) #\=))
  (displayln s)
  (displayln underline)
  (printf "  ~a contracts generated\n" total-contracts)
  (printf "  ~a total checks\n" total-checks))

;; Print a list of `aggregate` structs
(define (print-aggregate* aggregate* #:filename fname #:total-contracts total-contracts #:total-checks total-checks)
  (for ([a (in-list aggregate*)])
    (match-define (aggregate from to vals num-checks) a)
    (define num-contracts (length vals))
    (define pct-contracts (round (* 100 (/ num-contracts total-contracts))))
    (define pct-checks (round (* 100 (/ num-checks total-checks))))
    (when (< 10 pct-checks) ;(< 0 num-checks)
      (printf "* Boundary from '~a' to '~a' created ~a contracts (~a%) and caused ~a checks (~a%)\n" from to num-contracts pct-contracts num-checks pct-checks))))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "list-boundaries"
    #:args (FILE.rktd)
    (begin
      (define boundary** (parse-data FILE.rktd))
      (define aggregate* (map collapse-boundary* boundary**))
      (define total-contracts (count-contracts aggregate*))
      (define total-checks (count-checks aggregate*))
      (print-title FILE.rktd total-contracts total-checks)
      (print-aggregate* aggregate* #:filename FILE.rktd
                                   #:total-contracts total-contracts
                                   #:total-checks total-checks))))

;; =============================================================================

(module+ test
  (require rackunit)

)
