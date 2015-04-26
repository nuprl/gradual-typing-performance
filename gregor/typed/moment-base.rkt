#lang typed/racket/base

;; Support for moment.rkt
;; (Works together with offset-resolvers.rkt)

(provide
 moment?
 moment->iso8601
 moment->iso8601/tzid
 make-moment
)

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  racket/match
  "gregor-adapter.rkt"
  (only-in racket/format ~r)
)
(require/typed/check "datetime.rkt"
    [datetime->iso8601 (-> DateTime String)]
)

;; =============================================================================

(: moment-equal-proc (-> Moment Moment Boolean))
(define (moment-equal-proc x y)
  (match* (x y)
    [((Moment d1 o1 z1) (Moment d2 o2 z2))
     (and (equal? d1 d2)
          (= o1 o2)
          (equal? z1 z2))]))

(: moment-hash-proc (-> Moment (-> Any Integer) Integer))
(define (moment-hash-proc x fn)
  (match x
    [(Moment d o z)
     (bitwise-xor (fn d) (fn o) (fn z))]))

(: moment-write-proc (-> Moment Output-Port Any Void))
(define (moment-write-proc m out mode)
  (fprintf out
           "#<moment ~a>"
           (moment->iso8601/tzid m)))

(: moment->iso8601/tzid (-> Moment String))
(define (moment->iso8601/tzid m)
  (: iso String)
  (define iso (moment->iso8601 m))
  (match m
    [(Moment _ _ z) #:when z (format "~a[~a]" iso z)]
    [_ iso]))

(: moment->iso8601 (-> Moment String))
(define (moment->iso8601 m)
  (match m
    [(Moment d 0 _)
     (string-append (datetime->iso8601 d) "Z")]
    [(Moment d o _)
     (define sign (if (< o 0) "-" "+"))
     (define sec  (abs o))
     (define hrs  (quotient sec 3600))
     (define min  (quotient (- sec (* hrs 3600)) 60))
     (format "~a~a~a:~a"
             (datetime->iso8601 d)
             sign
             (~r hrs #:min-width 2 #:pad-string "0" #:sign #f)
             (~r min #:min-width 2 #:pad-string "0" #:sign #f))]))

(: moment? (-> Any Boolean))
(define moment? Moment?)

(: make-moment (-> DateTime Integer (U String #f) Moment))
(define (make-moment dt off z)
  (Moment dt off (and z (string->immutable-string z))))
