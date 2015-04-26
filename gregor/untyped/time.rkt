#lang racket/base

;; Working with Time objects

(provide;/contract
 Time?
 time?           ;(-> any/c boolean?)]
 time            ;(->i ([hour (integer-in 0 23)])
                 ;      ([minute (integer-in 0 59)]
                 ;       [second (integer-in 0 59)]
                 ;       [nanosecond (integer-in 0 (sub1 NS/SECOND))])
                 ;      [t time?])]
 time->hmsn     ; (-> time? HMSN?)]
 time->ns       ; (-> time? (integer-in 0 (sub1 NS/DAY)))]
 day-ns->time   ; (-> (integer-in 0 (sub1 NS/DAY)) time?)]
 time->iso8601  ; (-> time? string?)]
 time=?         ; (-> time? time? boolean?)]
 time<?         ; (-> time? time? boolean?)]
 time<=?        ; (-> time? time? boolean?)]
 time>?         ; (-> time? time? boolean?)]
 time>=?        ; (-> time? time? boolean?)]
 time-order     ; order?]
 MIDNIGHT       ; time?]
 NOON           ; time?])
 Time
)

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  (only-in racket/format ~r)
  "structs.rkt"
  "compare.rkt"
  racket/match)
(require (only-in
  "hmsn.rkt"
    hmsn->day-ns ;(-> HMSN Natural)]
    day-ns->hmsn ;(-> Natural HMSN)]
    NS/SECOND ;Natural]
))

;; =============================================================================

;(: time-equal-proc (-> Time Time Boolean))
(define (time-equal-proc x y)
  (= (Time-ns x) (Time-ns y)))

;(: time-hash-proc (-> Time (-> Natural Integer) Integer))
(define (time-hash-proc x fn)
  (fn (Time-ns x)))

;(: time-write-proc (-> Time Output-Port Any Void))
(define (time-write-proc t out mode)
  (fprintf out "#<time ~a>" (time->iso8601 t)))

(struct Time (hmsn ;: HMSN]
              ns ;: Natural]))
))
  ;; #:methods gen:equal+hash
  ;; [(define equal-proc time-equal-proc)
  ;;  (define hash-proc  time-hash-proc)
  ;;  (define hash2-proc time-hash-proc)]

  ;; #:methods gen:custom-write
  ;; [(define write-proc time-write-proc)]

  ;; #:property prop:serializable
  ;; (make-serialize-info (λ (t) (vector (time->ns t)))
  ;;                      #'deserialize-info:Time
  ;;                      #f
  ;;                      (or (current-load-relative-directory)
  ;;                          (current-directory))))

;(: time? (-> Any Boolean))
(define time? Time?)

;(: time->hmsn (-> Time HMSN))
(define time->hmsn Time-hmsn)
;(: time->ns (-> Any Natural))
(define (time->ns t)
  (unless (Time? t) (error "time->ns: type error"))
  (Time-ns t))

;(: hmsn->time (-> HMSN Time))
(define (hmsn->time hmsn)
  (Time hmsn (hmsn->day-ns hmsn)))

;(: day-ns->time (-> Natural Time))
(define (day-ns->time ns)
  (Time (day-ns->hmsn ns) ns))

;(: time (->* (Integer) (Integer Integer Integer) Time))
(define (time h [m 0] [s 0] [n 0])
  (hmsn->time (HMSN h m s n)))

;(: time->iso8601 (-> Time String))
(define (time->iso8601 t)
  ;(: f (-> Integer Natural String))
  (define (f n l) (~r n #:min-width l #:pad-string "0"))
  
  (match-define (HMSN h m s n) (time->hmsn t))
  (define fsec (+ s (/ n NS/SECOND)))
  (define pad (if (>= s 10) "" "0"))

  (format "~a:~a:~a~a" (f h 2) (f m 2) pad (~r fsec #:precision 9)))

(match-define (comparison time=? time<? time<=? time>? time>=? time-comparator time-order)
  (build-comparison 'time-order time? time->ns))

;; (define deserialize-info:Time
;;   (make-deserialize-info
;;    day-ns->time
;;    (λ () (error "Time cannot have cycles"))))

;; (module+ deserialize-info
;;   (provide deserialize-info:Time))

;(: MIDNIGHT Time)
(define MIDNIGHT (time 0))
;(: NOON Time)
(define NOON     (time 12))
