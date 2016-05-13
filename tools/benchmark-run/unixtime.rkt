#lang racket/base

(provide
  (struct-out unixtime)
  TIME-FMT
  TIME-RX
  unixtime*->min
  string->unixtime
)

(require
  racket/string
  racket/match
)

;; -----------------------------------------------------------------------------

(define TIME-FMT
  "'TIME %e real    %U user    %S sys    %M max-kbytes    %D udata    %p ustack    %c ictx    %w vctx    %x exit'")

(define TIME-RX
  #rx"^TIME ([0-9:.]+) real    ([0-9.]+) user    ([0-9.]+) sys    ([0-9]+) max-kbytes    ([0-9]+) udata    ([0-9]+) ustack    ([0-9]+) ictx    ([0-9]+) vctx    ([0-9]+) exit$")

(struct unixtime (
  real        ;; Elapsed real (wall clock) time used by the process, in milliseconds
  user        ;; Total number of CPU-seconds that the process used directly (in user mode), in seconds.
  sys         ;; Total number of CPU-seconds used by the system on behalf of the process (in kernel mode), in seconds.
  max-kbytes  ;; Maximum resident set size of the process during its lifetime, in Kilobytes.
  udata       ;; Average size of the process's unshared data area, in Kilobytes.
  ustack      ;; Average unshared stack size of the process, in Kilobytes.
  ictx        ;; Number of times the process was context-switched involuntarily (because the time slice expired).
  vctx        ;; Number of times that the program was context-switched voluntarily, for instance while waiting for an I/O operation to complete.
  exit        ;; Process exit code
) #:prefab )

(define (string->unixtime str)
  (cond
   [(regexp-match TIME-RX str)
    => (lambda (m)
      (apply unixtime (cons (real-time->milliseconds (cadr m)) (map string->number (cddr m)))))]
   [(string-prefix? str "TIME")
    (raise-user-error 'yolo "strin->unix failed ~a" str)]
   [else
    #f]))

(define (real-time->milliseconds str)
  (define-values (h-str m-str s+ms-str)
    (match (string-split str ":")
     [(list h-str m-str s+ms-str)
      (values h-str m-str s+ms-str)]
     [(list m-str s+ms-str)
      (values "0" m-str s+ms-str)]
     [(list s+ms-str)
      (values "0" "0" s+ms-str)]))
  (match-define (list s-str ms-str) (string-split s+ms-str "."))
  (+ (hours->milliseconds (string->number h-str))
     (minutes->milliseconds (string->number m-str))
     (seconds->milliseconds (string->number s-str))
     (string->number ms-str)))

(define (hours->milliseconds h)
  (minutes->milliseconds (* 60 h)))

(define (minutes->milliseconds m)
  (seconds->milliseconds (* 60 m)))

(define (seconds->milliseconds s)
  (* 1000 s))

(define (unixtime*->min ut*)
  (apply min (map unixtime-real ut*)))
