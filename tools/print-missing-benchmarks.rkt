#lang racket/base

(require racket/format racket/set)

(define (natural->bitstring n #:pad pad-width)
  (~r n #:base 2 #:min-width pad-width #:pad-string "0"))

;; Convert a binary string to a natural number
(define (bitstring->natural str)
  (define N (string-length str))
  (for/sum ([i (in-range N)])
    (define c (string-ref str (- N (add1 i))))
    (if (equal? #\1 c)
        (expt 2 i)
        0)))

(define width 14)
(define hi (expt 2 14))
(define last-lo (box #f))
(define last (box #f))

(define (path->bits cfg)
  (cadr (regexp-match "configuration([01]*)" cfg)))

(module+ main
  (require racket/cmdline glob)
  (command-line
   #:args (path)
   (unless (and (string? path) (directory-exists? path))
     (raise-user-error 'missing "path-string?" path))
   (define width-box (box #f))
   (define have
     (for/set ([cfg (in-glob (string-append path "/benchmark/config*"))]
               #:when (not (null? (glob (string-append cfg "/*.rktd")))))
       (define bits (path->bits cfg))
       (unless (unbox width-box)
         (set-box! width-box (string-length bits)))
       (bitstring->natural bits)))
   (define width (unbox width-box))
   (define hi (expt 2 width))
   (define last-lo (box #f))
   (define last (box #f))
   (for ([i (in-range hi)]
         #:when (not (set-member? have i)))
     (cond
      [(not (unbox last-lo))
       (set-box! last-lo i)
       (set-box! last i)]
      [(= (+ 1 (unbox last)) i)
       (set-box! last i)]
      [else
       (let ((lo (unbox last-lo))
             (hi i))
         (printf "MISSING ~a -- ~a  (~a -- ~a)\n" (natural->bitstring lo #:pad width) (natural->bitstring hi #:pad width) lo hi)
         (set-box! last-lo i)
         (set-box! last i))]))))

