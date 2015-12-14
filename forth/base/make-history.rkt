#lang racket/base

(require (only-in racket/string string-join))

(define binop* '(+ - *))
(define other* '(dup drop over swap))
(define new* (box '()))

(define (random-ref xs)
  (list-ref xs (random (length xs))))

(define (random-def)
  (define cmd* (for/list ([_i (in-range (add1 (random 10)))])
                 (random-ref other*)))
  (define name (gensym (apply string-append (map symbol->string cmd*))))
  (set-box! new* (cons name (unbox new*)))
  (string-join (list* "define" (map symbol->string (cons name cmd*))) " "))

(define (print-random-command n)
  (if (< n 2)
    (begin (printf "push ~a\n" (random 9001))
           (+ 1 n))
    (case (random 5)
     [(0) (displayln (random-ref binop*))
          (- n 1)]
     [(1 2) (printf "push ~a\n" (random 9001))
            (+ 1 n)]
     [(2) (displayln (random-ref other*))
          n]
     [(3) (displayln (if (null? (unbox new*))
              (random-def)
              (random-ref (unbox new*))))
          n]
     [(4) (displayln (random-def))
          n])))

(module+ main
  (require racket/cmdline)
  (command-line
   #:args (N-str out-file)
   (define N (string->number N-str))
   (with-output-to-file out-file #:exists 'replace
     (lambda ()
       (for/fold ([size 0])
                 ([i (in-range N)])
         (print-random-command size))))
   (void)))
