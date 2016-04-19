#lang typed/racket/base

(provide:
  (ensure-dir
   (-> Path-String Void))
  ;; Check whether a directory exists.
  ;; If not, create it.

  (split-list
   (All (A) (-> Natural (Listof A) (Listof (Listof A)))))

  (integer->letter
   (-> Integer Char))
)

(require
  (only-in racket/list split-at))

;; =============================================================================

(: ensure-dir (-> Path-String Void))
(define (ensure-dir path)
  (unless (directory-exists? path)
    (make-directory path)))

(: split-list (All (A) (-> Natural (Listof A) (Listof (Listof A)))))
(define (split-list n x*)
  (cond
   [(<= n 0)
    (raise-user-error 'split-list "Invalid partition size ~a" n)]
   [(null? x*)
    '()]
   [else
    (let loop : (Listof (Listof A))
              ([x* : (Listof A) x*]
               [L : Integer (length x*)])
      (if (< L n)
        (list x*)
        (let-values (((l* r*) (split-at x* n)))
          (cons l* (if (null? r*) '() (loop r* (- L n)))))))]))

(: integer->letter (-> Integer Char))
(define (integer->letter i)
  (integer->char (+ (char->integer #\a) i -1)))

;; =============================================================================

(module+ test
  (require typed/rackunit)

  (define-syntax-rule (check-apply* f [arg* ... => r] ...)
    (begin (check-equal? (f arg* ...) r) ...))

  (check-apply* split-list
   [1 '()
    => '()]
   [1 '(1 2 3)
    => '((1) (2) (3))]
   [2 '(1 2 3)
    => '((1 2) (3))]
   [3 '(9 9 9 9 9 9 9 9 9)
    => '((9 9 9) (9 9 9) (9 9 9))])

  (check-exn #rx"split-list"
    (lambda () (split-list 0 '(1 2 3))))

)
