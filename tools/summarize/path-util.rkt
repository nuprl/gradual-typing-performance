#lang typed/racket/base

(provide
  add-commas
  ; (-> Real String)

  assert-directory-exists
  ; (-> Path-String Void))

  assert-directory-exists*
  ; (-> (Listof Path-String) Void))

  ensure-dir
  ; (-> Path-String Void))
  ;; Check whether a directory exists.
  ;; If not, create it.

  split-list
  ; (All (A) (-> Natural (Listof A) (Listof (Listof A)))))

  integer->letter
  ; (-> Integer Char))
)

(require
  (only-in racket/string string-split string-join)
  (only-in racket/list split-at))

;; =============================================================================

(: add-commas (-> Real String))
(define (add-commas n)
  (define str (if (string? n) n (number->string n)))
  (define str* (string-split str "."))
  (string-append (add-commas/integer (car str*))
                 (if (or (null? (cdr str*)) (> (string-length str) 4))
                   ""
                   (string-append "." (cadr str*)))))

(: add-commas/integer (-> String String))
(define (add-commas/integer str)
  (define L (string-length str))
  (string-join
    (let loop : (Listof String)
              ([i   : Integer           L]
               [acc : (Listof String) '()])
      (let ([i-3 (- i 3)])
        (cond
         [(<= i-3 0)
          (cons (substring str 0 i) acc)]
         [else
          (loop i-3 (cons "," (cons (substring str i-3 i) acc)))]))) ""))

(: assert-directory-exists (-> Path-String Void))
(define (assert-directory-exists p)
  (unless (directory-exists? p)
    (raise-user-error 'assert-directory "Directory '~a' does not exist, cannot proceed" p)))

(: assert-directory-exists* (-> (Listof Path-String) Void))
(define (assert-directory-exists* p*)
  (for ([p (in-list p*)])
    (assert-directory-exists p)))

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
