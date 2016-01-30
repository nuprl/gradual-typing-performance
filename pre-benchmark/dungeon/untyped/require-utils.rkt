#lang racket/base

;; TODO needs command-line interface, to rewrite the file
;; 

(require (for-syntax racket/base syntax/parse))

(define-syntax m:require
  (syntax-parser
   [(_ e*:expr ...)
    (printf "hello require\n")
    #`(begin
        (require racket/require)
        (begin
          #,(printf "(require (only-in ~s\n" (syntax-e #'e*))
          (require
            (filtered-in
              (lambda (name)
                (printf "  ~a\n" name)
                name)
              e*))
          #,(printf ")")) ...)]))

(provide
  (rename-out [m:require require]))
