#lang racket
(require (for-syntax
          racket/sandbox
          racket/list
          racket/syntax
          racket/require-transform
          racket/require-syntax))
(require racket/require-syntax)

(provide variations
         test-variation)

;; Example
#|
(require
  (variations  "Benchmark-Folders"
               "Variation"
               "tree-game.rkt"
               main
               16))
|#

(define-syntax (test-variation stx)
 (syntax-case stx ()
   [(_  n fn-name iterations arg)
    (let ()
     (define fn-id (format-id stx "~a:~a" (syntax->datum #'n) (syntax->datum #'fn-name)))
     #`(begin (printf "~a~a\n" "Variation: " n)
              (with-handlers ([exn:fail? (lambda (exn) (printf "~a\n" exn))])
                  (for ([i iterations])
                    (with-limits #f #f (time (#,fn-id arg)))))))]))

(define-syntax variations
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ root-folder 
           var-folder
           file-name
           function
           number)
        (values
         (map
          (lambda (n) 
            (make-import (format-id stx "~a:~a" n (syntax->datum #'function))
                         (syntax->datum #'function)
                         (string-append (syntax->datum #'root-folder) "/"
                                        (syntax->datum #'var-folder)  (number->string n) "/" 
                                        (syntax->datum #'file-name))
                         0 ; run time phase?
                         0
                         0
                         stx))
          (range (syntax->datum #'number)))
         null
         )]))))
