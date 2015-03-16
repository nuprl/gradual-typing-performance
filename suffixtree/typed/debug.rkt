#lang typed/racket/base
(require (for-syntax racket/base))


(provide debug-add-hooks)

;; Syntax for debugging.  
(define-syntax (debug-add-hooks stx)
  (syntax-case stx ()
    ((_ module-name (debug enable disable))
     (with-syntax
         ((module-name-string (datum->syntax
                               stx
                               (symbol->string
                                (syntax-e (syntax module-name))))))
       (syntax/loc stx
         (begin
           ;; Enable debugging hooks by calling (enable-debugging),
           ;; and disable them by (disable-debugging).

           (define debug-on?
             (make-parameter #f))
           (define (enable)
             (debug-on? #t))
           (define (disable)
             (debug-on? #f))

           (define-syntax (debug stx)
             (syntax-case stx ()
               ((_ format-string args (... ...))
                (syntax/loc stx
                  (void)))))
           ;; debugging is off at the moment
           #;(define-syntax (debug stx)
             (syntax-case stx ()
               ((_ format-string args (... ...))
                (syntax/loc stx
                  (if (debug-on?)
                      (printf (string-append module-name-string ": "
                                             format-string "~%")
                              args (... ...))
                      (void))))))))))))


