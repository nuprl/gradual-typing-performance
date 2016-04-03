#lang racket

(provide
 keep-boundary?
 ;; syntax-object -> Boolean
 ;; given syntax-object containing the name of the required module,
 ;; return true if the boundary should be enabled.
 ;; Decides by checking the "override-all-except.rktd" file
 )
(require syntax/location)

(define (parse-overrides)
  (call-with-input-file "override-all-except.rktd"
    read))
(define (keep-boundary? m)
  (define them (syntax->datum m))
  (define us   (path->string (syntax-source-file-name m)))
  (unless (and (string? us) (string? them))
    (raise-arguments-error 'keep-boundary?
                           "expected to get strings in the end here"
                           "us" us
                           "them" them))
  (with-handlers ([exn:fail:filesystem? (λ (e) #t)])
    
    (define overrides (parse-overrides))
    (set-member? overrides (cons us them))))
