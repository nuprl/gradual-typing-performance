#lang racket

(provide
 keep-boundary?
 ;; syntax-object -> Boolean
 ;; given syntax-object containing the name of the required module,
 ;; return true if the boundary should be enabled.
 ;; Decides by checking the "override-all-except.rktd" file

 act-like-untyped?

 ;;
 ;; The override-all-except.rktd file should encode a set of pairs of filenames
 ;; that represent the *enabled* boundaries.
 ;; If the set is empty then all boundaries are disabled.
 ;; If the file is not present then all boundaries are enabled.
 ;; 
 ;; the first filename is the requiring module
 ;; the second is the required module
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

;; normal or add externals
(define (external-enabled?)
  (with-handlers ([exn:fail:filesystem? (λ (e) #t)])
    (define overrides (parse-overrides))
    (set-member? overrides 'external)))

;; add externals
(define (act-like-untyped?)
  (with-handlers ([exn:fail:filesystem? (λ (e) #f)])
    (define overrides (parse-overrides))
    (set-member? overrides 'external)))
