#lang racket

(provide
 ;; from unstable/contract
 maybe/c
 
 ;; SYNTAX
 ;; (==> a b) means (if a b #t)
 ==> 

 ;; SYNTAX
 ;; (interface name f-optional-with-ctc ...)
 ;; specifies a module interface with contracts 
 ;; 'name' gives a name to the interface that provides via contract-out all f's with contract 
 ;; and via plain provide all f's without contracts  ;; 'ctc' is its optional interface 
 
 ;; an interface is typically specified in a separate file say "server-intf.rkt"
 
 ;; in the exporting file, say "server.rkt"
 ;;   (name f ...)
 ;; connects the specification to actual definitions (should be done on a 'by-name' basis)
 
 ;; in the importing file, say "client.rkt"
 ;;   (require "server.rkt")
 ;; imports the specified variables and contracts 
 
 interface
 
 ;; SYNTAX
 ;; (testing def-or-exp ...)
 ;; create a test submodule that imports the surrounding module so that tests respect the contracts
 ;; PROBLEM: this won't let me test functions that don't get exported 
 ;; Perhaps we need new name : xtest 
 testing)

;; ---------------------------------------------------------------------------------------------------
;; implementation 

(require unstable/contract (for-syntax "../Performance/with-contract.rkt"))

(define-syntax (interface stx)
  (syntax-case stx (subject-to)
    [(interface name clauses ...)
     ;; ==> 
     (let* ([clauses (syntax->list #'(clauses ...))]
            [ids-with-contracts
             (foldr (λ (stx r) 
                      (syntax-case stx ()
                        [(f ctc) (list #'f #'ctc) (cons (list #'f #'ctc) r)]
                        [else r]))
                    '() clauses)]
            [ids-that-came-with-contracts 
             (map car ids-with-contracts)]
            [contracts-for-ids-with-contracts
             (map cadr ids-with-contracts)]
            [ids-without-contracts
             (foldr (λ (stx r) 
                      (syntax-case stx ()
                        [(f ctc) r]
                        [f (cons #'f r)]))
                    '() clauses)]
            [all-ids
             (foldr (λ (stx r) 
                      (syntax-case stx ()
                        [(f ctc) (cons #'f r)]
                        [f (cons #'f r)]))
                    '() clauses)])
       (with-syntax ([(ids-that-came-with-contracts ...) ids-that-came-with-contracts]
                     [(their-contracts ...) contracts-for-ids-with-contracts]
                     [(fresh-names ...) (generate-temporaries ids-that-came-with-contracts)]
                     [(ids-without-contracts ...) ids-without-contracts]
                     [(all-ids ...) all-ids])
         #`(begin 
             ; (define (ids-that-came-with-contracts . x) 0) ...
             ; (define ids-without-contracts 0) ...
             (provide name)
             (define (tmp)
               (define (ids-that-came-with-contracts) 0) ...
               (define fresh-names their-contracts) ...
               (void))
             (define-syntax (name stx)
               (syntax-case stx ()
                 [(name all-ids ...) 
                  #`(provide ids-without-contracts ...
                             #,@(if with-contracts 
                                    #'((contract-out (ids-that-came-with-contracts their-contracts) ...))
                                    #'(ids-that-came-with-contracts ...)))])))))]))

; (interface tst& (foo (-> integer? integer?)))
; (tst& add1)

(define-syntax-rule (==> a b) (if a b #t))

;; ---------------------------------------------------------------------------------------------------

(define-syntax-rule
  (testing def-or-exp ...)
  (module* xtest racket
    (require rackunit (submod ".."))
    (check-equal? 1 1)
    def-or-exp ...))
