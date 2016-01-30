#lang racket

(provide
 ;; [->* Any] [Any *-> Any] [S-expr -> Any] #:time T #:memory M -> Any 
 ;; run a thunk in a T sec, M mb limit and catches all exn:fail? exceptions, 
 ;; otherwise accept the code's behavior: 
 ;; -- accessing or deleting files 
 ;; -- displaying a gui 
 ;; -- reading or writing 
 ;; -- network connections 
 ;; etc. 
 in-sandbox)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(require racket/sandbox)

(module+ test
  (require rackunit)

  (define succ (lambda (x y z) (+ x y z)))
  (define fail (lambda (x) 'failure))
  
  (check-equal? (in-sandbox (lambda () (values 1 2 3)) succ fail) 6)
  (check-equal? (in-sandbox (lambda () (sleep 2 #;"secs")) succ fail) 'failure)
  (check-equal? (in-sandbox (lambda () (void (make-bytes 100000000))) succ fail) 'failure)
  (check-equal? (in-sandbox (lambda () (error 'x "y")) succ fail) 'failure))

(define (in-sandbox producer consumer failure #:time (sec-limit 1) #:memory (mb-limit 30))
  ((let/ec fail
     (define-syntax-rule (jump x) (fail (lambda () (failure x))))
     (call-with-values
      (lambda ()
        (with-handlers
            ((exn:fail:resource? (lambda (x) (jump `(R ,(exn-message x)))))
             (exn:fail:out-of-memory? (lambda (x) (jump `(R ,(exn-message x)))))
             (exn:fail? (lambda (x) (jump `(X ,(exn-message x))))))
          (with-limits sec-limit #;"s" mb-limit #;"Mb" 
                       (producer))))
      (lambda x (lambda () (apply consumer x)))))))
