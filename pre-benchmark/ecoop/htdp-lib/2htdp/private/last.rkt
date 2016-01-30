#lang typed/racket

(require typed/racket/gui "timer.rkt" "world-type.rkt")

(provide last-mixin)

(: last-mixin (All (r #:row)
                ((Class #:row-var r #:implements Start-Stop<%>)
                 ->
                 (Class #:row-var r #:implements Start-Stop<%>
                        (field [end:ch Any]
                               [dr:cust Custodian])
                        [last (-> World)]))))
(define (last-mixin cls)
  (class cls
    ;; to comunicate between stop! and last
    (field [end:ch  : (Channelof (U exn World)) ((inst make-channel (U exn World)))])

    (define/override (stop! w)
      (send-to-last w)
      (super stop! w))
    
    (define/public (last) 
      (define result (yield end:ch))
      (if (exn? result) (raise result) result))
    
    (field [dr:cust (current-custodian)])

    ;; send x to last method
    (: send-to-last ((U exn World) -> Void))
    (define/private (send-to-last x)
      (parameterize ((current-custodian dr:cust))
        (thread 
         (lambda ()
           (channel-put end:ch x))))
      (void))
    
    (super-new)))
    

