#lang typed/racket

(require typed/racket/gui)
(require/typed htdp/error
               [tp-error ((U Symbol String) String Any * -> Nothing)])
(require/typed mzlib/pconvert
               [print-convert (Any -> String)]
               [constructor-style-printing (Parameter Any)]
               [booleans-as-true/false (Parameter Any)]
               [abbreviate-cons-as-list (Parameter Any)])

(provide checked-cell% Checked-Cell%)
(define-type (Checked-Cell% X)
  (Class (init-field [value0 X] [ok? (X -> Boolean)])
         (init [display (Option String) #:optional])
         (field [value X] [pb (Option (Instance Pasteboard%))])
         [set ((U Symbol String) X -> Any)]
         [get (-> X)]))

;; (: checked-cell% : Checked-Cell%) -- Asumu -- why does this line fail?
(define checked-cell%
  (class object%
    #:forall (X)
    (init-field [value0 : X]
                [ok? : (X -> Boolean)])
    
    (init [display : (Option String) #f]) ; a string is the name of the state display window
    
    (field 
     [value : X (coerce "the initial expression" value0 #t)]
     [pb : (U False (Instance Pasteboard%))
         (let ([display display])
           (if (boolean? display)
               #f
               (let* ([f (new frame% [label display][width 400][height 400])]
                      [p (new pasteboard%)]
                      [e (new editor-canvas% [parent f] [editor p]
                              [style '(hide-hscroll hide-vscroll)])])
                 (send f show #t)
                 p)))])
    (private [show-state : (-> Void)])
    (define (show-state)
      (define xbox #f) ;; x coordinate (throw away)
      (define ybox ((inst box Real) 0))  ;; y coordinate for next snip
      (define s
        (pretty-format
         (parameterize ([constructor-style-printing #t]
                        [booleans-as-true/false #t]
                        [abbreviate-cons-as-list 
                         #t
                         ;; is this beginner or beginner+quote
                         #;
                         (let ([o (open-output-string)])
                           (print '(1) o)
                           (regexp-match #rx"list" (get-output-string o)))])
           (print-convert value)) 
         40))
      ;; turn s into lines and display them in pb
      (let ([pb (assert pb)]
            [value value])
        (send pb erase)
        (if (is-a? value snip%)
            (send pb insert (cast value (Instance Snip%)) 0 0)
            (parameterize ([current-input-port (open-input-string s)])
              (ann (let read-all ()
                     (define nxt (read-line))
                     (unless (eof-object? nxt)
                       (let ([s (make-object string-snip% nxt)])
                         (send pb insert s 0 (unbox ybox))
                         (send pb get-snip-location s xbox ybox #t)
                         (read-all)))) Void)))))
    
    ;; Symbol Any -> ok?
    (private [coerce : (case-> ((U String Symbol) X -> X)
                               ((U String Symbol) X Any -> X))])
    (define (coerce tag nw [say-evaluated-to #f])
      (let ([b (ok? nw)])
        (unless (boolean? b)
          (tp-error 'check-with "the test function ~a is expected to return a boolean, but it returned ~v" 
                    (object-name ok?) b))
        (unless b
          (define check-with-name 
            (let ([n (symbol->string (assert (object-name ok?) symbol?))])
              (if (regexp-match "check-with" n)
                  "handler"
                  n)))
          (tp-error 'check-with "~a ~a ~v, which fails to pass check-with's ~a test"
                    tag (if say-evaluated-to "evaluated to" "returned") 
                    nw check-with-name))
        nw))
    
    ;; Symbol Any -> Void 
    ;; effect: set value to v if distinct, also display it if pb exists
    (public (set : ((U Symbol String) X -> Any)))
    (define (set tag v)
      (define nw  (coerce tag v))
      ;; this is the old Robby "optimization" for not triggering draw
      ;; when the world doesn't change 
      ;if (equal? value nw)
      ;   #t
      (begin
        (set! value nw)
        (when pb (show-state))
        #f))
    
    ;; -> ok?
    (public (get : (-> X)))
    (define (get) value)
    
    (super-new)
    
    (when pb (show-state))))

; (define c (new checked-cell% [msg "World"] [value0 1] [ok? positive?]))
; (send c set "tick" 10)
