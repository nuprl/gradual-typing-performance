#lang typed/racket/base
(require typed/scribble/core
         "base-render.rkt"
         typed/racket/class racket/list racket/string racket/match)
(require/typed scribble/text/wrap [wrap-line (String Integer -> (Listof String))])
(require/typed "private/render-utils.rkt" [part-style? (part Any -> (Option (Listof Any)))])
(require/typed "transpose.rkt" [transpose (All (A) ((Listof (Listof A)) -> (Listof (Listof A))))])
(provide render-mixin)

(: current-preserve-spaces (Parameterof Boolean))
(define current-preserve-spaces (make-parameter #f))

(define current-indent (make-parameter 0))
(: make-indent (Integer -> Integer))
(define (make-indent amt)
  (+ amt (current-indent)))
(define (indent)
  (define i (current-indent))
  (unless (zero? i) (display (make-string i #\space))))
(define (indented-newline)
  (newline)
  (indent))

(define table-ticks-depth (make-parameter 0))
(define phrase-ticks-depth (make-parameter 0))
(define note-depth (make-parameter 0))

(define-type Render-Class%
  (Class [traverse ((Listof part) (Listof Path-String) -> (HashTable Symbol Procedure))]
         [collect (case-> ((Listof part) (Listof Path-String) (HashTable Symbol Procedure) -> collect-info)
                          ((Listof part) (Listof Path-String) (HashTable Symbol Procedure) (Tag collect-info -> Any) -> collect-info))]
         [resolve ((Listof part) (Listof Path-String) collect-info -> resolve-info)]
         [render ((Listof part) (Listof Path-String) resolve-info -> (Listof Any))]
         [serialize-info (resolve-info -> Any)]
         [deserialize-info (Any collect-info [#:root (Option Path-String)] -> Void)]
         [get-external (resolve-info -> (Listof Tag))]
         [get-undefined (resolve-info -> (Listof Tag))]
         [current-render-mode (-> (Listof Symbol))]
         [get-substitutions (-> (Listof (List Regexp String)))]
         [render-part (part resolve-info -> Void)]
         [render-flow ((Listof Block) part resolve-info Boolean -> (Listof Any))]
         [render-intrapara-block (Block part resolve-info Boolean Boolean Boolean -> Any)]
         [render-table (table part resolve-info Boolean -> (Listof Any))]
         [render-itemization (itemization part resolve-info -> (Listof Any))]
         [render-paragraph (paragraph part resolve-info -> (Listof Any))]
         [render-content (Content part resolve-info -> (Listof Any))]
         [render-nested-flow (nested-flow part resolve-info Boolean -> (U Void (Listof Any)))]
         [render-block (Block part resolve-info Boolean -> (Listof Any))]
         [render-other (Any part resolve-info -> (Listof Any))]
         [get-dest-directory (case-> (-> Path-String)
                                     (Boolean -> Path-String))]
         [format-number ((Listof Any) Any -> (U (Pairof String Any) '()))]
         [number-depth ((Listof Any) -> Integer)]
         [get-suffix (-> Bytes)])) ; method of render% class, but overridden in render-mixin so it is required

(: render-mixin (All (r #:row) ((Class #:implements Render-Class% #:row-var r)  -> (Class #:implements Render-Class% #:row-var r))))
(define (render-mixin %)
  (class %
    
    (define/override (current-render-mode)
      '(markdown))

    (define/override (get-suffix) #".md")

    (define/override (get-substitutions)
      '((#rx"---" "\U2014")
        (#rx"--" "\U2013")
        (#rx"``" "\U201C")
        (#rx"''" "\U201D")
        (#rx"'" "\U2019")))

    (inherit render-block
             format-number
             number-depth)

    (define/override (render-part d ht)
      (let ([number (collected-info-number (part-collected-info d ht))])
        (unless (part-style? d 'hidden)
          (unless (zero? (number-depth number))
            (printf (make-string (number-depth number) #\#))
            (printf " "))
          (let ([s (format-number number '())])
            (unless (null? s)
              (printf "~a.~a" 
                      (car s)
                      (if (part-title-content d)
                          " "
                          "")))
            (when (part-title-content d)
              (render-content (assert (part-title-content d) content?) d ht))
            (when (or (pair? number) (part-title-content d))
              (newline)
              (newline))))
        (render-flow (part-blocks d) d ht #f)
        (let: loop : Void ([pos : Integer 1]
                   [secs (part-parts d)]
                   [need-newline? (pair? (part-blocks d))])
          (unless (null? secs)
            (when need-newline? (newline))
            (render-part (car secs) ht)
            (loop (add1 pos) (cdr secs) #t)))))

    (define/override (render-flow f part ht starting-item?)
      (if (null? f)
          null
          (append
           (render-block (car f) part ht starting-item?)
           (append*
            (for/list: : (Listof (Listof Any)) ([p : Block (in-list (cdr f))])
              (indented-newline)
              (render-block p part ht #f))))))

    (define/override (render-intrapara-block p part ri first? last? starting-item?)
      (unless first? (indented-newline))
      (super render-intrapara-block p part ri first? last? starting-item?))

    (define/override (render-table i part ht inline?)
      (: flowss (Listof (Listof (U Block 'cont))))
      (define flowss (table-blockss i))
      (unless (null? flowss)
        ;; Set table-ticks-depth prior to render-block calls
        (define tick? (member (style-name (table-style i))
                              (list 'boxed "defmodule" "RktBlk")))
        (when tick?
          (when (zero? (table-ticks-depth))
            (displayln "```racket"))
          (table-ticks-depth (add1 (table-ticks-depth))))
        (: strs (Listof (Listof (U 'cont (Listof String)))))
        (define strs (map (lambda: ([flows : (Listof (U Block 'cont))])
                            ((inst map (U 'cont (Listof String)) (U Block 'cont)) (lambda: ([d : (U Block 'cont)])
                                   (if (eq? d 'cont)
                                       d
                                       (let ([o (open-output-string)])
                                         (parameterize ([current-indent 0]
                                                        [current-output-port o])
                                           (render-block d part ht #f))
                                         (regexp-split
                                          #rx"\n"
                                          (regexp-replace #rx"\n$"
                                                          (get-output-string o)
                                                          "")))))
                                 flows))
                          flowss))
        (define widths ((inst map Integer (Listof (U 'cont (Listof String)))) (lambda: ([col : (Listof (U 'cont (Listof String)))])
                              (for/fold: : Integer ([d 0]) ([i (in-list col)])
                                (if (eq? i 'cont)
                                    0
                                    (apply max d (map string-length i)))))
                            (transpose strs)))
        (define x-length (lambda: ([col : (U 'cont (Listof String))]) (if (eq? col 'cont) 0 (length col))))
        (for/fold: ([indent? : Boolean #f]) ([row : (Listof (U 'cont (Listof String))) (in-list strs)])
          (let: ([h : Integer (apply max 0 (map x-length row))])
            (let ([row* (for/list: : (Listof (Listof String)) ([i : Integer (in-range h)])
                          (for/list: : (Listof String) ([col : (U 'cont (Listof String)) (in-list row)])
                            (if (i . < . (x-length col))
                                (list-ref (assert col list?) i)
                                "")))])
              (for/fold ([indent? indent?]) ([sub-row (in-list row*)])
                (when indent? (indent))
                (for/fold: ([space? : Boolean #f])
                    ([col (in-list sub-row)]
                     [w (in-list widths)])
                  (let ([col (if (eq? col 'cont) "" col)])
                    (display (regexp-replace* #rx"\uA0" col " "))
                    (display (make-string (max 0 (- w (string-length col))) #\space)))
                  #t)
                (newline)
                #t)))
          #t)
        (when tick?
          (table-ticks-depth (sub1 (table-ticks-depth)))
          (when (zero? (table-ticks-depth))
            (displayln "```"))))
      null)

    (define/override (render-itemization i part ht)
      (let ([flows (itemization-blockss i)])
        (if (null? flows)
            null
            (append
             (begin (printf "* ")
                    (parameterize ([current-indent (make-indent 2)])
                      (render-flow (car flows) part ht #t)))
             (append*
              (for/list: : (Listof (Listof Any)) ([d (in-list (cdr flows))])
                (indented-newline)
                (printf "* ")
                (parameterize ([current-indent (make-indent 2)])
                  (render-flow d part ht #f))))))))

    (define/override (render-paragraph p part ri)
      (define (write-note)
        (write-string (make-string (note-depth) #\>))
        (unless (zero? (note-depth))
          (write-string " ")))
      (define o (open-output-string))
      (parameterize ([current-output-port o])
        (super render-paragraph p part ri))
      ;; 1. Remove newlines so we can re-wrap the text.
      ;;
      ;; 2. Combine adjacent code spans into one. These result from
      ;; something like @racket[(x y)] being treated as multiple
      ;; RktXXX items rather than one. (Although it would be
      ;; more-correct to handle them at that level, I don't easily see
      ;; how. As a result I'm handling it after-the-fact, at the
      ;; text/Markdown stage.)
      (define to-wrap (regexp-replaces (get-output-string o)
                                       '([#rx"\n" " "]   ;1
                                         [#rx"``" ""]))) ;2
      (define lines (wrap-line (string-trim (assert to-wrap string?)) (- 72 (current-indent))))
      (write-note)
      (write-string (car lines))
      (for ([line (in-list (cdr lines))])
        (newline) (indent) (write-note) (write-string line))
      (newline)
      null)

    (: content-style (Any -> (U String Symbol False style)))
    (define/private (content-style e)
      (cond
       [(element? e) (element-style e)]
       [(multiarg-element? e) (multiarg-element-style e)]
       [else #f]))

    (define/override (render-content i part ri)
      (define tick?
        (and (zero? (table-ticks-depth))
             (element? i)
             (let ([s (element-style i)])
               (or (eq? 'tt s)
                   (and (style? s)
                        (style-name s)
                        ; (style-name s) needs to be a string below
                        ; but nothing really prevents it from being a symbol
                        ; which is a valid return type of style-name
                        (regexp-match? #rx"^Rkt[A-Z]" (assert (style-name s) string?)))))))
      (when tick?
        (when (zero? (phrase-ticks-depth))
          (display "`"))
        (phrase-ticks-depth (add1 (phrase-ticks-depth))))
      (define properties (let ([s (content-style i)])
                           (if (style? s) (style-properties s) '())))
      (define targ (for/or: : (Option target-url) ([p  properties])
                    (if (target-url? p) p #f)))
      (define url (and targ (target-url-addr targ)))
      ; i needs to be an element below
      ; but there is no guard to check if it is actually an element
      ; in the render% class render-content supports more types...
      (begin0
          (cond [url (define new-i
                       (match (element-content (assert i element?))
                         [(list (? string? s))
                          (element (element-style (assert i element?))
                            (assert (list (format "[~a](~a)" s url)) content?))]
                         [else i]))
                     (super render-content (assert new-i content?) part ri)]
                [(and (element? i)
                      (let ([s (element-style i)])
                        (or (eq? 'hspace s)
                            (and (style? s)
                                 (eq? 'hspace (style-name s))))))
                 (parameterize ([current-preserve-spaces #t])
                   (super render-content (assert i content?) part ri))]
                [else (define style (and (element? i) (element-style i)))
                      (define bold?   (eq? style 'bold))
                      (define italic? (eq? style 'italic))
                      (cond [bold?   (display "**")]
                            [italic? (display "_")])
                      (begin0
                          (super render-content i part ri)
                        (cond [bold?   (display "**")]
                              [italic? (display "_")]))])
        (when tick?
          (phrase-ticks-depth (sub1 (phrase-ticks-depth)))
          (when (zero? (phrase-ticks-depth))
            (display "`")))))

    (define/override (render-nested-flow i part ri starting-item?)
      (define s (nested-flow-style i))
      (unless (memq 'decorative (style-properties (assert s style?)))
        (define note? (equal? (style-name (assert s style?)) "refcontent"))
        (when note?
          (note-depth (add1 (note-depth))))
        (begin0 (super render-nested-flow i part ri starting-item?)
          (when note?
            (note-depth (sub1 (note-depth)))))))

    (define/override (render-other i part ht)
      (cond
        [(symbol? i)
         (display (case i
                    [(mdash) "\U2014"]
                    [(ndash) "\U2013"]
                    [(ldquo) "\U201C"]
                    [(rdquo) "\U201D"]
                    [(lsquo) "\U2018"]
                    [(rsquo) "\U2019"]
                    [(lang) ">"]
                    [(rang) "<"]
                    [(rarr) "->"]
                    [(nbsp) "\uA0"]
                    [(prime) "'"]
                    [(alpha) "\u03B1"]
                    [(infin) "\u221E"]
                    [else (error 'markdown-render "unknown element symbol: ~e"
                                 i)]))]
        [(string? i)
         (let* ([i (if (or (not (zero? (phrase-ticks-depth)))
                           (not (zero? (table-ticks-depth))))
                       (regexp-replace** i '([#rx"``" . "\U201C"]
                                             [#rx"''" . "\U201D"]))
                       (regexp-replace* #px"([#_*`]{1})" i "\\\\\\1"))]
                [i (if (current-preserve-spaces)
                       (regexp-replace* #rx" " i "\uA0")
                       i)])
           (display i))]
        [else (write i)])
      null)

    (super-new)))

(: regexp-replace** (String (Listof (Pairof Regexp String)) -> String))
(define (regexp-replace** str ptns&reps)
  (for/fold: : String ([str : String str])
                      ([ptn : Regexp ((inst map Regexp (Pairof Regexp String)) car ptns&reps)]
                       [rep : String ((inst map String (Pairof Regexp String)) cdr ptns&reps)])
    (regexp-replace* ptn str rep)))

