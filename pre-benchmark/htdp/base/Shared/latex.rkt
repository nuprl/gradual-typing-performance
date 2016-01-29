#lang at-exp racket/gui

(provide
 ;; [#:file (f String)] String *-> ImageSnip
 ;; (latex s ..) runs latex on s ... and turns the result into an image snip
 ;; if #:file is specified it also saves a version of the image in a file
 latex)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(define (latex #:file (f "x") . strs)
  (define x.tex (string-append f ".tex"))
  (define x.pdf (string-append f ".pdf"))
  (define x.png (string-append f ".png"))
  (define cmd (regexp-replace #rx"\n+" (COMMANDS x.tex x.pdf x.png) " \\&\\& "))
  (define latex/x (string->path "Latex/")) 
  (define (run) 
    (parameterize ([current-directory latex/x] 
                   [current-input-port (open-input-bytes #"")] 
                   [current-output-port (open-output-string)]) 
      (call-with-output-file* x.tex #:exists 'truncate
        (lambda (o) (fprintf o TEMPLATE (string-append* strs))))
      (unless (system cmd)
        (display (get-output-string (current-output-port)) (current-error-port))
        (error 'latex "commands did not run successfully, see above output"))
      (begin0
        (make-object image-snip% x.png 'png/mask)
	#;
        (rename-file-or-directory x.png (string-append "../" x.png) #t)
	#;
        (rename-file-or-directory x.pdf (string-append "../" x.pdf) #t))))
  ; (define (cleanup) (delete-directory/files latex)) 
  (dynamic-wind void run void))

(define str string-append) 

(define TEMPLATE 
  @str{\documentclass[11pt]{article} 
       % \usepackage[mathletters]{ucs} 
       % \usepackage[utf8x]{inputenc} 
       \usepackage{amsmath} 
       \pagestyle{empty} 
       \begin{document} 
       \[ 
       ~a 
       \] 
       \end{document}})

(define (COMMANDS x.tex x.pdf x.png)
  @str{pdflatex @x.tex 
       convert -density 96x96 @x.pdf -trim +repage @x.png})

@;latex{\sum_{i=0}^{\infty}\lambda_i}
