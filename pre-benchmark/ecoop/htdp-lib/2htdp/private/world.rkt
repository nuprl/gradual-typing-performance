#lang typed/racket

(require "check-aux.rkt"
         "timer.rkt"
         "last.rkt"
         "stop.rkt"
         "world-type.rkt"
         "checked-cell.rkt"
         (except-in "pad.rkt" game-pad)
	 (rename-in (only-in typed/2htdp/image
                             Image Image-Color image? scale overlay/align empty-image)
                    [image? 2:image?])
         racket/runtime-path
         string-constants
         typed/mrlib/gif
         typed/mrlib/bitmap-label
         typed/racket/gui)

(require/typed htdp/error
               [check-arg ((U String Symbol) Boolean (U String Symbol)
                           (U String Symbol Natural) Any -> Void)]
               [check-result ((U Symbol String) (Any -> Boolean) (U Symbol String) Any Any * -> Any)]
               [tp-error ((U String Symbol) String Any * -> Void)])

(require/typed "universe-image.rkt"
               [check-scene-dimensions ((U Symbol String) Natural Natural -> Void)]
               [check-scene-result ((U Symbol String) Image -> Image)]
               [disable-cache (Image -> (Instance Snip%))]
               [image-height (Image -> Natural)]
               [image-width (Image -> Natural)]
               [text (String Positive-Integer Image-Color -> Image)]
               [draw-image (Image (Instance DC<%>) Natural Natural -> Void)]
               [insert-into-pasteboard ((Instance Pasteboard%) Image -> Void)]
               [delete-first-snip ((Instance Pasteboard%) -> Void)])

(define-type Pad-Event String)
(require/typed "pad.rkt" [game-pad Image])


(provide world% aworld%)

;                                     
;                                     
;                                     
;   ;   ;                  ;        ; 
;   ;   ;                  ;        ; 
;   ;   ;                  ;        ; 
;   ;   ;   ;;;   ; ;;     ;     ;;;; 
;   ;   ;  ;   ;  ;;  ;    ;    ;   ; 
;   ; ; ;  ;   ;  ;   ;    ;    ;   ; 
;   ;; ;;  ;   ;  ;        ;    ;   ; 
;   ;   ;  ;   ;  ;        ;    ;   ; 
;   ;   ;   ;;;   ;        ;;    ;;;; 
;                                     
;                                     
;                                     

(define MIN-WIDT-FOR-GAME-PAD 300)

;; -----------------------------------------------------------------------------
;; packages for broadcasting information to the universe 

(struct: package ([world : World] [message : Sexp]) #:transparent)
(: make-package (World Sexp -> package))
(define (make-package w m)
  (check-arg 'make-package (sexp? m) 'sexp "second" m)
  (package w m))

(provide
 make-package  ;; World S-expression -> Package
 package?      ;; Any -> Package
 package-world ;; Package -> World 
 )

(require (for-syntax syntax/parse racket/syntax))
(define-syntax (init-private stx)
  (syntax-parse stx #:literals (:)
    [(_) #'(begin)]
    [(_ [name : type e ...] . rst)
     (define/with-syntax internal (generate-temporary))
     #'(begin (init [(internal name) : type e ...])
              (: name type)
              (define name (#%expression internal))
              (init-private . rst))]))

(define world%
  (last-mixin
   (clock-mixin
    (class object%
      (inspect #f)
      (init-field [world0 : World])
      (init-field [record? : Any])
      (init-private [name : (Option String)]
                    [state : (Option String)]
                    [register : (Option String)]
                    [port : Integer]
                    [check-with : (Any -> Boolean)]
                    [on-key : (Option (World Key-Event -> World))]
                    [on-release : (Option (World Key-Event -> World))]
                    [on-pad : (Option (World Pad-Event -> World))]
                    [on-mouse : (Option (World Integer Integer Mouse-Event -> World))])
      (init [on-receive : (Option (World Sexp -> World))]
            [on-draw : (U (List (World -> Image) Natural Natural) (World -> Image) #f)]
            [stop-when : (U (World -> Boolean) (List (World -> Boolean) (World -> Image)))])
      
      ;; -----------------------------------------------------------------------
      (field [to-draw : (U (List (World -> Image) Natural Natural) (World -> Image) #f) on-draw])
      (define: world : (Instance (Checked-Cell% World))
               (new (inst checked-cell% World) [value0 world0] [ok? check-with]
                    [display (and state (or name "your world program's state"))]))


      ;; -----------------------------------------------------------------------
      (define: *out* : (Option Output-Port) #f)
      (define: *rec* : Custodian (make-custodian))

      (: register-with-host (-> Any))
      (define/private (register-with-host)
        (define FMT "\nworking off-line\n")
        (define FMTtry
          (string-append "unable to register with ~a after ~s tries"
                         FMT))
        (define FMTcom
          (string-append "unable to register with ~a due to protocol problems"
                         FMT))
        ;; create closure (for thread) to receive messages and signal events
        (: RECEIVE (Input-Port -> (-> Void)))
        (define (RECEIVE in)
          (: RECEIVE (-> Void))
          (define (RECEIVE)
            (sync 
             (handle-evt
              in
              (lambda: ([in : Input-Port])
                (define dis (text "the universe disappeared" 11 'red))
                (with-handlers ((tcp-eof?
                                 (compose (handler #f)
                                          (lambda: ([e : Symbol])
                                            (set! draw (lambda (w) dis))
                                            (pdraw)
                                            e))))
                  ;; --- "the universe disconnected" should come from here ---
                  (define msg (tcp-receive in))
                  (cond
                    [(sexp? msg) (prec (cast msg Sexp)) (RECEIVE)] ;; break loop if EOF
                    [#t (error 'RECEIVE "sexp expected, received: ~e" msg)]))))))
          RECEIVE)
        ;; --- now register, obtain connection, and spawn a thread for receiving
        (parameterize ([current-custodian *rec*])
          ;; try to register with the server n times 
          (let try ([n TRIES])
            (printf "trying to register with ~a ...\n" register)
            (with-handlers ((tcp-eof? (lambda (x) (printf FMTcom register)))
                            (exn:fail:network? 
                             (lambda (x)
                               (if (= n 1) 
                                   (printf FMTtry register TRIES)
                                   (begin (sleep PAUSE) (try (- n 1)))))))
              (define-values (in out) (tcp-connect (assert register) port))
              (tcp-register in out name)
              (printf "... successful registered and ready to receive\n")
              (set! *out* out)
              (thread (RECEIVE in))))))

      (: broadcast (Sexp -> Void))
      (define/private (broadcast msg)
        (let ([*out* : (Option Output-Port) *out*])
          (when *out*
            (check-result 'send sexp? "Sexp expected; given ~e\n" msg)
            (tcp-send *out* msg))))
      
      ;; -----------------------------------------------------------------------
      (field [draw : (Option (World -> Image))
                   (let ([to-draw to-draw])
                     (cond
                      [(procedure? to-draw) to-draw]
                      [(pair? to-draw)      (first to-draw)]
                      [else to-draw]))]
             [width : (Option Natural)
                    (let ([to-draw to-draw]) (if (pair? to-draw) (second to-draw) #f))]
             [height : (Option Natural)
                     (let ([to-draw to-draw]) (if (pair? to-draw) (third to-draw) #f))])
      (define: live : Boolean (not (boolean? draw)))
      
      ;; the visible world 
      (define: enable-images-button : (-> Void) void) ;; used if stop-when call produces #t
      (define: disable-images-button : (-> Void) void)
      (define: visible : (Instance Pasteboard%) (new pasteboard%))

      (: show-canvas (-> Void))
      (define/private (show-canvas)
        (send visible set-cursor (make-object cursor% 'arrow))
        (let ([fst-scene (ppdraw)]
              [width* width] [height* height])
          (if (2:image? fst-scene)
              (let ([first-width  (+ (image-width fst-scene) 1)]
                    [first-height (+ (image-height fst-scene) 1)])
                (unless (and width* height*)
                  (check-scene-dimensions (name-of draw 'your-draw) first-width first-height)
                  (set! width first-width)
                  (set! height first-height)))
              (let ([first-width  (image-width fst-scene)]
                    [first-height (image-height fst-scene)])
                (unless (and width* height*)
                  (set! width first-width)
                  (set! height first-height))))
	  (when pad
	    (unless (>= (assert width number?) MIN-WIDT-FOR-GAME-PAD)
	      (error 'big-bang
		     "a game pad requires a scene whose width is greater or equal to ~a, given ~e"
		     MIN-WIDT-FOR-GAME-PAD fst-scene))
	    (set! game-pad-image (scale (assert (/ (assert width number?) (image-width game-pad)) positive?) game-pad)))
          (create-frame)
          (show fst-scene)))
      
      (: add-game-pad (Image -> Image))
      (define/private (add-game-pad scene)
        (let ([game-pad-image : (Option Image) game-pad-image])
          (if (boolean? pad) scene (overlay/align 'left 'bottom (assert game-pad-image 2:image?) scene))))

      (: deal-with-key (All (r #:row) ((Class #:row-var r [on-char ((Instance Key-Event%) -> Void)])
                                       ->
                                       (Class #:row-var r [on-char ((Instance Key-Event%) -> Void)]))))
      (define/public (deal-with-key %)
        (if (and (not on-key) (not on-pad) (not on-release))
            %
            (class %
              (super-new)
              (define/override (on-char e) 
                (when live
                  (let ([e:str (key-event->parts e)])
                    (cond
		      [(string=? e:str "release") (prelease (key-release->parts e))]
		      [(and pad (pad-event? e:str)) (ppad e:str)]
		      [else (pkey e:str)])))))))

      (: deal-with-mouse (All (r #:row) ((Class #:row-var r [on-event ((Instance Mouse-Event%) -> Void)])
                                         ->
                                         (Class #:row-var r [on-event ((Instance Mouse-Event%) -> Void)]))))
      (define/public (deal-with-mouse %)
        (if (not on-mouse) 
            ;; No mouse handler => discard mouse events (so snip are not selected
            ;;  in the pasteboard, for example
            (class %
              (super-new)
              (define/override (on-event e)
                (void)))
            ;; Mouse handler => handle mouse events
            (let ()
              (define width* (assert width number?))
              (define height* (assert height number?))
              (class %
                (super-new)
                (define/override (on-event e)
                  (define-values (x y me) (mouse-event->parts e))
                  (when live
                    (cond
                     [(and (<= 0 x) (<= x width*) (<= 0 y) (<= y height*))
                      (pmouse x y me)]
                     [(member me '("leave" "enter")) (pmouse x y me)]
                     [else (void)])))))))
      
      ;; allows embedding of the world-canvas in other GUIs
      (: create-frame (-> Void))
      (define/public (create-frame)
        (create-frame/universe))
      
      ;; effect: create, show and set the-frame
      (: create-frame/universe (-> Void) #:augment ((Instance Frame%) Custodian -> (Values (-> Void) (-> Void))))
      (define/pubment (create-frame/universe)
        (define play-back:cust (make-custodian))
        (define frame (new (class frame%
                             (super-new)
                             (define/augment (on-close)  
                               (callback-stop! 'frame-stop)
                               (custodian-shutdown-all play-back:cust)))
                           (label (if name (format "~a" name) "World"))
                           (alignment '(center center))
                           (style '(no-resize-border metal))))
        
        (define editor-canvas
          (new (deal-with-key (deal-with-mouse editor-canvas%))
               (parent frame)
               (editor visible)
               (stretchable-width #f)
               (stretchable-height #f)
               (style '(no-hscroll no-vscroll))
               (horizontal-inset INSET)
               (vertical-inset INSET)))
        (define width* width)
        (define height* height)
        (when width* (send editor-canvas min-client-width (+ width* INSET INSET)))
        (when height* (send editor-canvas min-client-height (+ height* INSET INSET)))
        (set!-values (enable-images-button disable-images-button)
                     (inner (values void void) create-frame/universe frame play-back:cust))
        (send editor-canvas focus)
        (send frame show #t))
      
      ;; show the image in the visible world
      (: show (Image -> Void))
      (define/public (show pict0)
        (define pict (add-game-pad pict0))
        (send visible begin-edit-sequence)
        (send visible lock #f)
        (let ([c (send visible get-canvas)])
          (delete-first-snip visible)
          (insert-into-pasteboard visible pict)
          (send visible lock #t)
          (send visible end-edit-sequence)
          ;; The following flush trades streaming performance (where updates
          ;; could be skipped if they're replaced fast enough) for
          ;; responsiveness (where too many updates might not get
          ;; through if the canvas is mostly in suspended-refresh
          ;; mode for scene changes):
          ;(send c flush)
          ))
      
      ;; ----------------------------------------------------------------------
      ;; callbacks 
      (define: key : (World Key-Event -> World) (if on-key on-key (lambda: ([w : World] [ke : Key-Event]) w)))
      (define: pad : (Option (World Pad-Event -> World)) on-pad)
      (define: game-pad-image : (Option Image) #f)
      (define: release : (World Key-Event -> World) (if on-release on-release (lambda: ([w : World] [ke : Key-Event]) w)))
      (define: mouse : (Option (World Integer Integer Mouse-Event -> World)) on-mouse)
      (define: rec : (World Sexp -> World) (assert on-receive))

      (define: drawing : Boolean #f) ;; Boolean; is a draw callback scheduled?
      (: set-draw#! (-> Void))
      (define (set-draw#!) (set! draw# (random 3)) (set! drawing #f))
      (define: draw# : Integer 0)
      (set-draw#!)
      
      (define-syntax def/cback
        (syntax-rules ()
          [(_ pub (name arg ...) transform) 
           (def/cback pub (name arg ...) transform (assert (object-name transform) symbol?))]
          [(_ pub (name arg ...) transform tag)
           ;; Any ... -> Boolean
	   (begin
	     (pub name)

           (define (name arg ...)
             (queue-callback
              (lambda ()
                (define H (handler #t))
                (with-handlers ([exn? H])
                  ; (define tag (object-name transform))
                  (define: nw : (U stop-the-world World package) (transform (send world get) arg ...))
                  (define (d)
                    (with-handlers ((exn? H))
                      (pdraw))
                    (set-draw#!))
                  ;; ---
                  (define: w : (Listof (Boxof (-> Void))) '())
                  ;; set all to void, then w to null
                  ;; when a high priority draw is scheduledd
                  ;; ---
                  (define nw*
                   (cond [(package? nw)
                          (broadcast (package-message nw))
                          (package-world nw)]
                         [else nw]))
                  (cond
                    [(stop-the-world? nw*)
                     (define nw (stop-the-world-world nw*))
                     (send world set tag nw)
                     (last-draw)
                     (callback-stop! 'name)
                     (enable-images-button)]
                    [(not (package? nw*))
                     (: changed-world? Any)
                     [define changed-world? (send world set tag nw*)]
                     (: stop? Any)
                     [define stop? (stop (send world get))]
                     ;; this is the old "Robby optimization" see checked-cell:
                     ; unless changed-world?
                     (cond
                       [(and draw (not stop?))
                        (cond
                          [(not drawing)
                           (set! drawing #t)
                           (let ([b (box d)])
                             (set! w (cons b w))
                             ;; low priority, otherwise it's too fast
                             (queue-callback (lambda () ((unbox b))) #f))]
                          [(< draw# 0)
                           (set-draw#!)
                           (for-each (lambda: ([b : (Boxof (-> Void))]) (set-box! b void)) w)
                           (set! w '())
                           ;; high!!  the scheduled callback didn't fire
                           (queue-callback (lambda () (d)) #t)]
                          [else
                           (set! draw# (- draw# 1))])]
                       [stop?
                        (last-draw)
                        (callback-stop! 'name)
                        (enable-images-button)])
                     changed-world?]))))))]))

      ;; tick, tock : deal with a tick event for this world
      (: ptock (-> Void))
      (def/cback pubment (ptock) (lambda: ([w : World]) (pptock w)) (name-of-tick-handler))

      (: pptock (World -> (U World stop-the-world)))
      (define/public (pptock w) w)

      (: name-of-tick-handler (-> String))
      (define/public (name-of-tick-handler)
        "the on-tick handler")
      
      ;; key events
      (: pkey (Key-Event -> Void))
      (def/cback pubment (pkey ke) key)
      
      ;; key events 
      (: ppad (Key-Event -> Void))
      (def/cback pubment (ppad ke) (assert pad procedure?))
      
      ;; release events 
      (: prelease (Key-Event -> Void))
      (def/cback pubment (prelease ke) release)
      
      ;; mouse events
      (: pmouse (Integer Integer Mouse-Event -> Void))
      (def/cback pubment (pmouse x y me) (assert mouse procedure?))
      
      ;; receive revents
      (: prec (Sexp -> Void))
      (def/cback pubment (prec msg) rec)
      
      ;; ----------------------------------------------------------------------
      ;; draw : render the given world or this world (if #f)
      (: pdraw (-> Void))
      (define/private (pdraw) 
        (show (ppdraw)))
      
      ;; produce the scene for the this state
      (: ppdraw (-> Image))
      (define/public (ppdraw)
        (let ([draw draw])
          (if draw
              (check-scene-result (name-of draw 'your-draw) (draw (send world get)))
              ;; should not happen
              empty-image)))
      
      ;; ---------------------------------------------------------------------------------------------
      ;; stop-when 
      (define: stop : (World -> Any)
               (let ([stop-when stop-when])
                 (let ((s (if (procedure? stop-when) stop-when (first stop-when))))
                   (lambda: ([x : World])
                     (define result (s x))
                     (check-result (name-of s 'your-stop-when) boolean? "boolean" result)
                     result))))
      (define: last-picture : (Option (World -> Image))
               (let ([stop-when stop-when])
                 (if (pair? stop-when) (second stop-when) #f)))

      (: last-draw (-> Void))
      (define/private (last-draw)
        (when last-picture (set! draw last-picture))
        (pdraw))
      
      ;; ---------------------------------------------------------------------------------------------
      ;; start & stop
      (: callback-stop! (Symbol -> Void))
      (define/public (callback-stop! msg)
        (stop! (send world get)))

      (: handler (Boolean -> ((U Symbol exn) -> Void)))
      (define (handler re-raise)
        (lambda: ([e : (U Symbol exn)])
          (disable-images-button)
          (stop! (if re-raise e (send world get)))))

      (: start! (-> Void))
      (define/public (start!)
        (with-handlers ([exn? (handler #t)])
          (let ([width width] [height height])
            (when (and width height) ;; and height
              (check-scene-dimensions "your to-draw clause" width height))
            (when register (register-with-host))
            (define w (send world get))
            (cond
              [(stop w)
               (when last-picture (set! draw last-picture))
               (show-canvas)
               (stop! w)]
              [(stop-the-world? w)
               (when last-picture (set! draw last-picture))
               (show-canvas)
               (stop! (stop-the-world-world w))]
              [else (show-canvas)]))))

      (: stop! ((U Symbol exn World) -> Void))
      (define/public (stop! w)
        (set! live #f)
        (custodian-shutdown-all *rec*))
      
      ;; -------------------------------------------------------------------------
      ;; initialize the world and run 
      (super-new)
      (start!)))))

; (define make-new-world (new-world world%))

;; -----------------------------------------------------------------------------
(define-runtime-path break-btn:path '(lib "icons/break.png"))
(define break-button:label 
  ((bitmap-label-maker (string-constant break-button-label) break-btn:path) '_))

(define-runtime-path image-button:path '(lib "icons/file.gif"))
(define image-button:label ((bitmap-label-maker "Images" image-button:path) '_))

;; turn the list of thunks into animated gifs
;; effect: overwrite the ANIMATED-GIF-FILE (in current directory)
;; turn the list of thunks into animated gifs
;; effect: overwrite the ANIMATED-GIF-FILE (in current directory)
(: create-animated-gif (Real (Listof (U (Instance Bitmap%) (-> (Instance Bitmap%)))) -> Void))
(define (create-animated-gif R bitmap-list)
  (when (file-exists? ANIMATED-GIF-FILE) (delete-file ANIMATED-GIF-FILE))
  (write-animated-gif bitmap-list (if (and (> R 0) (< R +inf.0)) (exact-floor R) 5)
                      ANIMATED-GIF-FILE
                      #:one-at-a-time? #t
                      #:loop? #f))

(define: ANIMATED-GIF-FILE : String "i-animated.gif")

(define aworld%
  (class world% (super-new)
    (inherit-field world0 draw rate width height record?)
    (inherit show callback-stop!)
    
    (: recordable-directory : (-> (Option Path-String)))
    (define/private (recordable-directory)
      (let ([record? record?])
        (and (path-string? record?) (directory-exists? record?) record?)))
    
    ;; adds the stop animation and image creation button, 
    ;; whose callbacks runs as a thread in the custodian
    (define/augment (create-frame/universe frm play-back-custodian)
      (define p (new horizontal-pane% [parent frm][alignment '(center center)]))
      (: stop-button (Option (Instance Button%)))
      (: image-button (Option (Instance Button%)))
      (define stop-button #f)
      (define image-button #f)
      (: pb (-> Void))
      (define (pb)
        (parameterize ([current-custodian play-back-custodian])
          (thread (lambda () (play-back)))
          (stop)))
      (: switch (-> Void))
      (define (switch)
        (send (assert stop-button) enable #f)
        (if (recordable-directory) (pb) (send (assert image-button) enable #t)))
      (: stop (-> Void))
      (define (stop) 
        (send (assert image-button) enable #f)
        (send (assert stop-button) enable #f))
      (define-syntax-rule (btn l a y ...)
        (new button% [parent p] [label l] [style '(border)] 
             [callback (lambda: a y ...)]))
      (set! stop-button
        (btn break-button:label
             ([b : (Instance Button%)] [e : (Instance Control-Event%)])
             (callback-stop! 'stop-images) (switch)))
      (set! image-button
        (btn image-button:label
             ([b : (Instance Button%)] [e : (Instance Control-Event%)]) (pb)))
      (send (assert image-button) enable #f)
      (values switch stop))
    
    ;; an argument-recording ppdraw
    (field [image-history : (Listof Image) '()]) ;; [Listof Evt]
    (define/override (ppdraw)
      (define image (super ppdraw))
      (set! image-history (cons image image-history))
      image)
    
    ;; re-play the history of events; create a png per step; create animated gif
    ;; effect: write to user-chosen directory
    (: play-back (-> Void))
    (define/private (play-back)
      ;; --- creating images 
      (define total (+ (length image-history) 1))
      (define digt# (string-length (number->string total)))
      (: imag# Natural)
      (define imag# 0)
      (: bmps (Listof (Instance Bitmap%)))
      (define bmps '())
      (define width* (assert width exact-positive-integer?))
      (define height* (assert height exact-positive-integer?))
      (: save-image (Image -> Image))
      (define (save-image img)
        (define bm (make-bitmap width* height*))
        (define dc (new bitmap-dc% [bitmap bm]))
        (send dc clear)
        (draw-image img dc width* height*)
        (set! imag# (+ imag# 1))
        (send bm save-file (format "i~a.png" (zero-fill imag# digt#)) 'png)
        (set! bmps (cons bm bmps))
        img)
      ;; --- choose place 
      (define img:dir
        (or (recordable-directory)
            (get-directory "image directory:" #f (current-directory))))
      (define draw* (let ([draw draw]) (if draw draw (λ: ([x : Any]) empty-image))))
      (when img:dir
        (parameterize ([current-directory img:dir])
          (define imageN 
            (if (empty? image-history)
                (save-image (draw* world0))
                (first (map save-image image-history))))
          (show (text (format "creating ~a" ANIMATED-GIF-FILE) 18 'red))
          (create-animated-gif rate bmps)
          (show imageN))))))

