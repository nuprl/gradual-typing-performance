#lang scribble/manual

@(require "../base/Shared/shared.rkt" (prefix-in 2: 2htdp/image) "../base/Shared/shared1.rkt")

@(define tech/link tech)

@(define (tech/link-old x)
   (link (format "../part_one.html#%28tech._~a%29" (string-downcase x)) x))

@(define (simple-key (content ""))
   (2:place-image (2:text content 16 'red) 100 15 (2:empty-scene 200 30)))

@(define (simple-mouse)
  (define-syntax-rule 
    (seq (s x) y ...)
    (let* ([s x] [s y] ...) s))
  (define WIDTH 100)
  (define HEIGHT 100)
  (define (random-complex)
    (random-seed 1234)
    (let loop ([i 50][x (random WIDTH)][y (random WIDTH)])
      (cond
        [(= i 0) '()]
        [else (define x1 (+ x (random 8)))
              (define y1 (+ y (random 10)))
              (cons (make-rectangular x y) (loop (sub1 i) x1 y1))])))
  (seq (s (2:empty-scene WIDTH HEIGHT))
       (for/fold ([s s]) ((p (random-complex)))
         (2:place-image (2:circle 2 'solid 'red) (real-part p) (imag-part p) s))))

@; -----------------------------------------------------------------------------
@title[#:tag "note:mice-and-chars"]{A Note on Mice and Characters}

@Figure-ref{fig:mouse-record} displays another program that handles mouse
 events. Specifically, it is an interactive program that just records where
 the mouse events occur via small dots.@margin-note*{It is acceptable to
 break the rule of separating data representations and image rendering for
 such experimental programs, whose sole purpose it is to determine how
 something works.} It ignores what kind of mouse event occurs, and it also
 ignores the first guideline about the separation of state representation
 and its image. Instead the program uses images as the state of the
 world. Specifically, the state of the world is an image that contains red
 dots where a mouse event occurred. When another event is signaled, the
 @racket[clack] function just paints another dot into the current state of
 the world.

@figure["fig:mouse-record" "A mouse event recorder"]{
@(begin #reader scribble/comment-reader
(racketblock
;; @deftech{AllMouseEvts} is an element of @tech/link{Image} 
;; @bold{interpretation} an image with markers for all mouse events 

;; graphical constants
(define MT (empty-scene 100 100))

;; @racket[PositiveNumber] -> @tech/link{Image}
;; records all mouse events for the specified time interval  
(define (main duration)
  (big-bang MT
            [to-draw show]
            [on-tick do-nothing 1 duration]
            [on-mouse clack]))

;; @tech{AllMouseEvts} @tech/link{Number} @tech/link{Number} @tech/link{String} -> @tech{AllMouseEvts}
;; adds a dot at (x,y) to @racket[ws]

(check-expect 
 (clack MT 10 20 "something mousy")
 (place-image (circle 1 "solid" "red") 10 20 MT))

(check-expect 
 (clack (place-image (circle 1 "solid" "red") 1 2 MT) 3 3 "")
 (place-image (circle 1 "solid" "red") 3 3
              (place-image (circle 1 "solid" "red") 1 2 MT)))

(define (clack ws x y action) 
  (place-image (circle 1 "solid" "red") x y ws))

;; @tech{AllMouseEvts} -> @tech{AllMouseEvts} 
;; reveals the current world state (because it is am image)

(check-expect (show MT) MT)

(define (show ws)
  ws)

;; @tech{AllMouseEvts} -> @tech{AllMouseEvts} 
(define (do-nothing ws) ws)
))}

Stop! Check the documentation to find out what an @racket[on-tick] clause
means when it specifies three distinct pieces. All you have seen so far are
@racket[on-tick] clauses that specify a clock-tick handling function. 

To play with this program, copy it into the definitions area of @dr[]
 and click @button{run}. In the interactions area, evaluate 
@racketblock[
(main 10)
]
 This opens the world's canvas and allows you to move the mouse and to
 record its movement for 10 seconds. Doing so produces a canvas that looks
 something like this: 
@centerline{

}
 Stop! What do you think this image reveals? 

Given that we moved the mouse continuously to produce the above image, the
 scattering of points reveals that an operating system does @bold{not}
 track every single point where the mouse appears. Instead it samples mouse
 events sufficiently often and tells your program about those sample
 events. Usually these samples suffice for people's purposes.

@figure["fig:key-record" "A key event recorder"]{
@;%
@(begin
#reader scribble/comment-reader
(racketblock
; physical constants:
(define WIDTH 100)
(define HEIGHT 30)
 
; graphical constant:
(define MT (empty-scene WIDTH HEIGHT))

; @deftech{AllKeys} is a @tech/link{String}
; @bold{interpretation} the keys pressed since @racket[big-bang] created the canvas

;; @tech{AllKeys} -> @tech{AllKeys}
(define (main s)
  (big-bang s
            [on-key remember]
            [to-draw show]))
   

; @tech{AllKeys} @tech/link{String} -> @tech{AllKeys}
; adds @racket[ke] to @racket[ak], the state of the world
 
(check-expect (remember "hello" " ") "hello ")
(check-expect (remember "hello " "w") "hello w")
 
(define (remember ak ke)
  (string-append ak ke))

; @tech{AllKeys} -> @tech/link{Image}
; renders the string as a text and place it into @racket[MT]
 
(check-expect (show "hel") (overlay (text "hel" 11 "red") MT))
(check-expect (show "mark") (overlay (text "mark" 11 "red") MT))
 
(define (show ak)
  (overlay (text ak 11 "red") MT))
))
@;%
}

@Figure-ref{fig:key-record} is the analogue to @figure-ref{fig:mouse-record}
 for key events. The program starts with some basic constant definitions and
 a data definition that says we are using strings to record the key events
 we have seen so far. The @racket[main] function specifies on event handler,
 namely one for keys. This key event handler, called @racket[remember],
 consumes the key events seen so far and a string that represents the
 last key pressed; it then appends the latter to the former. As with the
 previous program, the purpose of this one is to conduct experiments
 concerning key events. 

Experiments are conducted to reveal flaws in our understanding of the
world. So run the program and type This opens the world canvas, like this:
@centerline{@simple-key[]}
 enabling you to press keys and see them drawn there. For example, if you
 choose to press ``h'' the window displays this image: 
@centerline{@simple-key["h"]}
 And that may surprise you. The letter ``h'' shows up in the middle of the
 canvas, not the left margin. 

@exercise["design11a"]{Look through the functions in @tp{image} and find a way
to create framed, left-aligned text. Then change the program in
@figure-ref{fig:key-record} so that it uses this combination of image
primitives to render its state.}

@exercise["design11"]{Run the @racket[main] function again, press some
 regular keys on your keyboard, and then try the tab key or the
 delete---also known as rubout---key. The result of these actions is an
 application of the key event handlers to strings such as @racket["\t"] and
 @racket["\r"]. Appearances are deceiving, however. These strings consists
 of a single character and @racket[remember] therefore adds them to the end
 of the current world state. Read the documentation of @racket[on-key] to
 find out which strings belong to @tt{KeyEvent}; then use a @racket[cond]
 expression inside of @racket[remember] so that it ignores all key strokes
 represented by one-character strings.}
