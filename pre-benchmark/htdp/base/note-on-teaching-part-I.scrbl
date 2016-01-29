#lang scribble/manual

@(require "../base/Shared/shared.rkt" (prefix-in 2: 2htdp/image) "../base/Shared/shared1.rkt")

@(define (tech/link x)
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
@title[#:tag "note:teaching"]{A Note on Teaching Part I}

Teaching the material in @secref{part:prologue} and @secref{part:one} calls
for different approaches, depending on the context. At the college level,
instructors do not have the time to re-introduce students to functions and
their workings; they also need to get to material that makes the
design-oriented approach to programming look relevant. In contrast, a
teacher at a K-12 school must realize that few of his students will end up
as programmers but all of them ought to benefit from an introduction to
systematic problem solving as presented in this book. Finally, the line
between these two situations is not a sharp separator; some college
instructors may wish to focus on strengthening their students' skills and
some high school teachers may wish to provide a college-level experience
for theirs. The two sections of this note sketch out two points on this
scale. For more advice, consider sending email to our education list. 

@; -----------------------------------------------------------------------------
@section[#:tag "college"]{Dealing with Part I: College}

For a college-level audience, we recommend to start with
@secref{part:prologue}, but skipping @secref{sec:not} initially. 

After finishing @secref{part:prologue}, we show students how
@racket[animate] works by translating @racket[create-rocket-scene] into a
@racket[big-bang] program. We explain @racket[big-bang] as a small sliver
of the underlying operating system, the part that takes care of clock
ticks and key strokes and other things. Instead of covering all of its
features, lectures introduce them as needed on an informal basis. 

Once @racket[big-bang] is covered, we continue in the same informal style
to cover the basic topics: 
@;
@itemlist[ @item{numbers, 
Booleans, strings and images (@secref{ch:basic-arithmetic});}

@item{intervals and enumerations (@secref{ch:intervals-enums});}

@item{and structure types (@secref{ch:structure}).}
]
We typically assign the matching chapters as reading. 

For each topic, a lecture may start with a sample problem like the one in
@secref{part:prologue} that motivates the new topic: 
@itemlist[

@item{For example, a lecture on @secref{ch:basic-arithmetic} may wish to
focus on @secref{sec:arith-str} with a problem like this
one:@margin-note*{If this problem looks overwhelming, start simple. Allow
the program to collect an arbitrary number of digits and don't stop it
after @racket[s] seconds. Add those properties after the first solution runs.}
@;
@sample-problem{Design a world program that collects numeric key
strokes in a given interval. The program also stops when it has collected
five digits. It returns the digits as one string.}
@;
@Figure-ref{fig:collect-digits} shows a solution. Note how studying this
problem naturally calls for using features and functions from across @bsl[],
not just string-manipulation functions. It also requires looking up what
functions are available in @bsl[] because the solution calls for
@racket[string-numeric?], @racket[string-append], and so on.

@figure["fig:collect-digits" "Collecting digits"]{
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(require 2htdp/universe)
(require 2htdp/image)

;; collect up to five digits for s seconds 
(define (collect5 s)
  (big-bang ""
            (to-draw render)
            (on-tick do-nothing 1 s)
            (on-key record)
            (stop-when 5digits?)))

(define (render digits-so-far)
  (overlay (text digits-so-far 22 "blue") 
           (rectangle 200 30 "solid" "red")))

(define (record digits-so-far ke)
  (cond
    [(string-numeric? ke) (string-append digits-so-far ke)]
    [else digits-so-far]))

(define (5digits? digits-so-far)
  (= (string-length digits-so-far) 5))

(define (do-nothing s)
  s)))
@;%
}
}

@item{To introduce intervals and enumerations, consider a variation of the
launch problem from @secref{part:prologue}: 
@;
@sample-problem{Design a world program that simulates a count-down and the
lift-off of a rocket. Assume the program is given the number of seconds
before lift-off and that it shows the flight of the rocket for three
seconds.}
This problem calls for using two intervals: the one before lift-off and the
one after.

Also consider the simulation of a traffic light that cycles through the
three primary colors. Students can draw a schematic traffic light or they
can get creative.} 

@item{Structures come into play when a problem deals with two objects: 
@; 
@sample-problem{Revise the rocket launch program so that it also shows a
satellite that moves from left to right at a constant speed. Of course the
satellite re-appears on the left once it reaches the right boundary of the
scene.}
@;
The solution@margin-note*{Technically, a @bsl[] programmer could solve this
problem without structures, using complex numbers instead. Say so for the
knowledgeable students and then move on.} of this problem calls for a
structure that combines the state of the rocket with the position of the
satellite, meaning two numbers.}
]

This approach to covering the first part of the book unquestionably
exhausts the students. By the time, the lecture cover structure type
definitions, they will hang on for their dear life. @bold{This state is the
goal of the exercise.} Now have them read @secref{sec:not} and tell them
that a good introduction to programming needs a systematic
approach. Otherwise they will drown in a flood of details. 

At this point your students ought to be receptive to a lecture 

@centerline{@bold{From Chaos to Order}}

This lecture should organize what they have learned so far, provide a
framework for future readings and lectures, and introduce the idea of
developing programs systematically. Here are the key topics to cover: 
@itemlist[

@item{Programming needs a programming language. If a student has studied a
foreign language before, he/she knows that the acquisition of a language
starts with some basic @defterm{vocabulary} and a @defterm{grammar}. Once a
student of a language can form sentences, he/she also need a way to
understanding the meaning of sentences.

This book starts with @bsl[], a language tailored to the needs of
novices. The whole vocabulary and grammar can be found in the documentation
of @bsl[]. The vocabulary includes keywords such as @racket[define] and
@racket[cond] as well as primitive or built-in functions such as @racket[+]
and @racket[string-append].}

@item{Computer scientists can explain the meaning of programs in many
different ways. Sometimes a programmer must understand how a program
affects some specific computer. At other times, the computer hardware is
assumed to be something generic but the programmer needs to understand how
a program uses this generic hardware. But most of the time, programmers
have no such needs, in which case they can focus on how the functions of
the underlying language work on its data.

For @bsl[], the meaning is easiest to understand in terms of the rules from
a middle school pre-algebra course. The most important rule is function
application, which says that @math{f(5)} is equal to the definition of
@math{f} with @math{x} replaced by @math{5}. For primitive functions
students just need to know what they do.

@dr[] comes with the @bold{stepper}. If you haven't shown the stepper yet,
now is the time. Instead of scaring students with blackboard rules from
pre-algebra, just show them how @bsl[] programs compute with a series of
small examples. Start with a one-line example like a polynomial and work
your way to a simple @racket[big-bang] function. The key is to let students
know that this is how @dr[] views programs, that it does so automatically,
and that if in doubt, they can mimic it with the stepper or by hand.}

@item{Languages in computing are often supplemented with libraries. While
we could have baked all functions into @bsl[], students must find out at
some point how important knowledge of libraries is. Show them the
documentation for @tp{2htdp/image} and encourage them to play with the
functions.} 

@item{Finally, when a programmer has a basic understanding of the language,
its meaning, and its libraries, it is time to study what programming is all
about. In a sense, it is the formulation of sentences in the language. When
it comes to programming, formulating sentences can be done systematically,
starting with the problem statement all the way to complete code.

Now it's time to start covering the design recipes for functions and
programs, as spelled out in @secref{ch:htdp}. The space of design recipes
is two-dimensional: 
@itemlist[
@item{one dimension covers the series of activities}
@item{and the other one arranges data in a series of increasingly complex forms.}
]
Additional dimensions concern abstraction and efficiency concerns.}

]
@;
Of these four elements, the last one is the key point. Students must
understand that picking up a systematic approach to programming is the
key. In particular, the process dimension of the design recipe must become
second nature so much so that they recognize how it applies to programming
in other languages than @bsl[] and to larger pieces of code than the
programs they write in response to problems in this book. As a matter of
fact, students who will never program again should explain how the
process dimension of the design recipe may apply to the ordinary tasks of
doctors, journalists, lawyers, engineers, and perhaps even artists. 

At this point, it is best to switch over to a full-fledged coverage of the
book, starting with @secref{ch:structure}. 

@; -----------------------------------------------------------------------------
@section[#:tag "k12"]{Dealing with Part I: K-12}
