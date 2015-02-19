#lang scribble/manual

@(require "../base/Shared/shared.rkt" "../base/Shared/shared1.rkt")

@; -----------------------------------------------------------------------------
@intermezzo[#:unnumbered? #t #:tag "part:prologue"]{Prologue: How to Program}

@; ---------------------------------
@margin-note{Consider a quick look
  at @secref{note:teaching}.}
@; ---------------------------------

When you were a small child, your parents probably taught you to count and
 later to perform simple calculations with your fingers: ``1 + 1 is 2''; ``1 +
 2 is 3''; and so on. Then they would ask ``what's 3 + 2'' and you would count
 off the fingers of one hand. They programmed, and you computed. And in
 some way, that's really all there is to programming and computing.

@margin-note*{Start @dr[] and select ``Choose language'' from the
 ``Language'' menu. This brings up a dialog listing ``Teaching Languages''
 for ``How to Design Programs'' (and possibly other books). Click
 ``Beginning Student Language'' and ``OK'' to set up @dr[] for this chapter.}

Now you're switching roles. You program, and the computer is a child. Of
 course, you start with the simplest of all calculations. You type 
@racketblock[(+ 1 1)]
 into the top part of @dr[], click @button{Run}, and a result shows up in the
 bottom part: @racketresult[2] 

@image{../base/Images/drracket-plain.png}

That's how simple programming is. You ask questions as if @dr[] were
 a child, and @dr[] computes for you. @margin-note*{People often say the
 ``computer does X for you'' but in reality, a plain computer is pretty
 useless.  It is @bold{software} that computes.}
 You can also ask @dr[] to process several 
 requests at once:  
@racketblock[
(+ 2 2)
(* 3 3)
(- 4 2)
(/ 6 2)
]
 After you click @button{Run}, you see 
@racketresult[
4
9
2
3]
 in the bottom half of @dr[], which are the expected results. 

@bold{Terminology} At this point, we slow down for a moment and introduce
some terms:  
@itemlist[

@item{The top-half of @dr[] is called the @defterm{definitions area}. In
 this area, you create the programs, which is called @defterm{editing}. As
 soon as you add a word or change something in the definitions area, the
 @button{Save} button shows up in the top-left corner. When you click
 @button{Save} for the first time, @dr[] asks you for the name of a file so
 that it can store your program for good. Once your definitions area is
 associated with a file, clicking @button{Save} ensures that the
 content of the definitions area is stored safely in the file.}

@item{@defterm{Programs} consist of @defterm{expressions}.  You have seen
 expressions in mathematics. For now, an expression is either a plain
 number or something that starts with a left parenthesis ``('' and ends in
 a matching right parenthesis ``)''---which @dr[] rewards by shading the
 area between the pair of parentheses.}

@item{When you click @button{Run}, @dr[] evaluates the expressions in the
 definitions area and shows their result in the @defterm{interactions
 area}. Then, @dr[], your faithful servant, awaits your commands at the
 @defterm{prompt}. The appearance of the prompt signals that @dr[] is
 waiting for you to enter additional expressions, which it then evaluates
 like those in the definitions area: 
@interaction[
#:eval (bsl-eval)
@; ---------------------------------
(+ 1 1)
]
 Enter an expression at the prompt, hit the ``return'' or ``enter'' key on
 your keyboard, and watch how @dr[] responds with the result. You can do
 so as often as you wish: 
@;
@interaction[
#:eval (bsl-eval)
@; ---------------------------------
(+ 2 2)
(* 3 3)
(- 4 2)
(/ 6 2)
(sqr 3)
(expt 2 3)
(sin 0)
(cos pi)
]}
]
 Take a close look at the last number. Its ``#i'' prefix is short for ``I
 don't really know the precise number so take that for now'' or
 @defterm{inexact number}. Unlike your
 calculator or most other programming systems, @dr[] is extremely
 honest. When it doesn't know the exact number, it warns you with this
 special prefix. Later, we shall show you really strange facts about
 ``computer numbers,'' and you will then truly appreciate that @dr[]
 issues such warnings.

 Enough terminology for now. 

By now, you might be wondering whether @dr[] can add more than two
 numbers at once, and yes, it can! As a matter of fact, it can do it in two
 different ways:
@interaction[
#:eval (bsl-eval)
(+ 2 (+ 3 4))
(+ 2 3 4)
]
 The first one is @defterm{nested arithmetic}, as you know it from school. 
 The second one is @defterm{@sch[] arithmetic} and it is natural, if you
 always use parentheses to group operations and numbers 
 together. This is as a good a time as any to discuss the nature of
 our notation---dubbed @defterm{Beginning Student Language} or just
 @defterm{@bsl[]}---something you might have pondered for a while now.  

@margin-note*{This book does not teach you Racket, even if the software is
 called @dr[]. Instead it uses a series of teaching languages created for
 learning design principles.  Once you have mastered these languages, you
 can quickly learn to program in all kinds of programming languages,
 including Racket and also JavaScript, Python, Ruby, and others.}

In @bsl[], every time you want to use a ``calculator operation,'' you write
 down an opening parenthesis followed by the operation, the numbers on
 which the operation should work (separated by spaces or even line breaks),
 and ended by a closing parenthesis. The items following the operation are
 called the @defterm{operands}. Nested arithmetic means that you can use an
 expression for an operand, which is why
@interaction[
#:eval (bsl-eval)
(+ 2 (+ 3 4))
]
 is a fine program. You can do this as often as you wish: 
@interaction[
#:eval (bsl-eval)
(+ 2 (+ (* 3 3) 4))
(+ 2 (+ (* 3 (/ 12 4)) 4))
(+ (* 5 5) (+ (* 3 (/ 12 4)) 4))
]
 There are no limits to nesting, except for your patience.
 
Naturally, when @dr{} calculates for you, it uses the rules that you know
 and love from math. Like you, it can determine the result of an addition
 only when all the operands are plain numbers. If an operand is a
 parenthesized operator expression---something that starts with a ``('' and
 an operation---it determines the result of that nested expression
 first. Unlike you, it never needs to ponder which expression to
 calculate first---because this first rule is the only rule there is to
 it. 

The price for @dr{}'s convenience is that parentheses have meaning.  You,
 the programmer, must enter all these parentheses, and you may not enter too
 many. For example, while extra parentheses are acceptable to your math
 teacher, this is not the case for @bsl[]. The expression @racket[(+ (1) (2))]
 contains way too many parentheses, and @dr[] lets you know in no uncertain terms:
@interaction[
#:eval (bsl-eval)
(+ (1) (2))
] 

Once you get used to @bsl[] programming though, you will see that it
 isn't a price at all. First, you get to use operations on several
 operands at once, if it is natural to do so:@margin-note*{Or you place the
 cursor next to the operation and hit F1. This action opens @dr[]'s @help[]
 and searches for the documentation of the operation. Use the results
 concerning the HtDP teaching languages. As you may have noticed by now,
 this text is also linked to the documentation in @help[].}
@;
@interaction[
#:eval (bsl-eval)
(+ 1 2 3 4 5 6 7 8 9 0)
(* 1 2 3 4 5 6 7 8 9 0)
]
 If you don't know what an operation does for several operands, enter an
 example into the interactions area and hit @key{return}; @dr{} lets
 you know whether and how it works.  Second, when you will read programs that
 others write---which you will do for about half the time of your
 programmer life---you will never have to wonder which expressions are
 evaluated first. The parentheses and the nesting will immediately tell
 you so. 

In this context, to program is to write down comprehensible, arithmetic
 expressions, and to compute is to determine their value. With @dr[], it is
 easy to explore this kind of programming and computing.

@isection[#:tag "arithmetic-pro"]{Arithmetic and Arithmetic}

If programming were just about numbers and arithmetic, it would be as
 boring as mathematics.@margin-note*{Just kidding: mathematics is a fascinating
 subject, but you knew that. You won't need too much of it for now, though
 if you want to be a really great programmer, you will need to study some.}
 Fortunately, there is much more to programming than numbers: text,
 truths, images, and more.

Here are three programs that deal with text: 
@racketblock[
(string-append "hello" "world")
(string-append "hello " "world")
(string-append "hell" "o world")
]
 After you click @button{Run}, @dr{} displays three results:
@racketresult["helloworld" "hello world" "hello world"]

 To understand exactly what's going on, you first need to know that in @bsl[],
 text is any sequence of keyboard characters enclosed in double-quotes
 ("). Technically, this is called a string. Thus, 
 @racket["hello world"], is a perfectly fine string and, when @dr{} evaluates this
 string, it displays it in the interactions area, just like a number. 
 Indeed, many people's first program is one that displays the words
 ``hello'' and ``world''---you wrote three of them already but the simplest one is to type
 in the string by itself:  
@racketblock[
"hello world"
]
 Click @button{Run} and admire the output of the program. 

Otherwise, you need to know that in addition to an arithmetic of numbers,
 @dr{} also knows about an arithmetic of strings. Thus,
 @racket[string-append] is an operation just like @racket[+]; it makes a
 string by adding the second to the end of the first. As the first line
 shows, it does this literally, without adding anything between the two
 strings: no blank space, no comma, nothing. Thus, if you want to see the
 phrase @racket["hello world"], you really need to add a space to one of
 these words somewhere; that's what the second and third line show. Of
 course, the most natural way to create this phrase from the two words is
 to enter 
@racketblock[
(string-append "hello" " " "world")
]
 because @racket[string-append], like @racket[+], can deal with as many
 operands as you wish. 

You can do more with strings than append them. You can extract pieces from
 a string; reverse them; render all letters uppercase (or lowercase); strip
 blanks spaces from the left and right; and so on. And best of all, you
 don't have to memorize any of that. If you need to know what you can do
 with strings, look it up in @help[]. @margin-note*{Use F1 or the drop-down
 menu on the right to open @help[], look at the manuals for HtDP languages
 (@bsl[]) and its section on primitives. It lists all the operations in @bsl[]
 and especially those that work on strings.}

If you did look up the primitive operations of @bsl[], you saw that
 @defterm{primitive} (also called @defterm{built-in}) operations can consume
 strings and produce numbers. You therefore can, if you so desire,
 add the length of a string to 20:
@interaction[
#:eval (bsl-eval)
(+ (string-length "hello world") 20)
]
 and @dr{} evaluates this expressions like any other one. 
 That is, an arithmetic operation doesn't have to be about just numbers or
 just strings. Many of them mix and match as needed. And then there are
 operations that convert strings into numbers and numbers into strings and
 you get what you expect:  
@interaction[
#:eval (bsl-eval)

(number->string 42)
(string->number "42")
]
 If you expected ``forty-two'' or something clever along those lines,
 sorry, that's really not what you want from a string calculator.  

The second expression raises a question, though. What if @racket[string->number]
 isn't used with a string that is a number wrapped in string quotes? In that
 case the operation produces a totally different kind of result:
@interaction[
#:eval (bsl-eval)

(string->number "hello world")
]
 This is neither a number nor a string; it is a Boolean. Unlike numbers
 and strings, Boolean values come in only two varieties: @racket[#true] and
 @racket[#false]. The first is truth, the second falsehood. Even so, @dr{}
 has several operations for combining Boolean values:  
@interaction[
#:eval (bsl-eval)

(and #true #true)
(and #true #false)
(or #true #false)
(or #false #false)
(not #false)
]
 and you get the results that the name of the operation suggests. (Don't
 know what @racket[and], @racket[or], and @racket[not] compute? Easy:
 @racket[(and x y)] is true if @racket[x] and @racket[y] are true; 
 @racket[(or x y)] is true if either @racket[x] or @racket[y] or both are 
 true; and @racket[(not x)] results in @racket[#true] precisely when 
 @racket[x] is @racket[#false].) 

Although it isn't possible to convert one number into a Boolean, it is certainly
 useful to ``convert'' two numbers into a Boolean: 
@racketblock[
(> 10 9)
(< -1 0)
(= 42 9)
]
 Guess what the results are before you move on: 

@racketresult[#true]

@racketresult[#true]

@racketresult[#false]

 Now try these: @racket[(>= 10 10)], @racket[(<= -1 0)], and
 @racket[(string=? "design" "tinker")]. This last one is totally different
 again but don't worry, you can do it. 

With all these new kinds of data---yes, numbers, strings, and
 Boolean values are data---and operations floating around, it is easy to
 forget some basics, like nested arithmetic:
@racketblock[
(and (or (= (string-length "hello world") (string->number "11"))
         (string=? "hello world" "good morning"))
     (>= (+ (string-length "hello world") 60) 80))
]
 What is the result of this expression? How did you figure it out? All by
 yourself? Or did you just type it into @dr{}'s interactions area
 and hit the @key{return} key? If you did the latter, do you think you would
 know how to do this on your own? After all, if you can't predict what
 @dr{} does for small expressions, you may not want to trust it when
 you submit larger tasks than that for evaluation. 

Before we show you how to do some ``real'' programming, let's discuss one
 more kind of data to spice things up: images. @margin-note*{To insert
 images such as this rocket into @dr[], use the @menu{Insert} menu and
 select the ``Insert image ...''  item. Or, if you're reading this book
 on-line, copy-and-paste the image from your browser into @dr[].} When you
 insert an image into the interactions area and hit return like this
@;%
@(begin
#reader scribble/comment-reader
(racketblock
#, @prompt #, @rocket[]
))
@;% 
 @dr[] replies with the image.  In contrast to many other programming
 languages, @bsl[] understands images, and it supports an arithmetic of
 images just as it supports an arithmetic of numbers or strings. In short,
 your programs can calculate with images, and you can do so in the
 interactions area. Furthermore @bsl[] programmers---like the programmers for
 other programming languages---create @defterm{libraries} that others may
 find helpful. Using such libraries is just like expanding your
 vocabularies with new words or your programming vocabulary with new
 primitives. We dub such libraries @defterm{teachpacks} because they are
 helpful with teaching.

@margin-note*{@racket[(require 2htdp/image)] specifies that you wish to add
 the definitions of @tp{image} to your program. Alternatively, use the
 ``Language'' drop-down menu, choose ``Add Teachpack ...'' and pick
 @filepath{2htdp/image} from the @tt{Preinstalled HtDP/2e Teachpack} menu.} 
One important library---@tp{image}---supports operations for computing
 the width and height of an image:
@racketblock[
(* (image-width (unsyntax @rocket[]))
   (image-height (unsyntax @rocket[])))
]
 Once you have added a library to your program, clicking @button{Run} gives
 you 
@racketresult[1176]
 because that's the area of a 28 by 42 image. 

You don't have to use Google to find images and insert them in your @dr{}
 programs with the ``Insert'' menu. You can also instruct @dr{} to
 create simple images from scratch:  
@interaction[
#:eval (bsl-eval)
(circle 10 "solid" "red")
(rectangle 30 20 "outline" "blue")
]
 Best of all, @dr[] doesn't just draw images, because it really considers
 them values just like numbers. So naturally @bsl[] has operations for
 combining images just like it has operations for adding numbers or
 appending strings: 
@interaction[
#:eval (bsl-eval)
(overlay (circle 5 "solid" "red")
         (rectangle 20 20 "solid" "blue"))
]
Overlaying the same images in the opposite order produces a solid blue
 square: 
@interaction[
#:eval (bsl-eval)
(overlay (rectangle 20 20 "solid" "blue")
         (circle 5 "solid" "red"))
]
 Stop and reflect on this last result for a moment. 

As you can see @racket[overlay] is more like @racket[string-append] than
 @racket[+], but it does ``add'' images just like @racket[string-append]
 ``adds'' strings and @racket[+] adds numbers. Here is another illustration
 of the idea: 
@interaction[
#:eval (bsl-eval)
(image-width (square 10 "solid" "red"))
(image-width 
  (overlay (rectangle 20 20 "solid" "blue")
           (circle 5 "solid" "red")))
]
 These interactions with @dr[] don't draw anything at all; they really just 
 measure their width.

You should know about two more operations: @racket[empty-scene] and
 @racket[place-image]. The first creates a scene, a special kind of
 rectangle. The second places an image into such a scene: 
@racketblock[
(place-image (circle 5 "solid" "green")
             50 80
	     (empty-scene 100 100))
]
 and you get this: @margin-note*{Not quite. The image comes without a
 grid. We superimpose the grid on the empty scene so that you can see
 where exactly the green dot is placed.}
@centerline{@grid-scene[]}
 As you can see from this image, the origin (or (0,0)) is in the upper-left
 corner. Unlike in mathematics, the @tt{y} coordinate is measured
 @bold{downwards}, not upwards. Otherwise, the  image shows what you should
 have expected: a solid green disk at the coordinates (50,80) in a 100 by
 100 empty rectangle.  

Let's summarize again. To program is to write down an arithmetic
 expression, but you're no longer restricted to boring numbers. With @bsl[],
 your arithmetic is the arithmetic of numbers, strings, Boolean values, and
 even images. To compute though still means to determine the value of the
 expressions(s) except that this value can be a string, a number, a
 Boolean, or an image.

And now you're basically ready to write programs that make rockets fly.

@isection[#:tag "some-i/o"]{Inputs and Output}

The programs you have written so far are pretty boring.  You write down an
expression or several expressions; you click @button{Run}; you see some
results. If you click @button{Run} again, you see the exact same results. As
a matter of fact, you can click @button{Run} as often as you want, and the
same results show up. In short, your programs really are like calculations
on a pocket calculator, except that @dr{} calculates with all kinds
of data not just numbers. 

That's good news and bad news. It is good because programming and computing
ought to be a natural generalization of using a calculator. It is bad
because the purpose of programming is to deal with lots of data and to get
lots of different results, with more or less the same calculations. (It
should also compute these results quickly, at least faster than we can.)
That is, you need to learn more still before you know how to program. No
need to worry though: with all your knowledge about arithmetic of numbers,
strings, Boolean values, and images, you're almost ready to write a program
that creates movies, not just some silly program for displaying ``hello
world'' somewhere. And that's what we're going to do next.

Just in case you didn't know, a movie is a sequence of images that are
rapidly displayed in order. If your algebra teachers had known about the
``arithmetic of images'' that you saw in the preceding section, you could
have produced movies in algebra instead of boring number
sequences. Remember those tables that your teachers would show you? Here is
one more: 
@rtable[9]
Your teachers would now ask you to fill in the blank, i.e., replace the ``?''
mark with a number. 

It turns out that making a movie is no more complicated than completing a
 table of numbers like that. Indeed, it is all about such tables:

@mrtable[5]

 To be concrete, your teacher should ask you here to draw the sixth image,
 the seventh, and the 1273rd one because a movie is just a lot of
 images, some 20 or 30 of them per second. So you need some 1200 to 1800
 of them to make one minute's worth of it.

You may also recall that your teacher not only asked for the fifth, sixth,
 or seventh number in some sequence but also for an expression that
 determines any element of the sequence from a given @math{x}.  In the
 numeric example, the teacher wants to see something like
 this:@margin-note*{Write @math{x^2} if you want to be fancy.}
@;
@centerline{@image{../base/Images/xsqr.png}}
@;
 If you plug in 1, 2, 3, and so on for @math{x}, you get 1, 4, 9, and so on for
 @math{y}---just as the table says. For the sequence of images, you could say
 something like
@;
@centerline{@math{y = } the image that contains a dot @math{x^2} pixels below the top.}
@;
 The key is that these one-liners are not just expressions but functions.

At first glance functions are like expressions, always with a @math{y} on the
 left, followed by an @math{=} sign, and an expression. They aren't expressions,
 however. And the notation you (usually) learn in school for functions is
 utterly misleading. In @dr{}, you therefore write functions a bit
 differently: 
@racketblock[
(define (y x) (* x x))
]
 The @racket[define] says ``consider @racket[y] a function'', which like
 an expression, computes a value. A function's value, though, depends on
 the value of something called the @defterm{input}, which we express with 
 @racket[(y x)]. Since we
 don't know what this input is, we use a name to represent the
 input. Following the mathematical tradition, we use @racket[x] here
 to stand in for the unknown input but pretty soon, we shall use all kinds
 of names.

This second part means you must supply one value---a number---for
 @racket[x] to determine a specific value for @racket[y]. When you do,
 @dr{} plugs in the value for @racket[x] into the expression
 associated with the function. Here the expression is @racket[(* x
 x)]. Once @racket[x] is replaced with a value, say @racket[1], @dr{}
 can compute the result of the expressions, which is also called the
 @defterm{output} of the function.

Click @button{Run} and watch nothing happen. Nothing shows up in the
 interactions area. Nothing seems to change anywhere else in
 @dr{}. It is as if you hadn't accomplished anything. But you
 did. You actually defined a function and informed @dr{} about its
 existence. As a matter of fact, the latter is now ready for you to use the 
 function. Enter  
@racketblock[
(y 1)
]
 at the prompt in the interactions area and watch a @racket[1] appear in
 response. The @racket[(y 1)] is called a @defterm{function application} in
 @dr{}.@margin-note*{... and in mathematics, too. Your teachers just
 forgot to tell you.} Try
@racketblock[
(y 2)
]
 and see a @racket[4] pop out. Of course, you can also enter all these
 expressions in the definitions area and click @button{Run}:
@racketblock[
(define (y x) (* x x))

(y 1)
(y 2)
(y 3)
(y 4)
(y 5)
]
 In response, @dr{} displays:
@racketresult[
1
4
9
16
25],
 which are the numbers from the table. Now determine the missing entry. 


What all this means for you is that functions provide a rather economic way
 of computing lots of interesting values with a single expression. Indeed,
 programs are functions, and once you understand functions well, you know
 almost everything there is about programming. Given their importance,
 let's recap what we know about functions so far:
@itemlist[
@item{First, 
@;
@centerline{@racket[(define (_FunctionName _InputName) _BodyExpression)]}
@;
 is a @defterm{function definition}. You recognize it as such, because it
 starts with the ``@racket[define]'' keyword. A function definition
 consists of three pieces: two names and an expression. The first name is
 the name of the function; you need it to apply the function as often as
 you wish. The second name---most programmers call it a
 @defterm{parameter}---represents the input of the function, which is
 unknown until you apply the function. The expression, dubbed
 @defterm{body} computes the output of the function for a specific
 input. As seen, the expression involves the parameter, and it may also
 consist of many other expressions.}

@item{Second, 
@;
@centerline{@racket[(_FunctionName _ArgumentExpression)]}
@;
 is a @defterm{function application}. The first part tells @dr{} which
 function you wish to use. The second part is the input to which you wish
 to apply the function. If you were reading a Windows or Mac manual, it
 might tell you that this expression ``launches'' the (software)
 ``application'' called @racket[_FunctionName] and that it is going to
 process @racket[_ArgumentExpression] as the input. Like all expressions,
 the latter is possibly a plain piece of data (number, string, image,
 Boolean) or a complex, deeply nested expression. }
]

Functions can input more than numbers, and they can output all kinds of
 data, too. Our next task is to create a function that simulates the
 second table---the one with images of a colored dot---just like the first
 function simulated the numeric table.  Since the creation of images from
 expressions isn't something you know from high school, let's start
 simply. Do you remember @racket[empty-scene]? We quickly mentioned it at
 the end of the previous section. When you type it into the definitions
 area, like that:
@racketblock[
(empty-scene 100 100)
]
 clicking @button{Run} produces an empty rectangle, also called a scene: 

@rocketi[0]

 You can add images to a scene with @racket[place-image]:
@racketblock[
(place-image #, @rocket[] 50 0 (empty-scene 100 100))
]
 produces an scene with a rocket hovering near the center of the top: 

@rocketi[1]

Think of the rocket as an object that is like the dot---though more
interesting---in the above table from your mathematics class. 

Next you should make the rocket descend, just like the dot in the above
table. From the preceding section you know how to achieve this effect by
increasing the @math{y} coordinate that is supplied to @racket[place-image]: 
@racketblock[
(place-image (unsyntax @rocket[]) 50 10 (empty-scene 100 100))
(place-image (unsyntax @rocket[]) 50 20 (empty-scene 100 100))
(place-image (unsyntax @rocket[]) 50 30 (empty-scene 100 100))
]
 Clicking @button{Run} yields three scenes: 

@rocketi[2]

@rocketi[3]

@rocketi[4]

 All that's needed now is to produce lots of these scenes easily and to
 display all of them in rapid order. 

The first goal can be achieved with a function of course: 
@racketblock[
(define (create-rocket-scene height)
  (place-image (unsyntax @rocket[]) 50 height (empty-scene 100 100)))
]
 Yes, this is a function definition. Instead of @racket[y], it uses the
 name @racket[create-rocket-scene], a name that immediately tells you what
 the function outputs: a scene with a rocket. Instead of @racket[x], the
 function definition uses @racket[height] for the name of its parameter, a
 name that suggests that it is a number and that it tells the function
 where to place the rocket. The body expression of the function is just
 like the series of expressions with which we just experimented, except
 that it uses @racket[height] in place of a number. And we can easily
 create all of those images with this one function:
@racketblock[
(create-rocket-scene 0)
(create-rocket-scene 10)
(create-rocket-scene 20)
(create-rocket-scene 30)
]
 Try this out in the definitions area or the interactions area, both
 create the expected scenes.

@margin-note*{@racket[(require 2htdp/universe)]}
The second goal requires knowledge about one additional primitive operation
 from @tp{universe}: @racket[animate]. So, add the
 @racket[require] line to the definitions area, click @button{Run}, and
 enter the following expression: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
#, @prompt (animate create-rocket-scene)
))
@;%
 Stop for a moment and note that the argument expression is a function. Don't
 worry for now about using functions as arguments; it works well with
 @racket[animate] but don't try this at home yet. 

As soon as you hit the ``return'' key, @dr{} evaluates the expression but
 it does not display a result, not even an interactions prompt (for a
 while). It open another window---a @defterm{canvas}---and starts a clock
 that ticks 28 times per second. Every time the clock ticks, @dr[]
 applies @racket[create-rocket-scene] to the number of ticks passed since
 this function call. The results of these function calls are displayed in
 the canvas, and it produces the effect of an animated movie. The
 simulation runs until you click @button{Stop} or close the window. At that
 point, animate returns the number of ticks that have passed.
 
The question is where the images on the window come
 from.
 @;@margin-note*{@Exref{ex:animate} explains how to design
 @;@racket[animate].}
 The short explanation is that @racket[animate] runs its
 operand on the numbers @racket[0], @racket[1], @racket[2], etc. and
 displays the resulting images. The long explanation is this: 
@itemlist[

@item{@racket[animate] starts a clock, and @racket[animate] counts the
 number of clock ticks;}

@item{the clock ticks 28 times per second;}

@item{every time the clock ticks, @racket[animate] applies the function
 @racket[create-rocket-scene] to the current clock tick; and} 

@item{the scene that this application creates is displayed on the screen.}
]
 This means that the rocket first appears at height @racket[1], then
 @racket[2], then @racket[3], etc., which explains why the rocket descends
 from the top of the screen to the bottom. That is, our three-line program
 creates some 100 pictures in about 3.5 seconds, and displaying these
 pictures rapidly creates the effect of a rocket descending to the ground. 

Here is what you learned in this section. Functions are useful because they
 can process lots of data in a short time.  You can launch a function by
 hand on a few select inputs to ensure it produces the proper outputs. This
 is called testing a function. Or, @dr[] can launch a function on lots of
 inputs with the help of some libraries; when you do that, you are running
 the function. Naturally, @dr{} can launch functions when you press a key
 on your keyboard or when you manipulate the mouse of your computer.  To
 find out how, keep reading. Whatever triggers a function application isn't
 important, but do keep in mind that (simple) programs are just functions.

@isection[#:tag "pro-cond"]{Many Ways To Compute}

When you run the @racket[create-rocket-scene] program from the preceding
 section, the rocket eventually disappears in the ground. That's plain
 silly. Rockets in old science fiction movies don't sink into the ground;
 they gracefully land on their bottoms, and the movie should end right
 there. 

This idea suggests that computations should proceed differently, depending
 on the situation. In our example, the @racket[create-rocket-scene] program
 should work ``as is'' while the rocket is in-flight. When the rocket's
 bottom touches the bottom of the screen, however, it should stop the
 rocket from descending any further.

In a sense, the idea shouldn't be new to you. Even your mathematics
 teachers define functions that distinguish various situations:
@centerline{@image{../base/Images/sign.png}}
 This @math{sign} distinguishes three kinds of inputs: those
 numbers larger than 0, those equal to 0, and those smaller than 0. Depending on the
 input, the result of the function---or output as we may occasionally call
 it---is +1, 0, or -1.  

@margin-note*{Open a new tab in @dr{} and start with a clean slate.}

You can define this function in @dr{} without much ado using a
 @racket[cond]itional expression: 
@racketblock[
(define (sign x)
  (cond
    [(> x 0) +1]
    [(= x 0)  0]
    [(< x 0) -1]))

(sign 10)
(sign -5)
(sign 0)
]
 To illustrate how @racket[sign] works, we added three applications of the
 function to the definitions area. If you click @button{run}, you see
 @racketresult[1], @racketresult[-1], and  @racketresult[0]. 

In general, a @defterm{conditional expression} has the shape
@;
@racketblock[
  (cond
    [_ConditionExpression1 _ResultExpression1]
    [_ConditionExpression2 _ResultExpression2]
    ....
    [_ConditionExpressionN _ResultExpressionN])
]
 That is, a @racket[cond]itional expressions consists of many
 @defterm{conditional lines}. Each line contains two expressions: the left
 one is often called @defterm{condition} and the right one is called
 @defterm{result}.  @margin-note*{This is a good time to explore what the
 @button{step} button does. Click @button{step} for the above
 @racket[sign] program. When the new window comes up, click the right and
 left arrows there.} To evaluate a @racket[cond] expression, @dr{}
 evaluates the first condition expression, @racket[_ConditionExpression1].
 If this evaluation yields @racket[#true],
 @dr{} replaces the @racket[cond] expression with the first result
 expression (@racket[ResultExpression1]) and evaluates it. Whatever value
 @dr{} obtains is the result of the entire @racket[cond] expression.
 If the evaluation of @racket[_ConditionExpression1] yields @racket[#false],
 @dr{} drops the first line and moves on to the second line, which is
 treated just like the first one. In case all condition expressions
 evaluate to @racket[#false], @dr{} signals an error.

@(define (crs version) (format "fig:create-rocket-scene.v~a" version))
@(define (tit version) (format "Landing a rocket (version ~a)" version))
@figure[(crs 2) (tit 2)]{
@racketblock[
(define (create-rocket-scene.v2 height)
  (cond
    [(<= height 100) 
     (place-image (unsyntax @rocket[]) 50 height (empty-scene 100 100))]
    [(> height 100)
     (place-image (unsyntax @rocket[]) 50 100 (empty-scene 100 100))]))
]
}

With this knowledge, you can now change the course of the simulation. The
 goal is to not let the rocket descend below the ground level of a
 100 by 100 scene. Since the @racket[create-rocket-scene] function consumes
 the height at which it is to place the rocket in the scene, a simple test
 comparing the given height to the maximum height appears to suffice. 

See @figure-ref[(crs 2)] for the revised function definition.
 @margin-note*{In @bsl[], you can really use all kinds of characters in
 function names, including ``.'' or ``-'' which you have already seen.} We
 call the revised function @racket[create-rocket-scene.v2] to distinguish it from the original
 version. Doing so also allows us to use both functions in the interactions
 area of @dr[] and to compare their results:
@interaction[
@#:eval (bsl-eval 
[(require 2htdp/image)
 (define rocket (bitmap "../base/Images/rocket-s.jpg"))
 (define (create-rocket-scene height)
   (place-image rocket 50 height (empty-scene 100 100)))
 (define (create-rocket-scene.v2 height)
   (cond
     [(<= height 100) 
      (place-image rocket 50 height (empty-scene 100 100))]
     [(> height 100)
      (place-image rocket 50 100 (empty-scene 100 100))]))]
)
(create-rocket-scene 5555)
(create-rocket-scene.v2 5555)
]
 No matter what number over @racket[100] you give to
 @racket[create-rocket-scene.v2], you get the same scene. In
 particular, when you run the simulation
@racketblock[
#, @prompt (animate create-rocket-scene.v2)
]
 the rocket descends, sinks half way into the ground, and finally comes to
 a halt. 

Landing the rocket half-way under ground is ugly. Then again, you basically
 know how to fix this aspect of the program. As you learned from the
 preceding sections, @dr{} knows an arithmetic of images. Images have
 a center point and, when @racket[place-image] adds an image to a scene, it
 uses this center point as if it were the image. This explains why the
 rocket is half way under ground at the end: @dr{} thinks of the
 image as if it were a point, but the image has a real height and a real
 width. As you may recall, you can measure the height of an image with the
 operation @racket[image-height], which is to images like @racket[+] is to
 numbers. This function comes in handy here because you really want to fly
 the rocket only until its bottom touches the ground. 

Putting one and one together---also known as playing around---you can now
 figure out that 
@racketblock[
(- 100 (/ (image-height (unsyntax @rocket[])) 2))
]
 is the point at which you want the rocket to stop its descent. @bold{Hint} You
 could figure this out by playing with the program directly. Or you can
 experiment in the interactions area with your image arithmetic. Enter
 this expression, which is one natural guess: 
@racketblock[
(place-image (unsyntax @rocket[]) 50 (- 100 (image-height (unsyntax @rocket[]))) 
             (empty-scene 100 100))
]
 and then this one: 
@racketblock[
(place-image (unsyntax @rocket[]) 50 (- 100 (/ (image-height (unsyntax @rocket[])) 2))
             (empty-scene 100 100))
]
 Which result do you like better? 

@figure[(crs 3) (tit 3)]{
@racketblock[
(define (create-rocket-scene.v3 height)
  (cond
    [(<= height (- 100 (/ (image-height (unsyntax @rocket[])) 2))) 
     (place-image (unsyntax @rocket[]) 50 height (empty-scene 100 100))]
    [(> height (- 100 (/ (image-height (unsyntax @rocket[])) 2))) 
     (place-image (unsyntax @rocket[]) 50 (- 100 (/ (image-height (unsyntax @rocket[])) 2)) 
                  (empty-scene 100 100))]))
]
}

When you think and experiment along these lines, you eventually get to the
 program in @figure-ref[(crs 3)].
 Given some number, which represents the @racket[height] of the rocket, it
 first tests whether the rocket's bottom is above the ground. If it is, it
 places the rocket into the scene as before. If it isn't, it places the
 rocket's image so that its bottom touches the ground. 

@; -----------------------------------------------------------------------------
@isection[#:tag "pro-many-def"]{One Program, Many Definitions}

Now imagine this. Your manager at your hot game company doesn't like the
size of your program's canvas and requests a version that uses
@racket[200] by @racket[400] scenes.  This simple request forces you to
replace @racket[100] with @racket[400] in five places in the program, which
includes the @racket[animate] line, and to replace @racket[100] with
@racket[200] in three other places. Before you read on, try to do just that
so that you get an idea of how difficult it is to execute this request for
a five-line program. As you read on, keep in mind that real programs
consists of 50,000 or 500,000 or 5,000,000 lines of code.

In the ideal program, a small request, such as changing the sizes of the
 canvas, should require an equally small change. The tool to achieve this
 simplicity with @bsl[] is @racket[define]. In addition to defining functions,
 you can also introduce @defterm{constant definitions}, which assign some
 name to a constant. The general shape of a constant definition is
 straightforward: 
@centerline{@racket[(define _Name _Expression)]}
 Thus, for example, if you write down
@;
@racketblock[
(define HEIGHT 100)
]
 in your program, you are saying that @racket[HEIGHT] always
 represents the number @racket[100]. The meaning of such a definition is
 what you expect. Whenever @dr{} encounters @racket[HEIGHT] during its
 calculations, it uses @racket[100] instead.  Of course, you can also add 
@racketblock[
(define WIDTH 100)
]
 to your program to have one single place where you specify the width of
 the scene. 

@figure[(crs 4) (tit 4)]{
@racketblock[
(define (create-rocket-scene.v4 h)
  (cond
    [(<= h (- HEIGHT (/ (image-height (unsyntax @rocket[])) 2))) 
     (place-image (unsyntax @rocket[]) 50 h (empty-scene WIDTH HEIGHT))]
    [(> h (- HEIGHT (/ (image-height (unsyntax @rocket[])) 2))) 
     (place-image (unsyntax @rocket[]) 50 (- HEIGHT (/ (image-height (unsyntax @rocket[])) 2))
                  (empty-scene WIDTH HEIGHT))]))

(define WIDTH 100)
(define HEIGHT 100)
]}

Now take a look at the code in @figure-ref[(crs 4)], which implements this
 simple change. Copy the program into @dr[], and after clicking
 @button{RUN}, evaluate the following interaction: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
#, @prompt (animate create-rocket-scene.v4)
))
@;%
 Confirm that the program still functions as before. 

The program in @figure-ref[(crs 4)] consists of three definitions: one
 function definition and two constant definitions. The number
 @racket[100] occurs only twice: once as the value of @racket[WIDTH] and
 once as the value of @racket[HEIGHT].  The last line starts the
 simulation, just as in version 3 of the program. You may also have noticed
 that it uses @racket[h] instead of @racket[height] for the function
 parameter of @racket[create-rocket-scene.v4]. Strictly speaking, this
 change isn't necessary because @dr{} doesn't confuse @racket[height] with
 @racket[HEIGHT] but we did it to avoid confusing you.

When @dr{} evaluates @racket[(animate create-rocket-scene.v4)],
 it replaces @racket[HEIGHT] with @racket[100] and @racket[WIDTH] with @racket[100]
 every time it encounters these names. To experience the joys of real
 programmers, change the @racket[100] next to @racket[HEIGHT] into a @racket[400]
 and click @button{Run}. You see a rocket descending and landing in a 100 by 400
 scene. One small change did it all.

In modern parlance, you have just experienced your first @defterm{program
 refactoring}. Every time you re-organize your program to prepare yourself
 for likely future change requests, you refactor your program. Put it on
 your resume. It sounds good, and your future employer probably enjoys
 reading such buzzwords, even if it doesn't make you a good programmer. 
 What a good programmer would never live with, however, is that the program
 contains the same expression three times: 
@racketblock[
(- HEIGHT (/ (image-height (unsyntax @rocket[])) 2))
]
 Every time your friends and colleagues read this program, they need to
 understand what this expression computes, namely, the distance between the
 bottom of the screen and the center point of a rocket resting on the
 ground.  Every time @dr{} computes the value of the expressions it
 has to perform three arithmetic operations: (1) determine the height of the
 image; (2) divide it by @racket[2]; and (3) subtract the result from
 @racket[100]. And every time it comes up with the same number.

This observation calls for the introduction of one more definition to your
 program: 
@racketblock[
(define ROCKET-CENTER-TO-BOTTOM 
  (- HEIGHT (/ (image-height (unsyntax @rocket[])) 2)))
]
 plus the replacement of every other occurrence of @racket[(- HEIGHT (/
 (image-height (unsyntax @rocket[])) 2))] with @racket[ROCKET-CENTER-TO-BOTTOM]. 
 You might wonder whether the new definition should be placed
 above or below the definition for @racket[HEIGHT]. More generally, you should
 be wondering whether the ordering of definitions matters. The answer is that
 for constant definitions, the order matters, and for function definitions
 it doesn't. As soon as @dr{}  encounters a constant definition, it
 determines the value of the expression and then associates the name with
 this value. Thus, for example,  
@racketblock[
(define HEIGHT (* 2 CENTER))
(define CENTER 100)
]
 is meaningless because @dr{} hasn't seen the definition for
 @racket[CENTER] yet when it encounters the definition for
 @racket[HEIGHT]. In contrast, 
@racketblock[
(define CENTER 100)
(define HEIGHT (* 2 CENTER))
]
 works as expected. First, @dr{} associates @racket[CENTER] with
 @racket[100]. Second, it evaluates @racket[(* 2 CENTER)], which yields
 @racket[200]. Finally, @dr{} associates @racket[200] with
 @racket[HEIGHT]. 

While the order of constant definitions matters, it doesn't matter whether
 you first define constants and then functions or vice versa. Indeed, if
 your program consisted of more than one function, it wouldn't matter in
 which order you defined those. For pragmatic reasons, it is good to
 introduce all constant definitions first, followed by the definitions of
 important functions, with the less important ones bringing up the rear
 guard. When you start writing your own multi-definition programs, you will
 soon see why this ordering is a good idea.

@figure[(crs 5) (tit 5)]{
@(begin
#reader scribble/comment-reader
(racketblock
;; constants 
(define WIDTH  100)
(define HEIGHT 100)
(define MTSCN  (empty-scene WIDTH HEIGHT))
(define ROCKET (unsyntax @rocket[]))
(define ROCKET-CENTER-TO-BOTTOM 
  (- HEIGHT (/ (image-height ROCKET) 2)))

;; functions
(define (create-rocket-scene.v5 h)
  (cond
    [(<= h ROCKET-CENTER-TO-BOTTOM) 
     (place-image ROCKET 50 h MTSCN)]
    [(> h ROCKET-CENTER-TO-BOTTOM) 
     (place-image ROCKET 50 ROCKET-CENTER-TO-BOTTOM MTSCN)]))
))}


@margin-note*{The program also contains two @defterm{line comments},
 introduced with semi-colons (``;'').  While @dr[] ignores such comments,
 people who read programs should not because comments are intended for
 human readers. It is a ``back channel'' of communication between the
 author of the program and all of its future readers to convey information
 about the program.}
@;
Once you eliminate all repeated expressions, you get the program in
 @figure-ref[(crs 5)].  It consists of one function definition and five
 constant definitions.  Beyond the placement of the rocket's center, these
 constant definitions also factor out the image itself as well as the
 creation of the empty scene.

Before you read on, ponder the following changes to your program:
@itemlist[
 @item{How would you change the program to create a 200 by 400 scene?}

 @item{How would you change the program so that it depicts the landing of a
 green UFO (unidentified flying object)? Drawing the UFO per se is easy:

 @racketblock[(overlay (circle 10 "solid" "green")
                       (rectangle 40 4 "solid" "green"))]}

 @item{How would you change the program so that the background is always
 blue?}

 @item{How would you change the program so that the rocket lands on a flat rock
 bed that is 10 pixels higher than the bottom of the scene? Don't forget to
 change the scenery, too.}
]
 Better than pondering is doing. It's the only way to learn. So don't let
 us stop you. Just do it. 

@bold{Magic Numbers} Take another look at the definition of
 @racket[create-rocket-scene.v5]. As we eliminated all repeated
 expressions, all but one number disappeared from this function
 definition. In the world of programming, these numbers are called
 @defterm{magic numbers}, and nobody likes them. Before you know it, you
 forget what role the number plays and what changes are legitimate. It is
 best to name such numbers in a definition.

Here we actually know that @racket[50] is our whimsical choice for an
 @italic{x} coordinate for the rocket. Even though @racket[50] doesn't look
 like much of an expression, it actually is a repeated expression, too.
 In other words, we have two reasons to eliminate @racket[50] from the
 function definition, and we leave it to you to do so.

@; -----------------------------------------------------------------------------
@isection[#:tag "more-def"]{One More Definition}

@margin-note*{Danger ahead! This section introduces one piece of knowledge
 from physics. If physics scares you, skip this section on a first reading;
 programming doesn't require physics knowledge.}
@;
Real rockets don't descend at a constant speed. Real cars don't stop on the
 spot. They decelerate, which is the opposite of accelerate. What this
 really means is that an object first travels at a constant speed and then
 the driver hits the brakes or the pilot fires some engines. The effect of
 using the brakes or firing the engines is to change the speed
 slowly---decelerate in fancy English. 

To understand how this process works precisely, you must think back to your
 physics courses. If you haven't because you're too young or
 if you can't remember because you're too old or if you fell asleep in the
 course, you'll need to look in a physics book. Don't worry. This happens to
 programmers all the time because they need to help people in music,
 physics, mechanical engineering, economics, photography, and all kinds of
 other disciplines and, obviously, not even programmers know everything. So
 they look it up. Or they are told what deceleration means in terms of
 distance traveled: 
@centerline{@image{../base/Images/d.png}}
 That is, if @math{v} is the speed of the object and @math{a} is the
 deceleration---change of speed---then the object travels @math{d}
 miles (or meters or pixels or whatever) in @math{t} seconds. 

By now you know that a good teacher would have shown you a proper function
 definition: 
@centerline{@image{../base/Images/d2.png}}
 because this tells everyone immediately that the computation of @math{d}
 depends on @math{t} and that @math{v} and @math{a} are constants. A
 programmer goes even further and uses meaningful names for these
 one-letter abbreviations: 
@racketblock[
(define VELOCITY 20)
(define DECELERATION 1)

(define (distance t)
  (- (*  VELOCITY t) (* 1/2 DECELERATION (sqr t))))
]
 This program consists of three definitions: a function that computes the
 distance traveled by a decelerating object; its velocity or speed for you;
 and its change in speed or deceleration. The @racket[distance] function
 uses a primitive operator called @racket[sqr]; if you can't figure out
 what it does, play with it in the interactions area or look it up in
 @help[]. 

The next and only other thing you need to know is that
 @racket[animate] actually applies its functions to the number of
 clock ticks that have passed since it was first called and @bold{not} the
 height of the rocket image. From this revelation it immediately follows
 that our five versions of @racket[create-rocket-scene] have thus far used
 the wrong name for the input. Clearly, @racket[t]---short for time---would
 be much better than @racket[h], which is short for height:
@racketblock[
(define (create-rocket-scene t)
  (cond
    [(<= t ROCKET-CENTER-TO-BOTTOM) 
     (place-image ROCKET X t MTSCN)]
    [(> t ROCKET-CENTER-TO-BOTTOM) 
     (place-image ROCKET X ROCKET-CENTER-TO-BOTTOM MTSCN)]))
]
 More importantly, however, this small change to the definition also
 clarifies that this program doesn't compute how far the rocket has
 traveled in the given time; it uses the time as if it were a distance.

@figure[(crs 6) (tit 6)]{
@(begin
#reader scribble/comment-reader
(racketblock
;; properties of the ``world''
(define WIDTH  100)
(define HEIGHT 100)

;; properties of the descending rocket
(define VELOCITY 20) 
(define DECELERATION 1)

;; various other constants 
(define MTSCN  (empty-scene WIDTH HEIGHT))
(define ROCKET (unsyntax @rocket[]))
(define ROCKET-CENTER-TO-BOTTOM 
  (- HEIGHT (/ (image-height ROCKET) 2)))

(define X 50)

;; functions
(define (create-rocket-scene.v6 t)
  (cond
    [(<= (distance t) ROCKET-CENTER-TO-BOTTOM) 
     (place-image ROCKET X (distance t) MTSCN)]
    [(> (distance t) ROCKET-CENTER-TO-BOTTOM) 
     (place-image ROCKET X ROCKET-CENTER-TO-BOTTOM MTSCN)]))

(define (distance t)
  (- (* VELOCITY t) (* 1/2 DECELERATION (sqr t))))
))
}

Even if you have never taken a physics course, you know that a time is not
 a distance. So somehow our program worked by accident. Don't worry,
 though; it is all easy to fix. Instead of @racket[t], we use
 @racket[(distance t)], and we add the above definitions to our
 program. The final program is displayed in @figure-ref[(crs 6)].  It
 consists of two function definitions: @racket[create-rocket-scene.v6] and
 @racket[distance]. The remaining constant definitions make the function
 definitions readable and modifiable. As always, you can run the program by
 supplying one of its functions to @racket[animate]:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
#, @prompt (animate create-rocket-scene.v6)
))
@;%

In comparison to the previous versions of @racket[create-rocket-scene],
 this final version teaches you that a program may consist of more than one
 function definition and that one function definition
 (@racket[create-rocket-scene.v6]) can refer to other function
 definitions (@racket[distance]).

In a way, this revelation shouldn't surprise you. Even the first version of
 @racket[create-rocket-scene] used @racket[+] and @racket[/] and other
 functions. It's just that you think of those as built into
 @dr{}. While @racket[+] and @racket[/] are indeed an intrinsic part
 of the programming language, some others are not. For example, most of the
 ``image arithmetic'' and a good part of the ``string arithmetic'' are just
 function definitions that we created a long time ago and that are added to
 your definitions area when you click @button{Run}.

As you become a true blue programmer you will find out that programs consist of
 many function definitions and many constant definitions. You will also see
 that functions refer to each other all the time. What you really need to
 practice is to organize such collections of definitions so that you can
 read them easily even months after completion. After all, you or your
 manager will want to make changes to these programs, and if you don't know
 how to read them and if you didn't organize them well, you will have a
 difficult time with even the smallest task. Otherwise you mostly know what
 there is to know and you can program.

@isection[#:tag "program-now"]{You Are a Programmer Now}

The claim that you are a programmer may have come as a surprise to you at
 the end of the preceding section but it is true. You know all the
 mechanics that there is to know.  You know that programming---and
 computing---is about arithmetic of numbers, strings, images, and whatever
 other data your chosen programming languages support.  You know that
 programs consist of function and constant definitions. You know, because
 we have told you, that in the end, it's all about organizing these
 definitions properly. Last but not least, you know that @dr{} and the
 teachpacks support lots of other functions and that @dr{} @help[] explains
 what these functions do.

You might think that you still don't know enough to write programs that
 react to keystrokes, mouse clicks, and so on. As it turns out, you do. In
 addition to the @racket[animate] function, @tp{universe}
 provide other functions that hook up your programs to the keyboard, the
 mouse, the clock and other moving parts in your computer. Indeed, it even
 supports writing programs that connect your computer with anybody else's
 computer around the world. So this isn't really a problem. 

@margin-note{From a theoretical perspective, you are missing the ability to
 define functions that run forever. This may sound useless and difficult to
 achieve. It is neither but it is too early to introduce this idea
 concretely.}

@;  is neither. Here is how you define such a program:

@; @racketblock[
@; (define (run ul)
@;   (run 42))

@; (run 5)
@; ]

@;  If you click @button{Run}, you get no result.  Actually, you should
@;  immediately move the mouse to the @button{Stop} button, click, hold the
@;  mouse button down, and wait for @dr{} to stop your run-away
@;  program.}

In short, you have seen almost all the mechanics of putting together
 programs. If you read up on all the functions that are available, you can
 write programs that play interesting computer games, run simulations, or
 keep track of business accounts. The question is whether this really means
 you are a programmer. 

@big-block[600 @t{Stop! Think! Don't turn the page yet.}]

@; -----------------------------------------------------------------------------
@isection[#:tag "sec:not"]{Not!}

When you look at the ``programming'' book shelves in any random book store
 of some unnamed book chain, not to speak of certain parts of college book
 stores, you will see loads of books that promise to turn lead into gold,
 that is, make you a programmer in 21 days or faster. There are also books
 by cautious authors who think you need to stretch the same or similar
 material over the entire course of a semester. If you have worked through
 the first six sections of this book, however, you know that neither of
 these approaches can create a solid understanding of programming.

Acquiring the mechanical skills of programming---learning how to write
 instructions or expressions that the computer understands, getting to know
 what functions are available in the libraries, and similar
 activities---aren't helping you much with @bold{real} programming. To make
 such claims is like saying that a 10-year old who knows how to dribble can
 play on a professional soccer (football) team. It is also like claiming
 that memorizing a thousand words from the dictionary and a few rules from a
 grammar book teaches you a foreign language.

Programming is far more than the mechanics of language acquisition. It is
 about reading problem statements, extracting the important concepts. It is
 about figuring out what is really wanted. It is about exploring examples
 to strengthen your intuitive understanding of the problem. It is about
 organizing knowledge and it is about knowing what you don't know yet. It
 is about filling those last few gaps. It is about making sure that you
 know how and why your code works, and that you and your readers will do so in
 the future. In short, it is really about solving problems systematically.

The rest of this book is all about these things; very little of the book's
 content is about the mechanics of @bsl[] or other HtDP languages. The book
 shows you how good computer programmers think about problems,
 and---promise!---you will even learn to see that these ideas of problem
 solving apply to other situations in life, e.g., the work of doctors and
 journalists, lawyers and engineers, or car mechanics and photographers.

Oh, and by the way, the rest of the book uses a tone that is appropriate
 for a serious text. 

@bold{What the book is @italic{not} about} Many early books on programming
 and even some of today's books teach you a lot about the authors' favorite
 application discipline for programming: mathematics, physics, music,
 accounting, and so on. To some extent that is natural, because programming
 is useful in those areas. Then again, it forces you to know a lot (or at
 least something) about those disciplines. This book really focuses on
 programming and problem solving and what computer science can teach you in
 this regard. We have made every attempt to minimize the use of knowledge
 from other areas; for those few occasions when we went too far, we
 apologize.
