#lang scribble/manual

@(require "../base/Shared/shared.rkt")
@(require racket/date (for-label racket))

@(define (one s) (string-append s ", "))

@(define copyright (with-output-to-string (lambda () (display #\u00A9))))
@(require (only-in racket with-output-to-string))

@margin-note{@copyright 1 August 2014 @link["http://mitpress.mit.edu"]{MIT Press}

This material is copyrighted and provided under the Creative Commons
@link["http://creativecommons.org/licenses/by-nc-nd/2.0/legalcode"]{CC
BY-NC-ND} license [@link["http://creativecommons.org/licenses/by-nc-nd/2.0/"]{interpretation}].}

@title[#:tag "htdp2e" #:style book-style ]{How to Design Programs, Second Edition}

@big-block[150]{
 @centerline{
   Matthias Felleisen,
   Robert Bruce Findler,
   Matthew Flatt,
   Shriram Krishnamurthi}}

@margin-note{Do you notice the italics? Italicized words refer to technical
 terms. Here they refer to books on programming currently in bookstores.} 
Bad programming is easy. Even @italic{Dummies} can learn it in @italic{21 days}.

Good programming requires thought, but @bold{everyone} can do it and
 @bold{everyone} can experience the extreme satisfaction that comes with
 it.  The price is worth paying for the sheer joy of the discovery process,
 the elegance of the result, and the commercial benefits of a systematic
 program design process.

The goal of our book is to introduce readers of all ages and backgrounds to
 the craft of designing programs systematically. We assume few
 prerequisites: arithmetic, a tiny bit of middle school algebra, and the
 willingness to think through issues. We promise that the travails will pay
 off not just for future programmers but for anyone who has to follow a
 process or create one for others.

@; -----------------------------------------------------------------------------

@(define draft? (or (getenv "DRAFT") (is-draft?)))

@(define draft 
@p[@list{This document is the @bold{draft release} of HtDP/2e. It
is updated on a frequent basis.  The @link["../index.html"]{@bold{stable
version}} is released in conjunction with the PLT software and is thus more
suitable for teaching than this draft.}]
)

@(define release
@p[@list{This document is the @bold{current, stable release} of HtDP/2e.  It is
 updated in sync with semester breaks (summer, new years). It is thus
 well-suited for courses. In contrast, @link["./Draft/index.html"]{the
 @bold{current draft}} changes on a frequent basis; it should be consulted
 when people discover problems and/or errors in this document. If such
 flaws exist in both documents, please report them to the first author.}]
)

@(if draft? @bold{Draft Release} @bold{Stable Release})

@(if draft? draft release)

@centerline{Released on @(date->string (seconds->date (current-seconds)) draft)}

@;local-table-of-contents[]

@; -----------------------------------------------------------------------------

@;section[#:tag "difference" #:style 'unnumbered]
@bold{How the Second Edition Differs from the First} 

This second edition of ``How to Design Programs'' differs from the first
one in several aspects:

@itemlist[#:style 'ordered

@item{The second edition explicitly acknowledges the difference between
 designing a program and designing a bunch of functions. In particular,
 this edition focuses on two kinds of programs: interactive, reactive
 (graphical) programs and so-called batch programs.@margin-note*{Because
 graphical interactive programs are more interesting than batch programs,
 the book emphasizes the former over the latter.}}

@item{The design of a program proceeds in a top-down planning and a
 bottom-up construction fashion. We explicitly show how the interface to
 the libraries dictates the shape of certain program elements. In
 particular, the very first phase of a program design yields a wish list of
 functions. While the concept of a wish list existed in the first edition,
 the second edition treats it as an explicit design element.}

@item{The design of each wish on the wish list exploits the design recipe
 for functions. As in the first edition, the six parts focus on structural
 design, compositional design, generative recursion, and design of both
 structural and generative programs with accumulators.}

@item{A key element of structural design is the definition of functions
 that compose others. This design-by-composition@margin-note*{We thank
 Dr. Kathi Fisler for focusing our attention on this point.} is especially
 useful for the world of batch programs. Like generative recursion, it
 requires a eureka, specifically a recognition that the creation of
 intermediate data by one function and processing the intermediate data by
 a second function simplifies the overall design. Again, this kind of
 planning also creates a wish list, but formulating these wishes calls for
 an insightful development of an intermediate data definition. This edition
 of the book weaves in a number of explicit exercises on
 design-by-composition.}

@item{While testing has always been a part of the ``How to Design
 Programs'' philosophy, the teaching languages and @dr[] started supporting
 it properly only in 2002, just after we had released the first
 edition. This new edition heavily relies on this testing support now.}

@item{This edition of the book drops the design of imperative programs. The
 old chapters remain available on-line. The material will flow into the
 second volume of this series, ``How to Design Components.''}

@item{The book's examples and exercises employ new teachpacks. The
 preferred style is to link in these libraries via so-called
 @racket[require] specifications, but it is still possible to add
 teachpacks via a menu in @dr{}.}

@item{Finally, we decided to use a slightly different terminology: 
@v-table[
@(map p 
@list{
 HtDP/1e
 contract
 union
 })

@(map p 
@list{
 HtDP/2e
 signature 
 itemization
})]
}
]

@; -----------------------------------------------------------------------------

@bold{Acknowledgments} 

@; Special thanks
@; colleagues 
@; Kathi, Gregor, Norman, 
@; teachers 
@; Dan (geb), Jack, Kyle, Nadeem

We thank 
 @one{Ennas Abdussalam}
 @one{Saad Bashir}
 Steven Belknap,
 Stephen Bloch,
 @one{Elijah Botkin}
 @one{Anthony Carrico}
 @one{Rodolfo Carvalho} 
 Estevo Castro,
 Stephen Chang,
 @one{Nelson Chiu}
 Jack Clay,
 @one{Richard Cleis}
 John Clements,
 @one{Mark Engelberg}
 Christopher Felleisen, 
 Sebastian Felleisen,
 Adrian German,
 @one{Jack Gitelson}
 @one{Kyle Gillette}
 @one{Scott Greene} 
 Ryan Golbeck,
 Josh Grams,
 Nadeem Abdul Hamid,
 @one{Jeremy Hanlon}
 @one{Craig Holbrook}
 @one{Wayne Iba}
 Jordan Johnson, 
 @one{Blake Johnson}
 @one{Erwin Junge}
 Gregor Kiczales,
 Eugene Kohlbecker,
 Jackson Lawler,
 Jay McCarthy,
 @one{Mike McHugh}
 @one{Wade McReynolds}
 @one{Elena Machkasova}
 @one{David Moses}
 Ann E. Moskol,
 @one{Scott Newson}
 Paul Ojanen,
 @one{Prof. Robert Ordóñez}
 @one{Laurent Orseau} 
 Klaus Ostermann,
 Sinan Pehlivanoglu,
 @one{Eric Parker}
 @one{Nick Pleatsikas}
 Norman Ramsey,
 @one{Krishnan Ravikumar}
 @one{Jacob Rubin}
 Luis Sanjuán,
 @one{Ryan ``Havvy'' Scheel}
 @one{Lisa Scheuing}
 @one{Willi Schiegel}
 @one{Vinit Shah}
 @one{Nick Shelley}
 @one{Matthew Singer}
 Stephen Siegel,
 @one{Joe Snikeris}
 Marc Smith,
 Dave Smylie,
 Vincent St-Amour,
 @one{Reed Stevens}
 @one{Kevin Sullivan}
 Éric Tanter,
 Sam Tobin-Hochstadt, 
 @one{Thanos Tsouanas}
 @one{Yuwang Yin}
 David Van Horn,
 @one{Jan Vitek}
 Mitchell Wand, 
 @one{Michael Wijaya}
 @one{G. Clifford Williams}
 @one{Ewan Whittaker-Walker}
 @one{Julia Wlochowski}
 Roelof Wobben
and 
 @one{Mardin Yadegar}
for comments on previous drafts of this second edition. 

The HTML layout is due to Matthew Butternick who created these styles for
 the Racket documentation. 

We are grateful to Ada Brunstein and Marie Lufkin Lee, our editors at MIT
 Press, who gave us permission to develop this second edition of "How to
 Design Programs" on-line.

@; -----------------------------------------------------------------------------

@table-of-contents[]

@; -----------------------------------------------------------------------------
@include-section["0prologue.scrbl"]
