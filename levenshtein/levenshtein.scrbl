#lang scribble/doc
@(require scribble/manual)
@;@(require (for-label (planet neil/levenshtein:1:3)))
@title[#:version "0.6"]{@bold{levenshtein}: Levenshtein Distance Metric in Scheme}
@author{Neil Van Dyke}


License: @seclink["Legal" #:underline? #f]{LGPL 3} @(hspace 1) Web: @link["http://www.neilvandyke.org/levenshtein-scheme/" #:underline? #f]{http://www.neilvandyke.org/levenshtein-scheme/}

@defmodule[levenshtein]

      

@section{Introduction}


      

This is a Scheme implementation of the @italic{Levenshtein Distance} algorithm, which is an @italic{edit distance} metric of string similarity, due to Vladimir Levenshtein.  The Levenshtein Distance is a function of two strings that represents a count of single-character insertions, deletions, and substitions that will change the first string to the second.  More information is available in @link["http://www.nist.gov/dads/HTML/Levenshtein.html"]{NIST DADS} and the Michael Gilleland article, ``@link["http://www.merriampark.com/ld.htm"]{Levenshtein Distance in Three Flavors}.''


      

This implementation is modeled after a @link["http://www.mgilleland.com/ld/ldperl2.htm"]{space-efficient Perl implementation} by Jorge Mas Trullenque.  It has been written in R5RS Scheme, and extended to support heterogeneous combinations of Scheme types (strings, lists, vectors), user-supplied predicate functions, and optionally reusable scratch vectors.


    
      

@section{Basic Comparisons}


      

In the current implementation, all comparisons are done internally via vectors.


      

@defproc[(vector-levenshtein/predicate/get-scratch (a any/c) (b any/c) (pred any/c) (get-scratch any/c)) any/c]{
          

Few, if any, programs will use this procedure directly.  This is like @tt{vector-levenshtein/predicate}, but allows @schemevarfont{get-scratch} to be specified.  @schemevarfont{get-scratch} is a procedure of one term, @emph{n}, that yields a vector of length @emph{n} or greater, which is used for record-keeping during execution of the Levenshtein algorithm. @tt{make-vector} can be used for @schemevarfont{get-scratch}, although some programs comparing a large size or quantity of vectors may wish to reuse a record-keeping vector, rather than each time allocating a new one that will need to be garbage-collected.


        }

      

@defproc[(vector-levenshtein/predicate (a any/c) (b any/c) (pred any/c)) any/c]{}


@defproc[(vector-levenshtein/eq (a any/c) (b any/c)) any/c]{}


@defproc[(vector-levenshtein/eqv (a any/c) (b any/c)) any/c]{}


@defproc[(vector-levenshtein/equal (a any/c) (b any/c)) any/c]{}


@defproc[(vector-levenshtein (a any/c) (b any/c)) any/c]{
          

Calculate the Levenshtein Distance of vectors @schemevarfont{a} and @schemevarfont{b}. @schemevarfont{pred} is the predicate procedure for determining if two elements are equal.  The @tt{/eq}, @tt{/eqv}, and @tt{/equal} variants correspond to the standard equivalence predicates, @tt{eq?}, @tt{eqv?}, and @tt{equal?}.  @tt{vector-levenshtein} is an alias for @tt{vector-levenshtein/equal}.


          

@SCHEMEBLOCK[
(vector-levenshtein '#(6 6 6) '#(6 35 6 24 6 32)) ==> 3
]


        }

      

@defproc[(list-levenshtein/predicate (a any/c) (b any/c) (pred any/c)) any/c]{}


@defproc[(list-levenshtein/eq (a any/c) (b any/c)) any/c]{}


@defproc[(list-levenshtein/eqv (a any/c) (b any/c)) any/c]{}


@defproc[(list-levenshtein/equal (a any/c) (b any/c)) any/c]{}


@defproc[(list-levenshtein (a any/c) (b any/c)) any/c]{
          

Calculate the Levenshtein Distance of lists @schemevarfont{a} and @schemevarfont{b}. @schemevarfont{pred} is the predicate procedure for determining if two elements are equal.  The @tt{/eq}, @tt{/eqv}, and @tt{/equal} variants correspond to the standard equivalence predicates, @tt{eq?}, @tt{eqv?}, and @tt{equal?}.  @tt{list-levenshtein} is an alias for @tt{list-levenshtein/equal}.  Note that comparison of lists is less efficient than comparison of vectors.


          

@SCHEMEBLOCK[
(list-levenshtein/eq '(b c e x f y) '(a b c d e f)) ==> 4
]


        }

      

@defproc[(string-levenshtein (a any/c) (b any/c)) any/c]{
          

Calculate the Levenshtein Distance of strings @schemevarfont{a} and @schemevarfont{b}.


          

@SCHEMEBLOCK[
(string-levenshtein "adresse" "address") ==> 2
]


        }

    
      

@section{Type-Coercing Comparisons}


      

Procedures @tt{levenshtein} and @tt{levenshtein/predicate} provide a convenient interface for comparing a combination of vectors, lists, and strings, the types of which might not be known until runtime.


      

@defproc[(levenshtein/predicate (a any/c) (b any/c) (pred any/c)) any/c]{
          

Calculates the Levenshtein Distance of two objects @schemevarfont{a} and @schemevarfont{b}, which are vectors, lists, or strings.  @schemevarfont{a} and @schemevarfont{b} need not be of the same type.  @schemevarfont{pred} is the element equivalence predicate used.


          

@SCHEMEBLOCK[
(levenshtein/predicate '#(#\A #\B #\C #\D)
                       "aBXcD"
                       char-ci=?)
==> 1
]


        }

      

@defproc[(levenshtein (a any/c) (b any/c)) any/c]{
          

Calculate the levenshtein distance of @schemevarfont{a} and @schemevarfont{b}, in a similar manner as using @tt{levenshtein/predicate} with @tt{equal?} as the predicate.


          

@SCHEMEBLOCK[
(define g '#(#\g #\u #\m #\b #\o))

(levenshtein g "gambol")  ==> 2
(levenshtein g "dumbo")   ==> 1
(levenshtein g "umbrage") ==> 5
]


        }

    
      

@section{History}


      

@itemize[

@item{Version 0.6 --- 2009-03-14 -- PLaneT @tt{(1 3)}
            

Documentation fixes.


          }


@item{Version 0.5 --- 2009-02-24 -- PLaneT @tt{(1 2)}
            

License is now LGPL 3.  Tests moved out of main file.  Converted to author's new Scheme administration system.


          }


@item{Version 0.4 --- 2005-07-10 -- PLaneT @tt{(1 1)}
            

Added Testeez tests.


          }


@item{Version 0.3 --- 2005-07-09 -- PLaneT @tt{(1 0)}
            

PLaneT release, and minor documentation changes.


          }


@item{Version 0.2 --- 2004-07-06
            

Documentation changes.


          }


@item{Version 0.1 --- 2004-05-13
            

First release.  Tested only lightly, and today @emph{is} the 13th, so @emph{caveat emptor}.


          }



]


    

@section[#:tag "Legal"]{Legal}



Copyright (c) 2004--2009 Neil Van Dyke.  This program is Free Software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 3 of the License (LGPL 3), or (at your option) any later version.  This program is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a particular purpose.  See http://www.gnu.org/licenses/ for details.  For other licenses and consulting, please contact the author.

