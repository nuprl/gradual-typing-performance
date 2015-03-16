#lang scribble/manual
@(require planet/scribble
          racket/sandbox
          scribble/eval
          (for-label (this-package-in main)
                     racket/base))


@(define my-evaluator
   (call-with-trusted-sandbox-configuration 
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string])
        (make-evaluator 'racket
                        #:requires
                        (list))))))

@title{Suffix trees with Ukkonen's algorithm}

@author+email["Danny Yoo" "dyoo@hashcollision.org"]

This is an implementation of suffix trees and their linear-time
construction with the Ukkonen algorithm.  This implementation is based
on notes from Gusfield, "Algorithms on Strings, Trees, and Sequences".


@section{Example}

Let's rush into a minimal example:

@interaction[#:eval my-evaluator 
(require (planet dyoo/suffixtree))
(define tree (make-tree))
(tree-add! tree (string->label "00010010$"))
(define root (tree-root tree))
(node-children root)
(label->string (node-up-label (car (node-children root))))
]


@section{Introduction}

Suffix trees encode all the nonempty suffixes of a string.  For
example, the string "0100101$" corresponds to the following suffix
tree.

@verbatim|{
   root
    |
    V
    +--- $
    |
    +--- 1 --- $
    |    |
    |    +---- 0 --- 0101$
    |          |
    |          +---- 1$
    |
    +--- 0 --- 0101$
         |
         +---- 1 ---- $
               |
               +----- 0 --- 1$
                      |
                      +---- 0101$
}|

Every path from the root to any leaf spells out a suffix of the string
@racket["0100101$"], and every suffix is accounted for.  This in
itself might not sound too sexy, but by preprocessing a string as a
suffix tree, we can then do some amazing things.

For example, we can see if a substring is present in a suffix tree in
time bounded by the length of the substring by following characters
starting from the root.  Suffix trees also allow us to find the
longest common substring between strings in linear time.  Dan
Gusfield's book "Algorithms on Strings, Trees, and Sequences" sings
praises about suffix trees, and deservedly so.

Constructing a suffix tree can be done in linear-time; the algorithm
used here is Ukkonen's algorithm, since it's one of the simplest to
code.  That being said, the algorithm is not quite simple; for more
information on the construction algorithm, see the References section
below.



@section{API}
@defmodule/this-package[main]

The API consists of the main suffix tree algorithm, and auxillary
utilities and applications.

The main structures are trees, nodes, and labels.

@subsection{Trees}
@declare-exporting/this-package[main]

A suffix tree consists of a root.  This implementation allows multiple
labels to be added to the tree.

@defproc[(make-tree) tree]{
Constructs an empty suffix tree with a single root node.  

For example:
@interaction[#:eval my-evaluator
(define a-tree (make-tree))
a-tree]
}


@defproc[(tree? [datum any]) boolean]{
Returns #t if datum is a suffix tree.

For example:
@interaction[#:eval my-evaluator
(define a-tree (make-tree))
(tree? a-tree)]
}

@defproc[(tree-root [a-tree tree]) node]{
    Selects the root node from a tree.

For example:
@interaction[#:eval my-evaluator
(define a-tree (make-tree))
(tree-root a-tree)]
}


@defproc[(tree-add! [a-tree tree] [a-label label]) void]{
    Adds a label and all of its nonempty suffixes to the tree.


For example:
@interaction[#:eval my-evaluator
(define a-tree (make-tree))
(tree-add! a-tree (string->label "supercalifragilisticexpialidocious"))]
}


@defproc[(tree-walk [a-tree tree] [a-label label] [succeed-f (node number -> A)] [fail-f (node number number -> B)]) (or/c A B)]{
    Starting from the tree-root, walks along a path whose path
    label exactly matches the input label.

    If the label matched completely, calls @racket[succeed-f] with the
    position where the matching had succeeded.


    If the label mismatched, calls @racket[fail-f] with the tree position where
    the matching had failed.

    The return value from tree-walk will either be @racket[A] or @racket[B].

For example:
@interaction[#:eval my-evaluator
(define a-tree (make-tree))
(tree-add! a-tree (string->label "banana"))
(define (success-f node up-label-offset)
  (list node up-label-offset))
(define (fail-f node up-label-offset input-label-offset)
  (list node up-label-offset input-label-offset))
(tree-walk a-tree (string->label "banana") success-f fail-f)
(tree-walk a-tree (string->label "ban") success-f fail-f)
(tree-walk a-tree (string->label "ana") success-f fail-f)
(tree-walk a-tree (string->label "apple") success-f fail-f)
]
}

@defproc[(tree-contains? [a-tree tree] [a-label label]) boolean]{

    Returns @racket[#t] if a path exists starting from the tree-root of the
    tree whose path-label exactly matches @racket[a-label].

For example:
@interaction[#:eval my-evaluator
(define a-tree (make-tree))
(tree-add! a-tree (string->label "0100101$"))
(tree-contains? a-tree (string->label "01"))
(tree-contains? a-tree (string->label "001"))
(tree-contains? a-tree (string->label "011"))
(tree-contains? a-tree (string->label "0101"))]





    @racket[tree-contains?] is an application of @racket[tree-walk]:

@racketblock[
        (define (tree-contains? tree label)
          (tree-walk tree label 
                     (lambda (node up-label-offset) #t)
                     (lambda (node up-label-offset input-label-offset) #f)))
]
}


@subsection{Nodes}

Nodes form the structure of the suffix tree, and link up children
nodes as well.  Every internal node I of a suffix tree will also have
a suffix-node whose path-label is the immediate suffix of node I.

For example, we can inspect the @racket[node-up-label] of each child of the root:
@interaction[#:eval my-evaluator
(define a-tree (make-tree))
(tree-add! a-tree 
          (string->label "peter piper"))
(for ([i (in-naturals)]
      [c (node-children (tree-root a-tree))])
  (printf "~a: ~a\n" i (label->string (node-up-label c))))

(define p-node (node-find-child (tree-root a-tree) #\p))
(for ([i (in-naturals)]
      [c (node-children p-node)])
  (printf "~a: ~a\n" i (label->string (node-up-label c))))
]

@defproc[(node-up-label [a-node node]) label]{

    Selects the label of the edge that connects this node to its
    parent.  The up-label of the root node is empty.

}


@defproc[(node-parent [a-node node]) (or/c node #f)]{
    Selects the parent of this node.  The root of a suffix tree has no
    parent, so @racket[(node-parent (tree-root tree))] returns @racket[#f].
}

@defproc[(node-suffix-link [a-node node]) node]{
    Selects the suffix node of this node.  If the suffix-link is not
    set, returns #f.
}

@defproc[(node-find-child [a-node node] [a-label-element label-element]) (or/c node #f)]{
    Selects the child whose up-label starts with the label-element.  If no
    such child can be found, returns @racket[#f].
}

@defproc[(node-children [a-node node]) (listof node)]{
    Selects the list of children nodes to this node.  If the node is a
    leaf, returns @racket['()].
}


@subsection{Labels}

Labels represent an immutable sequence of label-elements.
Label-elements can be anything that compare with equal?, but the most
common label-elements will be characters.  Labels can be sublabeled
with efficiency.


@defproc[(string->label [a-string string]) label]{
    Constructs a label from a string.  Each of the label-elements of
    this label will be a character.
}

@defproc[(string->label/with-sentinel [a-str string]) label]{

    Constructs a label from a string with a trailing sentinel
    character to guarantee that all suffixes can be explicitely
    represented in a suffix tree.  (See the Caveats section below for
    details.)

    Note that @racket[label->string] can't be directly used on a label with a
    sentinel.
}

@defproc[(label->string [a-label label]) string]{
    Constructs a string from a label, assuming that all label-elements
    of the string are characters.
}


@defproc[(vector->label [a-vec vector]) label]{
    Constructs a label from a vector.
}

@defproc[(vector->label/with-sentinel [a-vec vector]) label]{

    Constructs a label from a vector with a trailing sentinel character.
}

@defproc[(label->vector [a-label label]) vector]{
    Selects a vector of the label-elements that represent the label.
    This vector is immutable.
}

@defproc[(sublabel [a-label label] [left-offset number] [right-offset number (label-length label)]) label]{

    Derives a new sliced label from the parent label, along the
    half-open interval [@racket[left-offset], @racket[right-offset]).

    If right-offset is omitted, it defaults to @racket[(label-length label)].

    @racket[(<= left-offset right-offset)] should be @racket[#t].
}

@defproc[(label-ref [a-label label] [a-offset number]) label-element]{
    Returns the label's label-element at that offset.
}

@defproc[(label-length [a-label label]) number]{
    Returns the length of a label.
}

@defproc[(label-equal? [label-1 label] [label-2 label]) boolean]{
    Produce true if the two labels have equal content.

    Warning: two labels may have equal content, but come from
    different sources.
}

@defproc[(label-source-eq? [label-1 label] [label-2 label]) boolean]{

    Returns @racket[#t] if both labels share a common derivation from
    sublabeling.
}

@defproc[(label-source-id [a-label label]) number]{

    Returns an numeric identifier for this label.

    @racket[(label-source-eq? label-1 label-2)]

         logically implies:

    @racket[(= (label-source-id label-1) (label-source-id label-2))]
}


@subsection{Other utilities}

This module provides some example applications of suffix trees.

@defproc[(longest-common-substring [string-1 string] [string-2 string]) string]{
    Returns the longest common substring between the two strings.

For example:
@interaction[#:eval my-evaluator
(longest-common-substring 
 "Lambda: the Ultimate Imperative"
 "Procedure Call Implementations Considered Harmful, or, Lambda: the Ultimate GOTO")]
}

@defproc[(longest-common-sublabel [label-1 label] [label-2 label]) label]{
    Returns the longest sublabel that's shared between label-1 and label-2.
}

@defproc[(path-label [a-node node]) label]{
    Returns a new label that represents the path from the root to this
    node.
}





@section{Extended example: keyword search}

@codeblock|{
#lang racket/base

(require (planet dyoo/suffixtree))

;; String reference
(struct sref (str))

;; string->label*: string -> label
;; Creates a label out of a string, but with a reference
;; to the string at the end.
(define (string->label* s)
  (vector->label 
   (list->vector (append (string->list s) (list (sref s))))))


;; leaves: node -> (listof node)
;; Get the leaves of the tree rooted at node.
(define (leaves a-node)
  (let loop ([a-node a-node]
             [acc '()])
    (define children (node-children a-node))
    (cond [(eq? children '())
           (cons a-node acc)]
          [else
           (foldl loop acc children)])))


;; leaf-str: node -> string
;; Given a leaf node, get back the string stored in the
;; terminating sref element.
(define (leaf-str a-leaf)
  (define a-label (node-up-label a-leaf))
  (sref-str (label-ref a-label (sub1 (label-length a-label)))))


;; find-matches: tree string -> (listof string)
(define (find-matches a-tree str)
  (define (on-success node up-label-offset)
    (map leaf-str (leaves node)))

  (define (on-fail node up-label-offset input-label-offset)
    '())
  
  (tree-walk a-tree
             (string->label str)
             on-success
             on-fail))



(define a-tree (make-tree))
(tree-add! a-tree (string->label* "peter"))
(tree-add! a-tree (string->label* "piper"))
(tree-add! a-tree (string->label* "pepper"))

(find-matches a-tree "per")
(find-matches a-tree "ter")
}|



@section{Caveats}

The code in tree-add! assumes that the construction of the full
suffix tree on its input string is possible.  Certain strings don't
have an full explicit suffix tree, such as "foo".

@verbatim|{
    +-- "foo"
    |
    +-- "oo"
}|

In this case, when we try to construct a suffix tree out of "foo", we
have an implicit suffix tree, where not every leaf corresponds to a
suffix of the input string.  The suffix "o" is implicit in this tree.


In order to guarantee that all suffixes will have a place in the
suffix tree, we'll often add a sentinel character at the end the
string to make sure all suffixes have a unique path in the suffix
tree.  For example, assuming that we use "$" as our sentinel:

@verbatim|{
    +-- "foo$"
    |
    +-- "o" -- "o$"
    |    |
    |    +---- "$"
    |
    +-- "$"
}|   

The API has the function string->label/with-sentinel to automatically add
a unique sentinel character at the end of a string.

@racketblock[
(let ([tree (make-tree)]
      [label (string->label/with-sentinel "foo")])
  (tree-add! label))
]

so be sure to use this if you need to ensure the representation of all
suffixes in the suffix tree.



@section{References}

Dan Gusfield.  Algorithms on Strings, Trees, and Sequences: Computer
Science and Computational Biology.  Cambridge University Press, New
York, NY, 1997.

Lloyd Allison.  Suffix Trees.
@url{http://www.allisons.org/ll/AlgDS/Tree/Suffix/}

Mark Nelson.  Fast String Searching With Suffix Trees.  Dr. Dobb's
Journal, August, 1996.
@url{http://www.dogma.net/markn/articles/suffixt/suffixt.htm}

Mummer: Ultra-fast alignment of large-scale DNA and protein sequences.
@url{http://mummer.sourceforge.net/}
