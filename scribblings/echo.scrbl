#lang scribble/manual
@(require scribble/eval
          scriblib/autobib
          (for-label racket/base)
          )

@title[#:tag "echo"]{The Program: Echo}
First we make an untyped program and add types to it. The program is intentionally small
and over-modularized for demonstration purposes. The program is a simple echo server with a
client to send it some data.

In the @tt{benchmark/} directory, create a @tt{echo/} directory with @tt{untyped/} and @tt{typed/}
subdirectories.

@include-section{echo-untyped.scrbl}
@include-section{echo-typed.scrbl}
