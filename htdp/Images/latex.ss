#lang at-exp racket

(require "../latex.rkt")

;; Examples 
;; @latex{\sum_{i=0}^{\infty}\lambda_i} 

;; Prologue:
@(latex "y = x \\cdot x")
(@latex
#<<eos
\mbox{\it sign}(x) = 
  \left\{
  \begin{array}{r@{\mbox{ if }}l}
  +1 & x > 0 \\
   0 & x = 0 \\
  -1 & x < 0 
  \end{array}\right.
eos
)

(@latex 
 #<<eos
d = v \cdot t - {1\over2} \cdot a \cdot t^2
eos
 )

(@latex 
 #<<eos
d(t) = v \cdot t - {1\over2} \cdot a \cdot t^2  
eos
 )

(@latex 
 #<<eos
 200! = 200 \cdot 199 \cdot \ldots \cdot 2 \cdot 1 \cdot 1
eos
 )

(@latex 
 #<<eos
c = \frac59 \cdot (f - 32)
eos
 )

(@latex 
#<<eos
\sqrt{x^2 + y^2}
eos
)

(@latex 
#<<eos
\underbrace{x + \ldots + y}_n
eos
)

(@latex 
#<<eos
\frac{-b+\sqrt{b^2 - 4 \cdot a \cdot c}}{2 \cdot a}
eos
)

(@latex #:file "quadratic"
#<<eos
\frac{-b}{2 \cdot a}
eos
)

(@latex 
#<<eos
\frac{-b-\sqrt{b^2 - 4 \cdot a \cdot c}}{2 \cdot a}
eos
)

(@latex 
#<<eos
\sqrt{(x0 - x1)^2 + (y0 - y1)^2}
eos
)

(@latex #:file "ex-poly1"
#<<eos
5 \cdot x
eos
)

(@latex #:file "ex-poly2"
#<<eos
5 \cdot x + 17 \cdot y
eos
)

(@latex #:file "ex-poly3"
#<<eos
5 \cdot x + 17 \cdot y + 3 \cdot z
eos
)

(@latex #:file "ex-integrate"
#<<eos
(\mbox{\it right} - \mbox{\it left}) 
\cdot 
\frac{f (\mbox{\it right}) + f (\mbox{\it left})}
     {2}
eos
)

(@latex #:file "ex-integrate2"
#<<eos
\mbox{\it TOLERANCE\/} \cdot (\mbox{\it right\/} - \mbox{\it left\/})
eos
)

;;          f(x) - f(r1)
;; slope = --------------
;;             x - r1
;; 
;; f(x) - f(r1) = slope * (x - r1) 
;; f(x) = slope * (x - r1) + f(r1) 
;;    0 = slope * (x_0 - r1) + f(r1) 
;;              f(r1)
;;  x_0 = r1 - -------
;;              slope 


(@latex #:file "newton-tangent-line"
  #<<eos
\mbox{\it tangent\/}(x) = \mbox{{\it slope\/}} \cdot x + f(r1)
eos
)

(@latex #:file "newton-tangent-root0"
  #<<eos
0 = \mbox{{\it slope\/}} \cdot \mbox{\it root-of-tangent\/} + f(r1)
eos
)


(@latex #:file "newton-tangent-root"
  #<<eos
\mbox{\it root-of-tangent\/} = r1 - \frac{f(r1)}{\mbox{{\it slope\/}}}
eos
)

(@latex #:file "newton0"
  #<<eos
\mbox{\it slope\/}
= \frac{f(r1+\epsilon) - f(r1-\epsilon)}{(r1+\epsilon) - (r1-\epsilon)}
= \frac{f(r1+\epsilon) - f(r1-\epsilon)}{2 \cdot \epsilon}
eos
)

(@latex #:file "newton1"
  #<<eos
\begin{array}{lcl}
t(x) & = & f'(r0) * (x - r0) + f(r0)\\
     & = & f'(r0) * x + [f(r0) - f'(r0) \cdot r0] \\
\end{array}
eos
)
  
(@latex #:file "newton2"
  #<<eos
y(x) = a \cdot x + b
eos
)
  
(@latex #:file "newton3"
  #<<eos
 r - \frac{f(r0)}{f'(r0)} \ . 
eos
)

(@latex #:file "eq"
  #<<eos
a_1 \cdot x_1 + ... + a_n \cdot x_n = b
eos
)

(@latex #:file "gauss1"
  #<<eos
\begin{array}{lclcrcrr}
2 \cdot x & + & 2 \cdot y & + &  3 \cdot z & = & 10 \\
2 \cdot x & + & 5 \cdot y & + & 12 \cdot z & = & 31 & \qquad (\dagger)\\
4 \cdot x & + & 1 \cdot y & - &  2 \cdot z & = &  1
\end{array}
eos
)

(@latex #:file "gauss2"
  #<<eos
\begin{array}{lclcrcrr}
2 \cdot 1 & + & 2 \cdot 1 & + &  3 \cdot 2 & = & 10 \\
2 \cdot 1 & + & 5 \cdot 1 & + & 12 \cdot 2 & = & 31 & \\ %  \qquad (\dagger)\\
4 \cdot 1 & + & 1 \cdot 1 & - &  2 \cdot 2 & = &  1
\end{array}
eos
)

(@latex #:file "gauss5"
  #<<eos
\begin{array}{lclcrcrr}
2 \cdot x & + & 2 \cdot y & + &  3 \cdot z & = &  10 \\
          &   & 3 \cdot y & + &  9 \cdot z & = &  21 & \qquad (\ddagger)\\
          & - & 3 \cdot y & - &  8 \cdot z & = & -19
\end{array}
eos
)

(@latex #:file "gaussA"
  #<<eos
\begin{array}{lclcrcrr}
2 \cdot x & + & 2 \cdot y & + &  3 \cdot z & = &  10 \\
          &   & 3 \cdot y & + &  9 \cdot z & = &  21 & \qquad (*)\\
          &   &           &   &  1 \cdot z & = &   2
\end{array}
eos
)

(@latex #:file "gaussG"
  #<<eos
\begin{array}{lclcrcrr}
2 \cdot x & + & 2 \cdot y & + &  2 \cdot z & = & 6 \\
2 \cdot x & + & 2 \cdot y & + &  4 \cdot z & = & 8 \\
2 \cdot x & + & 2 \cdot y & + &  1 \cdot z & = & 2
\end{array}
eos
)

(@latex #:file "gaussGT"
  #<<eos
\begin{array}{lclcrcrr}
2 \cdot x & + & 2 \cdot y & + &  2 \cdot z & = & 6 \\
0 \cdot x & + & 0 \cdot y & + &  2 \cdot z & = & 2 \\
0 \cdot x & + & 0 \cdot y & - &  1 \cdot z & = & -4
\end{array}
eos
)

(@latex #:file "gaussH"
  #<<eos
 3 \cdot y  +   9 \cdot 2  =   21 \ . 
eos
)

(@latex #:file "gaussHY"
  #<<eos
y = \frac{21 - 9 \cdot 3}{3}
eos
)

(@latex #:file "gaussHX"
  #<<eos
2 \cdot x + 2 \cdot 1 + 3 \cdot 2 = 10
eos
)

(@latex #:file "gaussHXS"
  #<<eos
x = \frac{10 - (2 \cdot 1 + 3 \cdot 2)}{2}
eos
)

(@latex #:file "gaussI"
  #<<eos
\begin{array}{llrrrrr}
({\sf list} & ({\sf list} & a_{11} & \ldots &  \ldots &  b_1) \\
            & ({\sf list} &        & a_{21} &  \ldots &  b_2)  \\
	    & ({\sf list} &        & \vdots &  \vdots &  \vdots)  \\
            & ({\sf list} &        &        &  a_{nn} &  b_n) & ) \\
\end{array}
eos
)

(@latex #:file "gaussJ"
  #<<eos
\frac{b_n}{a_{nn}} \ . 
eos
)

(@latex #:file "gaussK"
  #<<eos
 3 \cdot x + 9 \cdot y = 21 
eos
)

(@latex #:file "gaussL"
  #<<eos
 y = 2 \ ,
eos
)

(@latex #:file "gaussM"
  #<<eos
 3 \cdot x + 9 \cdot 2 = 21 \ .
eos
)

(@latex #:file "gaussN"
  #<<eos
9 \cdot 2 - 21
eos
)

(@latex #:file "dagger" "\\dagger")
