#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:bm"]{The Benchmark Programs}

the benchmarks: describe purpose, structure, SE attributes (keep short)

@exact|{
\begin{figure}
\newcommand{\yespycket}{$\CIRCLE$}
\newcommand{\maybepycket}{$\RIGHTcircle$}
\newcommand{\nopycket}{$\Circle$}
\begin{tabular}[t]{lrll}
\toprule
Project name          & \# Modules & Module structure & Pycket \\
\midrule
gregor                & 13         & pyramidic        & \nopycket \\
HTDP script           & 4          & triangle         & \nopycket \\
echo                  & 4          & directed diamond & \nopycket \\
funkytown             & 9          & vine-like        & \yespycket \\
kcfa                  & 7          & line, or braid   & \maybepycket \\
morse-code            & 4          & vee              & \nopycket \\
sieve                 & 2          & one chain        & \yespycket \\
suffixtree            & 5          & line             & \yespycket \\
zo traversal          & 5          & almost diamond   & \nopycket \\
mbta                  & 4          & one chain        & \nopycket \\
tetris                & 9          & diamond          & \nopycket \\
snake                 & 12         & diamond          & \yespycket \\
\bottomrule
\end{tabular}
\nocaptionrule \caption{The software characteristics of the benchmarks} \label{fig:bm}
\end{figure}
}|
