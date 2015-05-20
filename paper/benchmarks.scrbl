#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:bm"]{The Benchmark Programs}

the benchmarks: describe purpose, structure, SE attributes (keep short)

@exact{
\begin{figure*}
\begin{verbatim}
| Project name          | Responsible | Ported? | Benchmarked? | # Modules | Module structure |
| --------------------- | ----------- | ------- | ------------ | --------- | ---------------- |
| gregor                | Ben         | Yes     | No           | 13        | pyramidic        |
| HTDP script           | Ben         | Yes     | No           | 4         | triangle         |
| echo                  | Ben         | Yes     | No           | 4         | directed diamond |
| funkytown             | Ben         | Yes     | No           | 9         | vine-like        |
| kcfa                  | Ben         | Yes     | No           | 7         | line, or braid   |
| morse-code            | Ben         | Yes     | No           | 4         | vee              |
| sieve                 | Ben         | Yes     | No           | 2         | one chain        |
| suffixtree            | Ben         | Yes     | No           | 5         | line             |
| zo traversal          | Ben         | Yes     | No           | 5         | almost diamond   |
| mbta                  | Matthias    | Yes     | No           | 4         | one chain        |
| tetris                | Max         | Yes     | No           | 9         | diamond          |
| snake                 | Max         | Yes     | No           | 12        | diamond          |
| protbuf (?)           |             | No      | No           |           |                  |
| [simulation][1] (?)   |             | No      | No           |           |                  |
| [ragg][2] (?)         |             | No      | No           |           |                  |
| [parser-tools][3] (?) |             | No      | No           |           |                  |
| [rmacs][4] (?)        |             | No      | No           |           |                  |
\end{verbatim}
\caption{The software characteristics of the benchmarks} \label{fig:bm}
\end{figure*}
}
