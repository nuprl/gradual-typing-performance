"""
    Simple wrappers for outputting LaTeX
"""

import util

# TeX preamble / header
PREAMBLE = "\n".join(["\\documentclass{article}"
                     ,"\\usepackage{graphicx,enumitem}"
                     ,"\\newcommand{\\mono}[1]{\\texttt{#1}}"
                     ,"\\begin{document}"
                     ,"\\setlist[enumerate,1]{start=0}"
                     ])

def difference(n1, n2):
    """ (-> Nat Nat (List Nat String))
       Rounded quotient (n1 / n2).
       Shows how many times better or worse `n1` is compared to the
       expected `n2`.
       Additionally, returns a descriptive string.
    """
    val = round(n1 / n2, 2)
    if val >= 1:
        descr = "slower"
    else:
        descr = "faster"
    return val, descr

def newpage():
    return "\n\\newpage\n"

def list(items, numbers=False):
    tag = "enumerate" if numbers else "itemize"
    return "\\begin{%s}\n\\item %s\\end{%s}" % (tag, "\n\item ".join(items), tag)

def end():
    return "\\end{document}"

def figure(fname, width_scale=1):
    """
        Splice the filename `fname` into an imported figure
        for the final .tex
    """
    return "\\includegraphics[width=%s\\textwidth]{%s}" % (width_scale, util.strip_directory(fname))

def section(title):
    return "\n\n\\section{%s}" % title

def subsection(title):
    return "\n\\subsection{%s}" % title

def subsubsection(title):
    return "\n\\subsubsection{%s}" % title

def table(title, rows):
    return "\n".join(["\n\\begin{center}\\begin{tabular}{|%s|}\\hline" % " ".join(("r" for _ in range(len(title))))
                     ," & ".join((str(x) for x in title)) + "\\\\\\hline"
                     ,"\\\\\n".join((" & ".join((str(x) for x in row)) for row in rows))
                     ,"\\\\\\hline\\end{tabular}\\end{center}"
                     ])
