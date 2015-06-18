"""
    Simple wrappers for outputting LaTeX
"""

import constants
import util

# TeX preamble / header
PREAMBLE = "\n".join(["\\documentclass{article}"
                     ,"\\usepackage{graphicx,enumitem,subcaption}"
                     ,"\\usepackage[margin=1in]{geometry}"
                     ,"\\newcommand{\\imgwidth}{0.25\\textwidth}"
                     ,"\\newcommand{\\imgkern}{0.01\\textwidth}"
                     ,"\\newcommand{\\magicvoffset}{-4.1cm}" # TODO less magic, more relative positioning
                     ,"\\newcommand{\\gtoverhead}{{\\tt\\bf$\\times$}}"
                     ,"\\begin{document}"
                     ,"\\setlist[enumerate,1]{start=0}"
                     ])

# Magic header for graphs' L-values
FIGSKIP = "\\hspace{\\imgkern}"
def titlebox(message):
    # Hacking, to put titles over the $$$ graphs
    message = message or "~"
    return "\\parbox{\\imgwidth}{\\centering\\textbf{%s}}" % message
L_HEADER = "\n".join(["\\hbox{"
                      ,"\\hspace{\\imgwidth}"
                      , FIGSKIP
                      ,titlebox("L = 0")
                      , FIGSKIP
                      , FIGSKIP.join([titlebox(L) for L in range(1, constants.MAX_L)])
                      , FIGSKIP
                      ,titlebox("%s (steps)" % constants.MAX_L)
                      ,"}"
                      ,"\\vspace{0.5ex}"
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

def magicparbox(title, lines):
    """
        Arrange the lines of text into a parbox
        that magically aligns with our specific 3D plot figures
    """
    body = "\n".join([title,
                      "\\\\[1ex]",
                      "\\\\\n".join(lines)])
    return "\\parbox{\\imgwidth}{\\vspace{\\magicvoffset}%s}\n%s" % (body, FIGSKIP)

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
    return "\\includegraphics[width=\\imgwidth]{%s}" % util.strip_directory(fname)

def section(title):
    return "\n\n\\section{%s}" % title

def subsection(title):
    return "\n\\subsection{%s}" % title

def subsubsection(title):
    return "\n\\subsubsection{%s}" % title

def table(title, rows):
    return "\n".join(["\n\\begin{tabular}{|@{}l@{} %s|}\\hline" % " ".join(("r" for _ in range(1, len(title))))
                     ," & ".join(("%s" % x for x in title)) + "\\\\\\hline"
                     ,"\\\\\n".join((" & \\hfill{}".join((str(x) for x in row)) for row in rows))
                     ,"\\\\\\hline\\end{tabular}"
                     ])
