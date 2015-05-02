"""
Helpers for printing results to an output destination.

2015-04-20: only renders a .tex file.
"""

import plot
import statistics
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

def list(items, numbers=False):
    tag = "enumerate" if numbers else "itemize"
    return "\\begin{%s}\n\\item %s\\end{%s}" % (tag, "\n\item ".join(items), tag)

def end():
    return "\\end{document}"

def figure(fname):
    """
        Splice the filename `fname` into an imported figure
        for the final .tex
    """
    return "\\includegraphics[width=\\textwidth]{%s}" % util.strip_directory(fname)

def section(title):
    return "\n\n\\section{%s}" % title

def subsection(title):
    return "\n\\subsection{%s}" % title

def as_tex(results, outfile):
    """ (-> Result Path-String Void)
        Given an Result object, save results to a nicely-formatted .tex file.
    """
    def render(s):
        print(s, file=f)
    untyped = results["ugt"]["summary"]["untyped"]
    typed   = results["ugt"]["summary"]["typed"]
    gradual = results["ugt"]["summary"]["gradual"]
    with open(outfile, "w") as f:
        render(PREAMBLE)
        render("\n\n\\section{Results: %s}" % results["title"])
        render("\n\\subsection{Module Summary}")
        render("\\begin{enumerate}\n\\item \\mono{%s}\\end{enumerate}" % "}\n\item \mono{".join([k for (k,v) in sorted(results["graph"].items(), key=lambda item: item[1][0])]))
        render("Total of %s configurations" % (2 ** (len(results["graph"].keys()))))
        render("Ran each configuration %s times" % results["runs"])
        render("\n\\subsection{Overall Runtimes}")
        render("\\begin{itemize}")
        render("\\item Average \\emph{untyped} runtime: %s" % untyped["mean"])
        render("  \\begin{itemize}\n  \\item %s\\end{itemize}" \
          % "\n  \\item ".join(["Median: %s" % untyped["median"]
                               ,"Min: %s"    % untyped["min"]
                               ,"Max: %s"    % untyped["max"]
                               ,"95\\%% confidence: %s~\\textendash~%s" % (untyped["ci"][0], untyped["ci"][1])
                               ]))
        t_vs_u, t_vs_u_descr = difference(typed['mean'], untyped['mean'])
        render("\\item Average \\emph{typed} runtime: %s (%s times %s than untyped average)" % (typed["mean"], t_vs_u, t_vs_u_descr))
        render("  \\begin{itemize}\n  \\item %s\n\\end{itemize}" \
          % "\n  \\item ".join(["Median: %s" % typed["median"]
                               ,"Min: %s"    % typed["min"]
                               ,"Max: %s"    % typed["max"]
                               ,"95\\%% confidence: %s~\\textendash~%s" % (typed["ci"][0], typed["ci"][1])
                               ]))
        if not gradual:
          render("\\end{itemize}")
        else:
          g_vs_u = difference(gradual["mean"], untyped["mean"])
          render("\\item Average gradually-typed runtime is %s times %s than untyped average." % g_vs_u)
          render("\\item Best gradually-typed runtime is %s times %s than untyped average." % difference(results["best"][0]["time"], untyped["mean"]))
          render("\\begin{itemize}\\item Configuration: \\mono{%s}\\end{itemize}" % results["best"][0]["id"])
          render("\\item Worst gradually-typed runtime is %s times %s than untyped average." % difference(results["worst"][0]["time"], untyped["mean"]))
          render("\\begin{itemize}\\item Configuration: \\mono{%s}\\end{itemize}" % results["worst"][0]["id"])
          bavg_vs_u = difference(statistics.mean([x["time"] for x in results["best"]]), untyped["mean"])
          render("\\item Average of top %s gradually-typed configurations is %s times %s than untyped average" % (len(results["best"]), bavg_vs_u[0], bavg_vs_u[1]))
          wavg_vs_u = difference(statistics.mean([x["time"] for x in results["worst"]]), untyped["mean"])
          render("\\item Average of bottom %s gradually-typed configurations is %s times %s than untyped average" % (len(results["worst"]), wavg_vs_u[0], wavg_vs_u[1]))
          render("\\end{itemize}")
          render("\n\\subsection{Aggregate Figures}")
          bar = plot.bar(range(5)
                        ,[1, g_vs_u[0], bavg_vs_u[0], wavg_vs_u[0], difference(typed["mean"], untyped["mean"])[0]]
                        ,"%s-normalized-runtimes" % results["title"]
                        ,"Group"
                        ,"Runtime (Normalized to untyped)"
                        ,xlabels=["Untyped", "All\nGradually-Typed", "Top %s" % (len(results["best"])), "Bottom %s" % (len(results["worst"])), "Typed"])
          render(figure(bar))
          render(figure(results["ugt"]["img"]))
          render(figure(results["bucketed"]))
          render("\n\\subsection{Worst (gradual) Configurations}")
          render("The worst %s configurations and their boundaries are:" % len(results["worst"]))
          render("\\begin{itemize}")
          for bad_c in results["worst"]:
              vs_u, descr = difference(bad_c["time"], untyped["mean"])
              edges_str = "  \\begin{itemize}\n  \\item %s\n  \\end{itemize}" % "\n  \\item ".join(("(\\mono{%s} $\\rightarrow$ \\mono{%s})" % (src,dst) for (src,dst) in bad_c["boundaries"]))
              render("\\item %s (%s times %s than untyped average)\n%s" % (bad_c["id"], vs_u, descr, edges_str))
          render("\\end{itemize}")
          num_mg_figs = min(5, len(results["worst"]))
          render("\n\\subsection{Top %s Worst (gradual) Configurations}" % num_mg_figs)
          render("Untyped modules are \\textbf{blue} and typed modules are \\emph{orange}.\n")
          for i in range(num_mg_figs):
              config = results["worst"][i]["id"]
              time   = results["worst"][i]["time"]
              bnds   = results["worst"][i]["boundaries"]
              vs_u, descr = difference(time, untyped["mean"])
              fname = plot.module_graph(results["graph"]
                                 ,results["title"]
                                 ,"Config %s: %s times %s than untyped" % (config, vs_u, descr)
                                 ,boundaries=bnds)
              render(figure(fname))
          render("\n\\subsection{Best (gradual) Configurations}")
          render("The best %s configurations and their boundaries are:" % len(results["best"]))
          render("\\begin{itemize}")
          for bad_c in results["best"]:
              vs_u, descr = difference(bad_c["time"], untyped["mean"])
              edges_str = "  \\begin{itemize}\n  \\item %s\n  \\end{itemize}" % "\n  \\item ".join(("(\\mono{%s} $\\rightarrow$ \\mono{%s})" % (src,dst) for (src,dst) in bad_c["boundaries"]))
              render("\\item %s (%s times %s than untyped average)\n%s" % (bad_c["id"], vs_u, descr, edges_str))
          render("\\end{itemize}")
          num_mg_figs = min(5, len(results["best"]))
          render("\n\\subsection{Top %s Best (gradual) Configurations}" % num_mg_figs)
          render("Untyped modules are \\textbf{blue} and typed modules are \\emph{orange}.\n")
          for i in range(num_mg_figs):
              config = results["best"][i]["id"]
              time   = results["best"][i]["time"]
              bnds   = results["best"][i]["boundaries"]
              vs_u, descr = difference(time, untyped["mean"])
              fname = plot.module_graph(results["graph"]
                                 ,results["title"]
                                 ,"Config %s: %s times %s than untyped" % (config, vs_u, descr)
                                 ,boundaries=bnds)
              render(figure(fname))
          if "fixed" in results and results["fixed"]:
              render("\n\\subsection{Fixing individual modules}")
              for fig in results["fixed"]:
                  render(figure(fig))
        render("\\end{document}")
    print("Results saved as %s" % outfile)
    return

