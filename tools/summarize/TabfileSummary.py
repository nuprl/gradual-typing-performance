"""
Parsing, creating, and computing statistics over .tab files.

This is the canonical file for reasoning about 'ground truth'
experiment results.
"""

from AbstractSummary import AbstractSummary
from ModuleGraph import ModuleGraph
import config
import constants
import math
import os
import plot
import shell
import sorted_buffer
import statistics
import util

class TabfileSummary(AbstractSummary):

    def __init__(self, fname):
        # Init data
        if fname.endswith(".rktd"):
            # Parse the .rktd file into a .tab file
            self.source = self.of_rktd(fname)
            self.of_tab(self.source)
        elif fname.endswith(".tab"):
            self.source = fname
            self.of_tab(fname)
        else:
            raise NotImplementedError("Tabfile cannot parse '%s'" % fname)
        # Set project name
        self.project_name = fname.rsplit(".", -1)[1].rsplit("/")[0]
        # Try to set module graph
        self.graph = ModuleGraph(fname)
        self.module_names = self.graph.get_module_names()

    def results_of_config(self, config):
        return util.fold_file(self.source
                              , None
                              , lambda acc, row: acc or (util.stats_of_row(row[1:])
                                                         if row[0] == config else None))

    def render(self, output_port):
        self.render_title(output_port)
        self.render_basics(output_port)
        self.render_normalized(???)
        self.render_absolute(???)
        self.render_worst_configs(???)
        self.render_best_configs(???)
        self.render_edge_violins(???)


    ### rendering
    def render_title(self, output_port):
        print(render.PREAMBLE, file=output_port)
        print(render.section("Results: %s" % self.project_name), file=output_port)

    def render_basics(self, output_port):
        print(render.subsection("Module Summary"), file=output_port)
        print(render.enumerate(["\\mono{%s}" % k for (k,v) in self.graph.module_names]), file=output_port)
        print("Total of %s configurations" % self.get_num_configurations(), file=output_port)
        print("Ran each configuration %s times" % self.num_iters, file=output_port)
        print(render.subsection("Overall Runtimes"), file=output_port)
        print(render.itemize(TODO))
        
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

    ### Helpers ################################################################

    def of_tab(self, tabfile):
        """
            Initialize `stats_by_config` table from the spreadsheet `tabfile`
        """
        util.fold_file(tabfile, None, lambda acc, row: self.stats_by_config[row[0]] = util.stats_of_row(row[1:]))

    def of_rktd(self, rktdfile):
        """ (-> Path-String Path-String)
            Input: a .rktd file; the results of running the benchmarks.
            Output: the string name of a newly-generated .tab file
                    (a more human-readable and Python-parse-able version of the .rktd)
        """
        # We have a Racket script to generate the .tab file,
        # make sure it exists.
        print("Parsing a .tab file from the raw source '%s'" % rktdfile)
        sexp_to_tab = shell.find_file("sexp-to-tab.rkt")
        if not sexp_to_tab:
            raise ValueError("Could not access 'sexp_to_tab' script. Cannot parse '%s'." % rktdfile)
        shell.execute("racket %s %s" % (sexp_to_tab, rktdfile))
        # Strip the suffix from the input file, replace with .tab
        return "%s.tab" % rktdfile.rsplit(".", 1)[0]

################################################################################

def all_cells_matching(tabfile, config_pred):
    """
        Return a list of all data cells in rows whose titles
        match `config_pred`.
    """
    def process_row(row):
        title = row[0]
        if config_pred(title):
            return [int(x) for x in row[1::]]
        else:
            return []
    return util.fold_file(tabfile, [], lambda acc,row: acc + process_row(row))

def bucketize(fname, get_bkt, num_buckets, pred=None):
    """
        Group the rows in `fname` into buckets,
        using `get_bkt` to assign a row of the file to a bucket.
        Return the collected list of lists
        (todo? optionally allow 'edit_row' function,
          instead of mushing all values into a list)
    """
    init = [[] for _ in range(1+num_buckets)]
    def add_row(acc, row):
        # TODO don't graph all points because ... (I had a reason at some point)
        if (pred is None) or (pred(row[0])):
            acc[get_bkt(row[0])].extend([int(x) for x in row[1::]])
        return acc
    return util.fold_file(fname, init, add_row)

def count_modules(fname):
    """ (-> Path-String Nat)
        Count the number of modules in the project.
    """
    with open(fname, "r") as f:
        _     = next(f)
        row   = next(f)
        title = row.split(constants.SEP, 1)[0]
    return len(title)

def count_runs(fname):
    """
        Count the number of runs recorded for each configuration in
        `fname`.

        2015-04-20: Warning, the count is not 'exact'.
          It trusts the title row, so if any data row has fewer
          columns than the title does we do not catch the error.
    """
    with open(fname, "r") as f:
        hdr = next(f)
    title = hdr.split(constants.SEP, 1)[0]
    if title != "Run":
        raise ValueError("Expected first column of '%s' to be labeled 'Run', got label '%s'" % (fname, title))
    last_index = hdr.rsplit(constants.SEP, 1)[-1]
    return int(last_index)

def main(tabfile, dgraph):
    """ (-> Path-String GraphDict Result)
        Input: a .tab file, an overall summary of running all configurations
               of a project.
        Output: a Result object.
    """
    print("Collecting results from ground truth data '%s'" % tabfile)
    fname = util.strip_suffix(tabfile).rsplit("/", 1)[-1]
    num_modules = count_modules(tabfile)
    num_configs = 2 ** num_modules
    print("Project contains %s modules (%s configurations)" % (num_modules, num_configs))
    u_raw = all_cells_matching(tabfile, config.is_untyped)
    g_raw = all_cells_matching(tabfile, lambda x: not (config.is_typed(x) or config.is_untyped(x)))
    t_raw = all_cells_matching(tabfile, config.is_typed)
    ugt_violin = plot.violin([u_raw, g_raw, t_raw]
                            ,"%s_untyped-vs-gradual-vs-typed" % fname
                            ,"Configuration"
                            ,"Runtime (ms)"
                            ,xlabels=["untyped","gradual\n(all configs)","typed"]) if u_raw and g_raw and t_raw else None
    # Collect absolute BEST and WORST times+configs
    ten_percent = max(3, min(10, int(0.10 * num_configs)))
    best_cfg_and_times  = best_rows(tabfile
                        ,ten_percent
                        ,lambda acc,tmp: tmp[1] < acc[1]
                        ,lambda row:(row[0], int(statistics.mean([int(x) for x in row[1::]]))))
    worst_cfg_and_times = best_rows(tabfile
                        ,ten_percent
                        ,lambda acc,tmp: acc[1] < tmp[1]
                        ,lambda row:(row[0], int(statistics.mean([int(x) for x in row[1::]]))))
    stats = {
        "title"    : fname
        ,"runs"    : count_runs(tabfile)
        ,"graph"   : dgraph
        ,"ugt"     : {"img" : ugt_violin
                     ,"summary" : {"untyped" : basic_row_stats(u_raw)
                                  ,"gradual" : basic_row_stats(g_raw)
                                  ,"typed"   : basic_row_stats(t_raw)}}
        ,"best"    : [config.basic_stats(v[0], v[1], dgraph)
                      for v in best_cfg_and_times if v is not None]
        ,"worst"   : [config.basic_stats(v[0], v[1], dgraph)
                      for v in worst_cfg_and_times if v is not None]
        ,"bucketed": plot.violin(bucketize(tabfile, config.num_typed_modules, num_modules)
                           ,"%s_by-typed-modules" % fname
                           ,"Number of Typed Modules"
                           ,"Runtime (ms)"
                           ,positions=range(0, 1+num_modules)
                           ,xlabels=range(0, 1+num_modules))
        ,"fixed"   : [plot.double_violin([bucketize(tabfile, config.num_typed_modules, num_modules, pred=lambda cfg: config.untyped_at(cfg, v[0]))
                                    ,bucketize(tabfile, config.num_typed_modules, num_modules, pred=lambda cfg: config.typed_at(cfg, v[0]))]
                                    ,"%s_fixing-%s" % (fname, k)
                                    ,"Number of Typed Modules"
                                    ,"Runtime (ms)"
                                    ,legend=["%s is untyped" % k
                                            ,"%s is typed" % k])
                      for (k,v) in sorted(dgraph.items(), key=lambda item:item[1][0])]
    }
    return stats
