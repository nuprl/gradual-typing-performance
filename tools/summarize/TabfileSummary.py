"""
Parsing, creating, and computing statistics over .tab files.

This is the canonical file for reasoning about 'ground truth'
experiment results.
"""

from AbstractSummary import AbstractSummary
from ModuleGraph import ModuleGraph
import config
import constants
import os
import latex
import shell
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
        self.project_name = fname.rsplit(".", 1)[0].rsplit("/")[-1]
        # Try to set module graph
        self.graph = ModuleGraph(fname)
        self.module_names = self.graph.get_module_names()

    def results_of_config(self, config):
        return util.fold_file(self.source
                              , None
                              , lambda acc, row: acc or (util.stats_of_row([int(x) for x in row[1:]])
                                                         if row[0] == config else None))

    def render(self, output_port):
        title = "Ground Truth Results: %s" % self.project_name
        self.render_title(output_port, title)
        self.render_summary(output_port)
        best_cfgs = self.best_rows(config.is_gradual, lambda x,y: self.stats_by_config[x]["mean"] > self.stats_by_config[y]["mean"])
        worst_cfgs = self.best_rows(config.is_gradual, lambda x,y: self.stats_by_config[x]["mean"] < self.stats_by_config[y]["mean"])
        print(latex.subsection("Aggregate Figures"), file=output_port)
        self.render_overall(output_port
                            ,("untyped", config.is_untyped)
                            ,("gradual", config.is_gradual)
                            ,("fastest(%s)" % best_cfgs[0], lambda x: x == best_cfgs[0])
                            ,("slowest(%s)" % worst_cfgs[0], lambda x: x == worst_cfgs[0])
                            ,("typed", config.is_typed))
        self.render_normalized(output_port
                            ,("untyped", config.is_untyped)
                            ,("gradual", config.is_gradual)
                            ,("top %s" % len(best_cfgs), lambda x: x in best_cfgs)
                            ,("bottom %s" % len(worst_cfgs), lambda x: x in worst_cfgs)
                            ,("typed", config.is_typed))
        self.render_absolute(output_port
                            ,*[(str(n), config.has_typed_modules(n))
                              for n in range(self.get_num_modules())])
        baseline = self.stats_of_config("0" * self.get_num_modules())["mean"]
        self.render_graphs(output_port
                          ,worst_cfgs
                          ,baseline
                          ,title="Top %s slowest gradually-typed configurations" % len(worst_cfgs))
        self.render_graphs(output_port
                          ,best_cfgs
                          ,baseline
                          ,title="Top %s fastest gradually-typed configurations" % len(best_cfgs))
        # self.render_best_configs(???)
        # self.render_edge_violins(???)
        print(latex.end(), file=output_port)

    ### rendering
    def render_summary(self, output_port):
        AbstractSummary.render_summary(self, output_port)
        print("Total of %s configurations" % self.get_num_configurations(), file=output_port)
        print("Ran each configuration %s times" % self.num_iters, file=output_port)

    def render_graphs(self, output_port, cfgs, baseline, title="Module Graphs"):
        print(latex.subsection(title), file=output_port)
        for cfg in cfgs:
            mean = self.stats_of_config(cfg)["mean"]
            diff, txt = latex.difference(mean, baseline)
            g = self.graph_config(cfg
                             ,title="Config %s: %s %s than baseline" % (cfg, diff, txt)
                             ,output="%s-graph-%s.png" % (self.project_name, cfg))
            print(latex.figure(g), file=output_port)

    ### Helpers ################################################################

    def of_tab(self, tabfile):
        """
            Initialize `stats_by_config` table from the spreadsheet `tabfile`
        """
        util.fold_file(tabfile, None, lambda acc, row: self.set_stats(row[0], util.stats_of_row([int(v) for v in row[1:]])))

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

