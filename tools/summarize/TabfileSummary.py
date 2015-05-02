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
import render
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
                              , lambda acc, row: acc or (util.stats_of_row(row[1:])
                                                         if row[0] == config else None))

    def render(self, output_port):
        self.render_title(output_port)
        self.render_summary(output_port)
        best_cfgs = self.best_rows(config.is_gradual, lambda x,y: self.stats_by_config[x]["mean"] > self.stats_by_config[y]["mean"])
        worst_cfgs = self.best_rows(config.is_gradual, lambda x,y: self.stats_by_config[x]["mean"] < self.stats_by_config[y]["mean"])
        print(render.subsection("Aggregate Figures"), file=output_port)
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
        print(render.end(), file=output_port)

    ### rendering
    def render_title(self, output_port):
        print(render.PREAMBLE, file=output_port)
        print(render.section("Results: %s" % self.project_name), file=output_port)

    def render_summary(self, output_port):
        print(render.subsection("Module Summary"), file=output_port)
        print(render.list(["\\mono{%s}" % k for k in self.graph.module_names], numbers=True), file=output_port)
        print("Total of %s configurations" % self.get_num_configurations(), file=output_port)
        print("Ran each configuration %s times" % self.num_iters, file=output_port)

    def render_overall(self, output_port, *labeled_preds):
        labels = [k for (k,v) in labeled_preds]
        preds  = [v for (k,v) in labeled_preds]
        results = [self.stats_of_predicate(p) for p in preds]
        baseline = (labels[0], results[0])
        print(render.subsection("Overall Runtimes"), file=output_port)
        print(render.list([" ".join(["Average"
                                    ,"\\textbf{%s}" % tag
                                    ,"runtime"
                                    ,str(row["mean"])
                                    ,"(%s times %s than %s)" % (render.difference(row["mean"], baseline[1]["mean"])[0], render.difference(row["mean"], baseline[1]["mean"])[1], baseline[0])
                                    ,render.list(["Median: %s" % row["median"]
                                                 ,"Min: %s" % row["min"]
                                                 ,"Max: %s" % row["max"]
                                                 ,"95\\%% confidence: %s\\textendash~%s" % (row["ci"][0], row["ci"][1])])])
                           for (tag, row) in zip(labels, results)]), file=output_port)

    def render_normalized(self, output_port, *labeled_preds):
        labels = [k for (k,_) in labeled_preds]
        preds  = [v for (_,v) in labeled_preds]
        graph  = self.graph_normalized_runtimes(preds
                                               ,"%s-normalized.png" % self.project_name
                                               , xlabels=labels)
        print(render.figure(graph), file=output_port)

    def render_absolute(self, output_port, *labeled_preds):
        labels = [k for (k,_) in labeled_preds]
        preds  = [v for (_,v) in labeled_preds]
        graph  = self.graph_absolute_runtimes(preds
                                             ,"Num. typed modules"
                                             ,labels
                                             ,"%s-absolute.png" % self.project_name)
        print(render.figure(graph), file=output_port)

    def render_graphs(self, output_port, cfgs, baseline, title="Module Graphs"):
        print(render.subsection(title), file=output_port)
        for cfg in cfgs:
            mean = self.stats_of_config(cfg)["mean"]
            diff, txt = render.difference(mean, baseline)
            g = self.graph_config(cfg
                             ,title="Config %s: %s %s than baseline" % (cfg, diff, txt)
                             ,output="%s-graph-%s.png" % (self.project_name, cfg))
            print(render.figure(g), file=output_port)

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

