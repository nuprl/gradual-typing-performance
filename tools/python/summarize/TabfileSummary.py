"""
Parsing, creating, and computing statistics over .tab files.

This is the canonical file for reasoning about 'ground truth'
experiment results.
"""

from AbstractSummary import AbstractSummary
from ModuleGraph import ModuleGraph
import config
import constants
import networkx
import os
import latex
import shell
import util

class TabfileSummary(AbstractSummary):

    def __init__(self, fname, out_dir=constants.OUTPUT_DIR):
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
        self.output_dir = out_dir

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
        self.render_overall(output_port
                            ,("untyped", config.is_untyped)
                            ,("gradual", config.is_gradual)
                            ,("fastest(%s)" % best_cfgs[0], lambda x: x == best_cfgs[0])
                            ,("slowest(%s)" % worst_cfgs[0], lambda x: x == worst_cfgs[0])
                            ,("typed", config.is_typed))
        print("Num. within 2x: %s" % len(self.stats_of_predicate(lambda x: self.stats_by_config[x]["mean"] < 2 * self.stats_by_config["0" * self.get_num_modules()]["mean"])), file=output_port)
        print(latex.subsection("Aggregate Figures"), file=output_port)
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
        # self.render_all_paths(output_port, [1,2,3,4])
        # self.render_cutoff_paths(output_port)
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

    def render_all_paths(self, output_port, transitivity=[1]):
        """
            Options:
            - transitivity : How many transitive edges to include.
                             By default, edges are only between consecutive levels.
                             If argument is a list, analyzes one graph of each transitivity
        """
        print(latex.subsection("Lattices+Freedom"), file=output_port)
        typed_mean   = self.stats_of_config("1" * self.get_num_modules())["mean"]
        untyped_mean = self.stats_of_config("0" * self.get_num_modules())["mean"]
        rows = []
        for trans in transitivity:
            print("Building lattice for %s with transitivity %s" % (self.project_name, trans))
            lattice = self.make_lattice(transitivity=trans)
            untyped_config = "0" * self.get_num_modules()
            typed_config   = "1" * self.get_num_modules()
            paths   = networkx.all_simple_paths(lattice, source=untyped_config, target=typed_config)
            weights = [self.max_weight(lattice, path)
                       for path in paths]
            num_release_u = sum((1 for x in weights if x < (untyped_mean * constants.DELIVERABLE)))
            num_dev_u     = sum((1 for x in weights if x < (untyped_mean * constants.USABLE)))
            num_x_u     = sum((1 for x in weights if x < (untyped_mean * 15)))
            num_release_t = sum((1 for x in weights if x < (typed_mean * constants.DELIVERABLE)))
            num_dev_t     = sum((1 for x in weights if x < (typed_mean * constants.USABLE)))
            num_x_t     = sum((1 for x in weights if x < (typed_mean * 15)))
            num_paths     = len(weights)
            rows.append([str(trans)
                         ,str(num_paths)]
                        + ["%s (%s\\%%)" % (x, round((x / num_paths) * 100, 2))
                           for x in [num_release_u
                                     , num_dev_u, num_x_u
                                     ,num_release_t
                                     ,num_dev_t,num_x_t]])
        print(latex.table(["Fuel", "Total", "$<$ 2x untyped", "$<$ 4x untyped", "$<$ 15x untyped", "$<$ 2x typed", "$<$ 4x typed", "$<$ 15x typed"]
                          , rows), file=output_port)

    def render_cutoff_paths(self, output_port, xmax=None, ymax=None):
        print(latex.subsection("Paths with cutoff"), file=output_port)
        # Build a lattice for each cluster size {1 .. num_modules-1}
        print("Building lattice for %s" % self.project_name)
        lattice = self.make_lattice(transitivity=self.get_num_modules())
        # xmax = max((e[2]["weight"] for e in lattice.edges_iter(data=True)))
        untyped_config = "0" * self.get_num_modules()
        untyped_mean   = self.stats_of_config(untyped_config)["mean"]
        typed_config   = "1" * self.get_num_modules()
        typed_mean     = self.stats_of_config(typed_config)["mean"]
        rows = []
        for group_size in range(1, self.get_num_modules()):
            # For each group size (freedom to type exactly N modules at once)
            # make a histogram of max overhead edges along each path
            # (Shows the number of paths that have 'really bad' overhead)
            print("Computing paths for group size '%s'" % group_size)
            cutoff  = 1 + (self.get_num_modules() - group_size)
            paths   = networkx.all_simple_paths(lattice, source=untyped_config, target=typed_config, cutoff=cutoff)
            weights = [self.max_weight(lattice, path)
                       for path in paths
                       if len(path) == 1+cutoff]
            num_release_u = sum((1 for x in weights if x < (untyped_mean * constants.DELIVERABLE)))
            num_dev_u     = sum((1 for x in weights if x < (untyped_mean * constants.USABLE)))
            num_x_u     = sum((1 for x in weights if x < (untyped_mean * 15)))
            num_release_t = sum((1 for x in weights if x < (typed_mean * constants.DELIVERABLE)))
            num_dev_t     = sum((1 for x in weights if x < (typed_mean * constants.USABLE)))
            num_x_t     = sum((1 for x in weights if x < (typed_mean * 15)))
            num_paths     = len(weights)
            rows.append([str(cutoff)
                         ,str(num_paths)]
                        + ["%s (%s\\%%)" % (x, round((x / num_paths) * 100, 2))
                           for x in [num_release_u
                                     ,num_dev_u,num_x_u
                                     ,num_release_t
                                     ,num_dev_t,num_x_t]])
        print(latex.table(["Path length", "Total", "$<$ 2x untyped", "$<$ 4x untyped", "$<$ 15x untyped", "$<$ 2x typed", "$<$ 4x typed", "$<$ 15x typed"]
                          ,rows), file=output_port)

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

    def make_lattice(self, transitivity=1):
        """
            Create a try-everything lattice showing all the ways of moving
             from untyped to typed.
            Args:
            - configs : Iterable of all configurations (nodes) to include.
                        Must include an untyped and a typed configuration.
        """
        g = networkx.DiGraph()
        for cfg in self.all_configurations():
            g.add_node(cfg)
            w = self.stats_of_config(cfg)["mean"]
            for prev in config.previous_iter(cfg, transitivity):
                # SWAG networkx ignore duplicates SWAGSWAGS
                g.add_edge(prev, cfg, weight=w)
        return g

    def max_weight(self, lattice, path):
        """
            Return the max weight of edges along a networkx path.
            2015-05-07: really, this could just be MAX of cached sample stats
        """
        acc = 0
        prev = None
        for node in path:
            if prev is not None:
                acc = max(acc, lattice[prev][node]["weight"])
            prev = node
        return acc

