"""
Exploring L, M, N figures.

- `N-deliverable` overhead no worse than `(N *100)%` slowdown over untyped.
- `N/M-usable` overhead worse than `(N*100)%` but better than `(M*100)%`
- `L-step N/M-usable` reach `N/M-usable` after at most `L` type conversion steps.


"""

from TabfileSummary import TabfileSummary
from ModuleGraph import ModuleGraph
import config
import constants
import os
import latex
import plot
import plot3
import shell
import util
import numpy as np

RED_LINE = {"xpos" : constants.DELIVERABLE
            ,"color" : "r"
            ,"style" : "solid"
            ,"width" : 1
}
ORANGE_LINE = {"xpos" : constants.ACCEPTABLE
               ,"color" : "tomato"
               ,"style" : "dashed"
               ,"width" : 4
}

class LmnSummary(TabfileSummary):

    def __init__(self, *args, **kwargs):
        TabfileSummary.__init__(self, *args, **kwargs)
        # Commonly-used state
        self.num_configs = self.get_num_configurations()
        self.num_modules = self.get_num_modules()
        self.base_runtime = self.stats_of_untyped()["mean"]

    def render(self, output_port):
        """
            Print experimental L-M-N graphs,
            save everything to a .tex file for easy reading.
        """
        title = "L-M-N Results: %s" % self.project_name
        self.render_title(output_port, title)
        self.render_summary(output_port)
        self.render_overall(output_port)
        # 
        self.render_n(output_port, Nmax=20)
        self.render_mn(output_port, Nmax=constants.ACCEPTABLE, Mskip=2)
        self.render_lmn(output_port
                        ,Nmax=constants.ACCEPTABLE
                        ,Mmax=2*constants.ACCEPTABLE
                        ,Lvals=[int(x) for x in np.linspace(0, self.num_modules-1, 4)])
        print(latex.end(), file=output_port)

    def render_overall(self, output_port):
        """
            Show overall results in a table
        """
        print(latex.subsection("Summary Results"), file=output_port)
        best_cfg = self.best_rows(config.is_gradual, lambda x,y: self.stats_by_config[x]["mean"] > self.stats_by_config[y]["mean"], limit=1)[0]
        worst_cfg = self.best_rows(config.is_gradual, lambda x,y: self.stats_by_config[x]["mean"] < self.stats_by_config[y]["mean"], limit=1)[0]
        title = ["---", "mean", "95-confidence"]
        rows = [[label, stat["mean"], "%s--%s" % (stat["ci"][0], stat["ci"][1])]
                for (label, stat)
                in [("Untyped", self.stats_of_untyped())
                   ,("Avg. Gradual", self.stats_of_predicate(config.is_gradual))
                   ,("Best Gradual (%s)" % best_cfg, self.stats_of_config(best_cfg))
                   ,("Worst Gradual (%s)" % worst_cfg, self.stats_of_config(worst_cfg))
                   ,("Typed", self.stats_of_typed())]]
        print(latex.table(title, rows), file=output_port)

    ### -----------------------------------------------------------------------------

    def render_n(self, output_port, Nmax=10):
        """
            Visualize the N-deliverable configurations.
            - build a mapping (N -> good configs)
            - graph the map
        """
        print(latex.newpage(), file=output_port)
        print(latex.subsection("N-deliverable graphs"), file=output_port)
        # x-axis lines, for all graphs
        vlines = [RED_LINE, ORANGE_LINE]
        # graph counts, continuously
        y_fun = self.num_faster_than
        graph = plot.line([0, Nmax]
                          ,[y_fun]
                          ,title="Num. variations with runtime less than N-times untyped" # title
                          ,xlabel="N (overhead factor)"
                          ,ylabel="Num. deliverable"
                          ,samples=100
                          ,output="%s/%s.png" % (self.output_dir, "count-vs-N")
                          ,ymax=self.num_configs
                          ,vlines=vlines)
        print(latex.figure(graph), file=output_port)

    ### -----------------------------------------------------------------------------

    def render_mn(self, output_port, Nmax=None, Mskip=None):
        """
            Zoom in on a range of the graph (acceptable)
            Draw multiple lines showing various M values

            Arguments:
            Options:
            - Nmax : largest N value to display
            - index : x-values for grid display
        """
        print(latex.newpage(), file=output_port)
        print(latex.subsection("MN-acceptable graphs"), file=output_port)
        print("The acceptable variations for an N/M pair lie between the lowest line (N-deliverable) and one of the higher lines.\n", file=output_port)
        y_funs = []
        lbls = []
        for diff in range(0, Nmax, Mskip):
            y_funs.append(self.faster_offset(diff))
            lbls.append(diff)
        lines = plot.line([0, Nmax]
                          ,y_funs
                          ,title="Num. variations with runtime less than M-times untyped\n(legend is the difference M-N)"
                          ,xlabel="Overhead over untyped"
                          ,ylabel="Num. acceptable"
                          ,samples=100
                          ,output="%s/%s.png" % (self.output_dir, "count-vs-NM")
                          ,linelabels=lbls
                          ,ymax=self.num_configs)
        print(latex.figure(lines), file=output_port)

    ### -----------------------------------------------------------------------------

    def render_lmn(self, output_port, Nmax=None, Mmax=None, Lvals=None):
        print(latex.newpage(), file=output_port)
        print(latex.subsection("L-M-N graphs"), file=output_port)
        print("Dark is BAD, implies a low percentage of all variations are practical\n", file=output_port)
        for L in Lvals:
            print(latex.subsubsection("L = %s" % L), file=output_port)
            figs = plot3.contour([0, Nmax]
                                 ,[0, Mmax]
                                 ,self.faster_path(L)
                                 ,title="L-N-M contour"
                                 ,xlabel="N (overhead factor)"
                                 ,ylabel="M (>= N)"
                                 ,zlabel="Num. variations"
                                 ,output="%s/%s" % (self.output_dir, "lmn-contour-%sstep" % L)
                                 ,zlim=self.num_configs)
            print("\\hbox{\\hspace{-5.2cm}", file=output_port)
            print(latex.figure(figs[0], width_scale=0.9), file=output_port)
            print(latex.figure(figs[1], width_scale=0.9), file=output_port)
            print("}", file=output_port)

    ### -------------------------------------------------------

    def faster_path(self, num_steps):
        """
            Count the number of variations better than,
            or within `num_steps` to a better-than variation.
        """
        # TODO we ignore N, except as a filter for invalids?
        return (lambda N,M:
                0 if (N > M)
                else sum((1 for cfg in self.all_configurations()
                          # Current is better, or can reach a better
                          if ((self.stats_of_config(cfg)["mean"] < M * self.base_runtime)
                              or
                              any((self.stats_of_config(cfg2)["mean"] < M * self.base_runtime
                                   for cfg2 in config.next_iter(cfg, num_steps)))))))

    # Dumb functions, to get scope for N
    def faster_offset(self, diff):
        return (lambda N: self.num_faster_than(N + diff))
    def percent_offset(self, diff):
        return (lambda N: self.percent_faster_than(N + diff))

    def num_faster_than(self, N):
        """
            Count the number of configurations with performance
            less than N times the untyped baseline
        """
        return sum((1 for cfg in self.all_configurations()
                    if self.stats_of_config(cfg)["mean"] < N * self.base_runtime))

    def percent_faster_than(self, N):
        return self.num_faster_than(N) / self.num_configs
