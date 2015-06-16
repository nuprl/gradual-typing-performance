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
        Nmap = self.make_Nmap()
        small_Nmap = self.make_Nmap(skip=0.5)
        print("NSMAP len is %s" % len(Nmap))
        print("SMALLL NSMAP len is %s" % len(small_Nmap))
        self.render_n(output_port, Nmap)
        self.render_mn(output_port, small_Nmap, Nmax=10, Mskip=2)
        self.render_lmn(output_port, Nmap, Nmax=constants.ACCEPTABLE)
        self.render_lmn(output_port, Nmap)
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

    def render_n(self, output_port, Nmap):
        """
            Visualize the N-deliverable configurations.
            - build a mapping (N -> good configs)
            - graph the map
        """
        print(latex.newpage(), file=output_port)
        print(latex.subsection("N-deliverable graphs"), file=output_port)
        # x-axis lines, for all graphs
        vlines = [{"xpos" : constants.DELIVERABLE + 0.5
                   ,"color" : "r"
                   ,"style" : "solid"
                   ,"width" : 4
                   }
                  ,{"xpos" : constants.ACCEPTABLE + 0.5
                    ,"color" : "tomato"
                    ,"style" : "dashed"
                    ,"width" : 5
                }]
        # graph histogram of counts
        counts = [len(x) for x in Nmap]
        hist_graph = plot.bar(range(0, len(Nmap)) # x-values
                             ,counts # y-values
                             ,"Number deliverable variations vs. N" # title
                             ,"N" # x-label
                             ,"Num. deliverable" # y-label
                             ,alpha=0.8
                             ,output="%s/%s.png" % (self.output_dir, "count-vs-N")
                             ,vlines=vlines
                             ,ymax=max(counts))
        print(latex.figure(hist_graph), file=output_port)
        # graph plot of percents
        percents = [len(x) / self.num_configs for x in Nmap]
        pct_graph = plot.bar(range(0, len(Nmap))
                             ,percents
                             ,"Percent deliverable vs. N"
                             ,"N"
                             ,"% deliverable"
                             ,alpha=0.8
                             ,output="%s/%s.png" % (self.output_dir, "percent-vs.N")
                             ,vlines=vlines
                             ,ymax=1)
        print(latex.figure(pct_graph), file=output_port)

    def render_mn(self, output_port, Nmap, Nmax=None, Mskip=1):
        """
            Zoom in on a range of the graph (acceptable)
            Draw multiple lines showing various M values
        """
        print(latex.newpage(), file=output_port)
        print(latex.subsection("MN-acceptable graphs"), file=output_port)
        counts = [len(x) for x in Nmap]
        lines = plot.dots(range(0, Nmax)
                         ,[util.pad(counts[i:i+Nmax], self.num_configs, Nmax)
                           for i in range(0, Nmax, Mskip)]
                         ,"Number acceptable as M increases (legend is N-M)"
                         ,"N"
                         ,"Num. acceptable"
                         ,skip=Mskip
                         ,output="%s/%s.png" % (self.output_dir, "count-vs-NM")
                         ,vlines = [{"xpos" : constants.DELIVERABLE
                                    ,"color" : "r"
                                    ,"style" : "solid"
                                    ,"width" : 2
                                    }])
        print(latex.figure(lines), file=output_port)
        print("ON TO PERCENTS")
        percents = [x/self.num_configs for x in counts]
        pcts = plot.dots(range(0, Nmax)
                         ,[util.pad(percents[i:i+Nmax], 1, Nmax)
                           for i in range(0, Nmax, Mskip)]
                         ,"Percent acceptable as M increases (legend is N-M)"
                         ,"N"
                         ,"Percent acceptable"
                         ,output="%s/%s.png" % (self.output_dir, "percent-vs-NM")
                         ,skip=Mskip
                         ,vlines = [{"xpos" : constants.DELIVERABLE
                                    ,"color" : "r"
                                    ,"style" : "solid"
                                    ,"width" : 1
                                    }])
        print(latex.figure(pcts), file=output_port)

    def render_lmn(self, output_port, Nmap, Nmax=None):
        print(latex.newpage(), file=output_port)
        print(latex.subsection("L-M-N graphs"), file=output_port)
        LNmap = self.make_Lmap(Nmap)
        figs = plot3.contour(range(0, (Nmax or len(LNmap[0])))
                     ,range(0, len(LNmap))
                     ,[[len(ds) for ds in Nmap][:(Nmax or len(Nmap))]
                       for Nmap in LNmap]
                     ,"L-N contour" # title
                     ,xlabel="N"
                     ,ylabel="L"
                     ,zlabel="num. deliverable"
                     ,output="%s/%s" % (self.output_dir, "ln-contour-%sn" % (len(LNmap)))
                     ,zlim=self.num_configs)
        print("\\hbox{\\hspace{-5.2cm}", file=output_port)
        print(latex.figure(figs[0], width_scale=0.9), file=output_port)
        print(latex.figure(figs[1], width_scale=0.9), file=output_port)
        print("}", file=output_port)
        print(latex.figure(figs[2], width_scale=0.9), file=output_port)

    ### -------------------------------------------------------

    def make_Nmap(self, skip=1):
        """
            Create a vector,
            - indices are values of N,
            - values are N-deliverable configurations
        """
        Nmap = []
        N = 0
        gen_Npred = (lambda n:
                     (lambda cfg:
                      self.stats_of_config(cfg)["mean"] <= n * self.base_runtime))
        # add to Nmap until every configuration is deliverable
        deliverable = self.configs_of_predicate(gen_Npred(N))
        while (len(deliverable) < self.num_configs):
            Nmap.append(deliverable)
            N += skip
            deliverable = self.configs_of_predicate(gen_Npred(N))
        Nmap.append(deliverable)
        return util.pad(Nmap, list(self.all_configurations()), constants.ACCEPTABLE)

    def make_Lmap(self, Nmap):
        """
            Create a 2D vector
            - indices are L-values
            - values are Nmaps (indices N-values, values N-deliverable configurations)
        """
        Lmap = [Nmap]
        prev_nmap = Nmap
        for L in range(1, self.num_modules):
            N = 0
            new_nmap = []
            # Create a new Nmap by adding entries to the old one
            for i in range(0, len(Nmap)):
                dvs = []
                # Add the entries that are within L from any GOOD config
                for cfg in self.all_configurations():
                    if any((0 <= config.minus(cfg, cfg2) <= L
                           for cfg2 in prev_nmap[i])):
                        dvs.append(cfg)
                new_nmap.append(dvs)
            Lmap.append(new_nmap)
            prev_nmap = new_nmap
        return Lmap
