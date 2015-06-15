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
import shell
import util

class LmnSummary(TabfileSummary):
    ## __init__ inherited from superclass,
    # sets fields, parses .rktd and .tab files.

    def render(self, output_port):
        """
            Print experimental L-M-N graphs,
            save everything to a .tex file for easy reading.
        """
        title = "L-M-N Results: %s" % self.project_name
        self.render_title(output_port, title)
        self.render_summary(output_port)
        self.render_n(output_port)
        # self.render_mn(output_port)
        # self.render_lmn(output_port)
        print(latex.end(), file=output_port)

    def render_n(self, output_port):
        """
            Visualize the N-deliverable configurations.
            - build a mapping (N -> good configs)
            - graph the map
        """
        print(latex.newpage(), file=output_port)
        print(latex.subsection("N-deliverable graphs"), file=output_port)
        Nmap = self.make_Nmap()
        # graph histogram of counts
        counts = [len(x) for x in Nmap]
        hist_graph = plot.bar(range(0, len(Nmap)) # x-values
                             ,counts # y-values
                             ,"Num. deliverable vs. N" # title
                             ,"N" # x-label
                             ,"Num. deliverable" # y-label
                             ,alpha=0.8
                             ,output="%s/%s.png" % (self.output_dir, "count-vs-N")
                             ,ymax=max(counts))
        print(latex.figure(hist_graph), file=output_port)
        # graph plot of percents
        num_configs = self.get_num_configurations()
        percents = [len(x) / num_configs for x in Nmap]
        pct_graph = plot.bar(range(0, len(Nmap))
                             ,percents
                             ,"Percent deliverable vs. N"
                             ,"N"
                             ,"% deliverable"
                             ,alpha=0.8
                             ,output="%s/%s.png" % (self.output_dir, "percent-vs.N")
                             ,ymax=1)
        print(latex.figure(pct_graph), file=output_port)

    def render_mn(self, output_port):
        raise NotImplementedError()

    def render_lmn(self, output_port):
        raise NotImplementedError()

    ### -------------------------------------------------------

    def make_Nmap(self):
        """
            Create a vector,
            - indices are values of N,
            - values are N-deliverable configurations
        """
        Nmap = []
        N = 0
        num_configs = self.get_num_configurations()
        base_runtime = self.stats_of_untyped()["mean"]
        gen_Npred = (lambda n:
                     (lambda cfg:
                      self.stats_of_config(cfg)["mean"] < n * base_runtime))
        # add to Nmap until every configuration is deliverable
        deliverable = self.configs_of_predicate(gen_Npred(N))
        while len(deliverable) < num_configs:
            Nmap.append(deliverable)
            N += 1
            deliverable = self.configs_of_predicate(gen_Npred(N))
        Nmap.append(deliverable)
        return Nmap
