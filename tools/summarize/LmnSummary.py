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
        self.render_overall(output_port)
        self.render_n(output_port)
        # self.render_mn(output_port) # TODO
        self.render_lmn(output_port)
        print(latex.end(), file=output_port)


    def render_overall(self, output_port):
        """
            Show overall results
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

    def render_n(self, output_port):
        """
            Visualize the N-deliverable configurations.
            - build a mapping (N -> good configs)
            - graph the map
        """
        print(latex.newpage(), file=output_port)
        print(latex.subsection("N-deliverable graphs"), file=output_port)
        Nmap = self.make_Nmap()
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
        num_configs = self.get_num_configurations()
        percents = [len(x) / num_configs for x in Nmap]
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

    def render_mn(self, output_port):
        print(latex.newpage(), file=output_port)
        print(latex.subsection("MN-acceptable graphs"), file=output_port)
        Nmap = self.make_Nmap()
        raise NotImplementedError()

    def render_lmn(self, output_port):
        print(latex.newpage(), file=output_port)
        print(latex.subsection("L-M-N graphs"), file=output_port)
        LNmap = self.make_Lmap()
        cgraph = plot3.contour(LNmap
                     ,"L-N contour" # title
                     ,xlabel="N"
                     ,ylabel="L"
                     ,zlabel="num. deliverable"
                     ,output="%s/%s.png" % (self.output_dir, "ln-contour")
                     ,zlim=self.get_num_configurations())
        print(latex.figure(cgraph), file=output_port)

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

    def make_Lmap(self):
        """
            Create a 2D vector
            - indices are L-values
            - values are Nmaps (indices N-values, values N-deliverable configurations)
        """
        Lmap = []
        num_configs = self.get_num_configurations()
        base_runtime = self.stats_of_untyped()["mean"]
        graph = self.make_lattice()
        gen_LNpred = (lambda l,n:
                      (lambda cfg:
                       self.stats_of_config(self.best_reachable(graph, cfg, l))["mean"] < n * base_runtime))
        longest_nmap = 0
        for L in range(0, self.get_num_modules()):
            Nmap = []
            N = 0
            deliverable = self.configs_of_predicate(gen_LNpred(L, N))
            while (len(deliverable) < num_configs):
                Nmap.append(len(deliverable)) # could generalize to `deliverable`, but whatever we only want to plot the count
                N += 1
                deliverable = self.configs_of_predicate(gen_LNpred(L, N))
            Nmap.append(len(deliverable))
            longest_nmap = max(longest_nmap, len(Nmap))
            Lmap.append(Nmap)
        # Return a padded array. Makes for a better surface.
        print("LMAP = %s" % Lmap)
        return [self.pad(Nmap, longest_nmap, num_configs)
                for Nmap in Lmap]

    def pad(self, row, length, default):
        return row + ([default] * (length - len(row)))

    def best_reachable(self, graph, start_cfg, num_steps):
        """
            Return the config with the best performance
            out of all configs within `num_steps` of `start_cfg`.
        """
        best_mean = self.stats_of_config(start_cfg)["mean"]
        best_cfg = start_cfg
        curr_neighbors = graph[start_cfg].keys()
        next_neighbors = []
        steps_left = num_steps - 1
        while curr_neighbors:
            for cfg in curr_neighbors:
                if config.num_typed_modules(cfg) < config.num_typed_modules(start_cfg):
                    raise ValueError("graph went backwards from '%s' to '%s'" % (start_cfg, cfg))
                mean = self.stats_of_config(cfg)["mean"]
                if mean < best_mean:
                    best_mean = mean
                    best_cfg = cfg
                if num_steps > 0:
                    next_neighbors.extend(graph[cfg].keys())
            curr_neighbors = next_neighbors
            next_neighbors = []
            steps_left -= 1
        return best_cfg
