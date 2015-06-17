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
import util

import itertools
import math
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
        self.strategy = constants.CACHE
        # Commonly-used state
        self.num_configs = self.get_num_configurations()
        self.num_modules = self.get_num_modules()
        self.base_runtime = self.stats_of_untyped()["mean"]
        # Graph parameters
        self.Nmax = 3
        self.Mmax = 8
        self.Lvals = [0, 1, 2]
        self.num_samples = 60
        # Table, to precompute M -> num.good
        self.configs_within_overhead = self._precompute_counts()
        self.show_table()

    def show_table(self):
        print("HEY HERE IS THE CONFIGS WITHIN OVERHEAD TABLE")
        for l in [0, 1, 2]:
            print("L = %s" % l)
            print(self.configs_within_overhead[l])
        

    def render(self, output_port):
        """
            Print experimental L-M-N graphs,
            save everything to a .tex file for easy reading.
        """
        title = "L-M-N Results: %s" % self.project_name
        self.render_title(output_port, title)
        self.render_summary(output_port)
        self.render_overall(output_port)
        self.render_lmn(output_port
                        ,Nmax=self.Nmax
                        ,Mmax=self.Mmax
                        ,Lvals=self.Lvals)
        print(latex.end(), file=output_port)

    def render_overall(self, output_port):
        """
            Show overall results in a table
            TODO improve this
        """
        print(latex.subsection("Summary Results"), file=output_port)
        best_cfg = self.best_rows(config.is_gradual, lambda x,y: self.stats_by_config[x]["mean"] > self.stats_by_config[y]["mean"], limit=1)[0]
        worst_cfg = self.best_rows(config.is_gradual, lambda x,y: self.stats_by_config[x]["mean"] < self.stats_by_config[y]["mean"], limit=1)[0]
        title = ["---", "mean (ms)", "95-confidence"]
        rows = [[label, stat["mean"], "%s--%s" % (stat["ci"][0], stat["ci"][1])]
                for (label, stat)
                in [("Untyped", self.stats_of_untyped())
                   ,("Avg. Gradual", self.stats_of_predicate(config.is_gradual))
                   ,("Best Gradual (%s)" % best_cfg, self.stats_of_config(best_cfg))
                   ,("Worst Gradual (%s)" % worst_cfg, self.stats_of_config(worst_cfg))
                   ,("Typed", self.stats_of_typed())]]
        print(latex.table(title, rows), file=output_port)

    def render_lmn(self, output_port, Nmax=None, Mmax=None, Lvals=None):
        print(latex.subsection("L-M-N graphs"), file=output_port)
        figs = []
        for L in Lvals:
            figs.append(plot3.contour([0, Nmax]
                                 ,[0, Mmax]
                                 ,self.countLNM_continuous(L)
                                 ,title="L=%s step%s" % (L, "" if L==1 else "s")
                                 ,xlabel="\nN (overhead factor)"
                                 ,ylabel="\nM (≥ N)"
                                 ,zlabel="Count"
                                 ,samples=self.num_samples
                                 ,output="%s/%s" % (self.output_dir, "lmn-contour-%sstep" % L)
                                 ,zlim=self.num_configs))
        print("\\hspace{-5.5cm}\\hbox{", file=output_port)
        print(latex.figure(figs[0], width_scale=0.5), file=output_port)
        print(latex.figure(figs[1], width_scale=0.5), file=output_port)
        print(latex.figure(figs[2], width_scale=0.5), file=output_port)
        print("}", file=output_port)

    ### -----------------------------------------------------------------------------

    def countLNM_continuous(self, L):
        """
            Count the number of variations better than,
            or within `num_steps` to a better-than variation.
        TODO 0 should be None
        """
        return (lambda N,M:
                0 if (N > M) else self.countLM_continuous(L, M))

    def countLM_continuous(self, L, M_float):
        """
            Return the number of configurations with performance
             within (M * untyped).
            M can be a float.
        """
        M_lo = math.floor(M_float)
        M_hi = math.ceil(M_float)
        cached = self.configs_within_overhead[L]
        # Easy part: all configs in a lower-overhead bucket are OK
        total = sum((len(cached[i]) for i in range(0, M_lo+1)))
        if M_lo != M_hi:
            # Harder part: check all configs in the next bucket,
            # see if they have acceptable overhead
            maybe_ok_cfgs = cached[M_hi]
            max_mean = M_float * self.base_runtime
            extras = sum((1 for cfg in maybe_ok_cfgs
                          if self.stats_of_config(cfg)["mean"] <= max_mean))
            print("counting found '%s' EXTRA configs after baseline '%s' for LM=(%s, %s)" % (extras, total, L, M_float))
            total += extras
        return total

    def _precompute_counts(self):
        """
            Precompute a few mappings from (L/M) to (L/M-good configs)

            Returns:
             A table TBL such that TBL[L][M] returns the list of configurations
              that are L-M-(M-1) acceptable.
             i.e. you get the _names_ of all configurations worse than (M-1)
              but still within M times untyped.

            Runtime:
            - L passes over all configurations, so L*(2**N) where N = num modules
            - each pass does
              [factorial(L) multiplications] and
              [up to M increments] per configuration
        """
        LM_table = []
        for L in self.Lvals:
            print("precomputing M-acceptables for L=%s" % L)
            row = [[]] # Skip the 0 step
            unsorted_configs = self.all_configurations()
            for M in range(1, self.Mmax+1):
                good_iter, rest_iter = util.partition(unsorted_configs, self._curryLM_acceptable(L, M))
                row.append(list(good_iter))
                unsorted_configs = rest_iter
            LM_table.append(row)
        return LM_table

    def _curryLM_acceptable(self, L, M):
        return lambda cfg: self.LM_acceptable(L, M, cfg)

    def LM_acceptable(self, L, M, cfg):
        """
            True if any configuration L-steps aways from cfg
             is M-acceptable.
        """
        reachable_cfgs = itertools.chain([cfg], config.next_iter(cfg, L))
        min_mean = min((self.stats_of_config(c)["mean"] for c in reachable_cfgs))
        return min_mean <= M * self.base_runtime
