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

    def render(self, output_port):
        """
            Print experimental L-M-N graphs,
            save everything to a .tex file for easy reading.
        """
        print("\\begin{subfigure}{\\textwidth}", file=output_port)
        print("\\hbox{", file=output_port)
        self.render_lmn(output_port
                        ,Nmax=self.Nmax
                        ,Mmax=self.Mmax
                        ,Lvals=self.Lvals)
        print("}", file=output_port)
        print("\\caption{%s}" % self.quick_stats(), file=output_port)
        print("\\end{subfigure}", file=output_port)

    def quick_stats(self):
        gt_stat = self.stats_of_predicate(config.is_gradual)
        return " ".join(["\\textbf{%s}" % self.project_name
                        ,":"
                        ,"%s modules," % self.num_modules
                        ,"~~"
                        ,"max. overhead %s{\\tt $\\times$}," % round(gt_stat["max"] / self.base_runtime, 2)
                        ,"~~"
                        ,"average overhead %s{\\tt $\\times$}" % round(gt_stat["mean"] / self.base_runtime, 2)
                        ])

    def render_lmn(self, output_port, Nmax=None, Mmax=None, Lvals=None):
        figs = []
        for L in Lvals:
            figs.append(plot3.contour([0, Nmax]
                                 ,[0, Mmax]
                                 ,self.countLNM_continuous(L)
                                 ,title="L=%s %s" % (L, "steps" if L==0 else "")
                                 ,xlabel="\n\nN (overhead factor)"
                                 ,ylabel="\n\nM (≥ N)"
                                 ,zlabel="Count"
                                 ,samples=self.num_samples
                                 ,output="%s/%s" % (self.output_dir, "%s-lmn-%sstep" % (self.project_name, L))
                                 ,zlim=self.num_configs))
        print("\n\\hfill{}".join([latex.figure(fg) for fg in figs]), file=output_port)

    ### -----------------------------------------------------------------------------

    def countLNM_continuous(self, L):
        """
            Return a function to compute counts for a contour plot,
             the counts are modulo a path length L.
            Function uses `np.nan` as a placeholder for undefined values.
            These are masked in the result
        """
        return (lambda N,M: np.nan if (N > M) else self.countLM_continuous(L, M))

    def countLM_continuous(self, L, M_float):
        """
            Return the number of configurations that can reach in
             at most L steps, a config with runtime less than (M * untyped).
            M can be a float.
        """
        M_lo = math.floor(M_float)
        M_hi = math.ceil(M_float)
        cached = self.configs_within_overhead[L]
        # Easy part: all configs in a lower-overhead bucket are OK
        total = sum((len(configs) for configs in cached[:M_lo+1]))
        if M_lo != M_hi:
            # Harder part: check all configs in the next bucket,
            # see if they have acceptable overhead
            maybe_ok_cfgs = cached[M_hi]
            max_mean = M_float * self.base_runtime
            extras = sum((1 for cfg in maybe_ok_cfgs
                          if self.stats_of_config(cfg)["mean"] <= max_mean))
            total += extras
        return total

    def _precompute_counts(self):
        """
            Precompute mappings from (L/M) to (L/M-good configs)
             for discrete L and M.

            Returns:
             A table TBL such that TBL[L][M] returns the list of configurations
              that are L-M-(M-1) acceptable.
             i.e. you get the _names_ of all configurations worse than (M-1)
              but still within M times untyped.

            Running time:
            - L passes over all configurations, so L*(2**N) where N = num modules
            Space:
            - stores at most all configurations in each row of the table,
              L * (2**N)
        """
        LM_table = []
        for L in self.Lvals:
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
