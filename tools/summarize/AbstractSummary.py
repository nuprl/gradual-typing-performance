"""
    Common supertype for experiments.

    A summary object is built from a data source, like a table of results or a raw project folder.

    Subclasses must:
    - validate input (in the __init__ constructor)
    - generate experimental results from a configuration (in `results_of_config`)
    - implement `render`, to display results

"""

import constants
import numpy
import plot
import util

class AbstractSummary(object):
    """
    """
    ### Fields #################################################################
    graph           = None ## Module graph object representing the project
    module_names    = None ## (List String), names of the modules in the project
    num_iters       = 50   ## Number of iterations for run.rkt
    project_name    = None ## Title of the project
    stats_by_config = {}   ## Map: bitstring -> {mean, median, min, max, ci95}
    output_dir      = constants.OUTPUT_DIR  ## Default directory to save outputs

    ### Abstract Methods (subclasses must implement) ###########################
    # 1. Implement the __init__ constructor

    # 2. Generate experimental results from a configuration
    def results_of_config(self, config):
        """
            Generate & return experimental results for the configuration `config`.
        """
        raise NotImplementedError

    # 3. Convert a summary object to some output format
    def render(self, output_port, *args, **kwargs):
        """
            Convert this object to readable output.
            Subclasses should use the file extension
            of `output_location` to choose what type of output
            to produce (.txt, .tex, ...)
        """
        raise NotImplementedError

    ### Methods ################################################################

    def all_configurations(self):
        """
            Return a generator of all configuration bitstrings valid for this project
        """
        return ((self.bitstring_of_int(i)
                 for i in range(self.get_num_configurations())))

    def best_rows(self, pred, metric, limit=5):
        """
            Return the identfiers of the `limit` best configurations,
            according to the measure `metric`.
            Ignores configurations that do not satisfy `pred`.
        """
        cache = [None] * limit
        for cfg in self.all_configurations():
            if pred(cfg):
                util.sorted_buffer_insert(cache, cfg, metric, 0)
        return cache

    def bitstring_of_int(self, n):
        return bin(n)[2:].zfill(self.get_num_modules())

    ## Graphing

    def graph_absolute_runtimes(self, preds, xtitle, xlabels, output, title=None):
        """
            Plot the absolute running times of the configurations picked by
            each predicate in `preds`.
            (The number of columns in the result is equal to the size of `preds`)
        """
        # TODO filter empty columns, so the violin plot never fails
        columns = [self.stats_of_predicate(pred)["raw"]
                   for pred in preds]
        return plot.violin(columns
                           ,title or "%s absolute runtimes" % self.get_project_name()
                           ,xtitle
                           ,"Runtime (ms)"
                           #,positions=xlabels
                           ,output="%s/%s" % (self.output_dir, output))

    def graph_config(self, config, title=None, output=None):
        """
            Print the module graph of the configuration `config`.
            Color typed nodes and mark boundary edges.
            Return the name of the generated file.
        """
        if not self.graph:
            return None
        return plot.module_graph(self.graph
                                 ,self.project_name
                                 ,config
                                 ,title=title
                                 ,output="%s/%s" % (self.output_dir, output))

    def graph_conditional(self, pred, xtitle, xlabels, output, title=None):
        """
            Plot the absolute results conditioned on `pred`.
            Generates two distributions:
            - configurations where `pred` returns a positive number
            - configurations where `pred` returns a negative number
            (Configurations where `pred` returns zero are ignored)

            TODO generalize to +2 dimensions?
        """
        # TODO figure out what graph to make & how
        raise NotImplementedError

    def graph_normalized_runtimes(self, preds, output, base_index=0, title=None, xlabels=[]):
        """
            Plot the normalized running times of the configurations picked
            by each predicate in `preds`.

            Args:
            - preds : list of functions (-> config bool)
            Options:
            - base_index : Index of result to use as predicate (default 0)
            - title : Title of graph
            - output : Location to save graph to
            - xlabels : Labels for each column. Order matters! Must match `preds`
        """
        columns = [self.stats_of_predicate(pred)["mean"]
                   for pred in preds]
        base = columns[base_index]
        return plot.bar(range(len(columns))
                        ,[col / base for col in columns]
                        ,title or ("%s normalized runtimes" % self.get_project_name())
                        ,""
                        ,"Runtime (Normalized to %s-th column)" % base_index
                        ,xlabels=xlabels
                        ,output="%s/%s" % (self.output_dir, output))

    ## Getters

    def get_module_names(self):
        return self.module_names

    def get_num_configurations(self):
        return 2 ** len(self.module_names)

    def get_num_modules(self):
        """
            Return the number of modules this experiment summarizes.
        """
        return len(self.module_names)

    def get_num_iters(self):
        return self.num_iters

    def get_project_name(self):
        return self.project_name

    ## Compute experimental results

    def set_stats(self, cfg, stats):
        self.stats_by_config[cfg] = stats

    def stats_by_numtyped(self):
        """
            Return an array of results. The results at the i-th index are the results
            when `i` modules are typed.
        """
        return [self.stats_of_predicate(lambda cfg: n == util.num_typed(cfg))
                for n in range(self.num_modules)]

    def stats_of_config(self, config, recompute=False):
        """
            Return statistics from the experiment on configuration `config`.
            This is a low-level measure -- the results of one experiment.
        """
        if (config not in self.stats_by_config) or recompute:
            self.stats_by_config[config] = self.results_of_config(config)
        return self.stats_by_config[config]

    def stats_of_predicate(self, pred, recompute=False):
        """
            Return an array of the experimental results
            for each configuration matching `pred`.
        """
        unflattened = [self.stats_of_config(cfg, recompute)
                      for cfg in self.all_configurations()
                      if pred(cfg)]
        return util.stats_of_row([x for st in unflattened for x in st["raw"]])

    def stats_of_typed(self):
        return self.get_stats("1" * self.get_num_modules())

    def stats_of_untyped(self):
        return self.get_stats("0" * self.get_num_modules())
