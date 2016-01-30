"""
    Simulate results by random sampling.
    A sample is a subset of configurations matching some predicate.
    For example, we sample and plot all configurations with 3 typed modules.
    - The predicate is "has 3 typed modules"
    - The sample is a subset of matching configurations

    These samples give a reasonable estimate of the population's true
    mean and variance.
    - The mean is less useful, but gives an idea what performance cost to expect
    - The variance describes the range of performance costs at a given level.

    TODO let's add some more measurements and samples,
    like conditional distributions to help find bottlenecks
"""

import constants
import config
import glob
import math
import os
import random
import shell
import statistics
import latex
import util
from AbstractSummary import AbstractSummary
from ModuleGraph import ModuleGraph
from statsmodels.stats.stattools import jarque_bera

class SrsSummary(AbstractSummary):
    ### Fields ################################################################
    sample_size  = 50
    #setup_script = "setup.rkt"
    run_script   = "run.rkt"
    tmp_output   = "sampling_output.rktd"

    ###

    def __init__(self, fname, *args, **kwargs):
        ### Find external scripts
        #self.setup_script = shell.find_file(self.setup_script)
        #if not self.setup_script:
        #    raise ValueError("Could not find setup script within current directory, goodbye.")
        self.run_script   = shell.find_file(self.run_script)
        if not self.run_script:
            raise ValueError("Could not find run script within current directory, goodbye.")
        ### Check assumptions
        self.check_directory_structure(fname)
        #self.setup_variations(fname)
        ### Setup fields
        self.project_name = fname
        self.graph        = ModuleGraph(fname)
        self.module_names = glob.glob("%s/untyped/*.rkt" % self.project_name)
        self.num_iters    = kwargs.get("num_iters", self.num_iters)
        self.sample_size  = kwargs.get("sample_size", self.sample_size)
        self.strategy     = constants.APPEND # or, recompute?

    def results_of_config(self, config):
        # Execute the config for a pre-set number of iterations
        return util.stats_of_row(self.run_config(config))

    def render(self, output_port):
        self.render_title(output_port, "Simple Random Sampling: %s" % self.project_name)
        self.render_summary(output_port)
        self.render_overall(output_port
                           ,("untyped", config.is_untyped)
                           ,("typed", config.is_typed))
        self.render_absolute(output_port
                            ,*[(str(n), self.in_random_sample(num_typed=n))
                              for n in range(self.get_num_modules())])
        self.render_sample_results(output_port)
        self.strategy = constants.CACHE
        # self.render_all_paths(output_port, [1,2,3,4])
        print(latex.end(), file=output_port)

    def render_sample_results(self, output_port):
        print(latex.subsection("Notes"), file=output_port)
        num_sampled = sum((1 for v in self.stats_by_config.values() if v))
        print("Sampled %s of %s configurations." % (num_sampled, self.get_num_configurations()), file=output_port)
        print(latex.table(["\# Typed", "\# Configs", "\# Samples", "Sample Mean", "Sample Variance", "95\% CI", "Standard Error", "Jarque-Bera"]
                         ,[self.sample_stats((lambda cfg, nt=n: config.num_typed_modules(cfg) == n), n) for n in range(self.get_num_modules())]), file=output_port)

    def render_all_paths(self, output_port, transitivity=[1]):
        print(latex.subsection("Sampling paths"), file=output_port)
        untyped_cfg = "0" * self.get_num_modules()
        for trans in transitivity:
            weights = [self.max_runtime_over_typed(config.random_walk(untyped_cfg, trans))
                       for _ in range(self.sample_size)]
            print(latex.figure(self.graph_histogram(weights
                               ,"%s-sample-paths-trans-%s.png" % (self.project_name, trans)
                               ,"Sampled Paths in a %s-trans lattice" % trans
                               ,"Max Overhead (runtime / typed runtime)")), file=output_port)

    def max_runtime_over_typed(self, path):
        """
            TODO share this with TabfileSummary, or something
        """
        ty_cfg = "1" * self.get_num_modules()
        return max((self.stats_of_config(cfg)["mean"] / self.stats_of_config(ty_cfg)["mean"]
                    for cfg in path))

    def sample_stats(self, pred, tag):
        """
            Get statistics about the samples matching predicate `pred`
        """
        all_samples = [(k,v['raw']) for (k,v) in self.stats_by_config.items() if pred(k)]
        configs = [k for (k,_) in all_samples]
        vals    = [v for (k,vs) in all_samples for v in vs]
        stat = util.stats_of_row(vals)
        return [tag
               ,len(set(configs))
               ,len(vals)
               ,round(stat["mean"], 2)
               ,round(stat["variance"], 2)
               ,"%s~\\textendash~%s" % (round(stat["ci"][0], 2), round(stat["ci"][1], 2))
               ,round(math.sqrt(stat["variance"]) / math.sqrt(len(vals)), 2)
               ,round(jarque_bera(vals)[0], 2)]

    ### Helpers

    def check_directory_structure(self, dirname):
        """
            Assert that directory `dirname` is a properly-formatted
            experiment directory.
            Should have:
            - a folder `typed/`
            - a folder `untyped/`
            - a folder `base/`
            - (optional) a folder `both/`
            Constraints:
            - the `typed/` and `untyped/` folders should have the same
              numbers of files, and matching filenames.
        """
        # Ensure directories exist
        base_dir = "%s/base" % dirname
        un_dir   = "%s/base" % dirname
        ty_dir   = "%s/base" % dirname
        req_dirs = [base_dir, un_dir, ty_dir]
        for required_dir in req_dirs:
            if not os.path.isdir(required_dir):
                raise ValueError("Required directory '%s' not found. Please create it." % required_dir)
        both_dir = "%s/both"
        if os.path.isdir(both_dir):
            req_dirs.append(both_dir)
        # Ensure no nested directories
        for d in req_dirs:
            if util.contains_any_directories(d):
                raise ValueError("Directory '%s' should not contain any sub-directories, but it does. Please remove." % d)
        # Ensure num. typed and num. untyped files are the same
        if not(util.count_files(ty_dir) == util.count_files(un_dir)):
            raise ValueError("Directories '%s' and '%s' must have the same number of files." % (un_dir, ty_dir))
        # Ensure filenames in typed/untyped are the same
        if not (sorted((util.get_files(un_dir))) == sorted((util.get_files(ty_dir)))):
            raise ValueError("The filenames in '%s' must match the filenames in '%s', but do not. Please fix." % (un_dir, ty_dir))
        return

    #def get_num_measurements(self):
    #    """
    #        Return the number of measurements taken for each sampled
    #        configuration.
    #        Fail if the number is not the same across all configurations.
    #    """
    #    meas_set = set((len(v['raw']) for v in self.stats_by_config.values()))
    #    if len(meas_set) == 0:
    #        raise ValueError("Cannot give number of measurements: No measurements recorded")
    #    if len(meas_set) > 1:
    #        raise ValueError("Detected unequal sample sizes in set '%s'" % meas_set)
    #    return meas_set.pop()

    def in_random_sample(self, *args, **kwargs):
        sample = self.random_sample(*args, **kwargs)
        return (lambda cfg: cfg in sample)

    def random_sample(self, num_typed=None):
        matches = [cfg for cfg in self.all_configurations()
                   if config.num_typed_modules(cfg) == num_typed]
        # Randomly select `sample_size` items from the population of `matches`.
        return [matches[random.randint(0, len(matches)-1)]
                for _ in range(self.sample_size)]

    #def setup_variations(self, dirname):
    #    """
    #        Create all untyped/typed variations for files
    #        in the experiment directory `dirname`.
    #        Clobber existing variations folder, if it exists.
    #    """
    #    return shell.execute("racket %s %s" % (self.setup_script, dirname))

    def parse_rkt_results(self):
        """
            Parse experiment results from a .rktd file containing a
            (Vector (Vectorof Int))
        """
        raw = None
        with open(self.tmp_output, "r") as f:
            raw = next(f)
        if raw.startswith("#((") and raw.endswith("))"):
            return [int(x) for x in raw[3:-2].split(" ")]
        raise ValueError("Could not parse results from %s. Results were:\n%s" % (self.tmp_output, raw))

    def run_config(self, config, entry_point="main.rkt"):
        """
            Sample the configuration `config`, return a list of
            observed runtimes.
        """
        self.setup_config(config)
        print("Running config '%s' for %s iterations"% (config, self.num_iters))
        shell.execute(" ".join(["racket" , self.run_script
                                ,"-i", str(self.num_iters) ## -i : Number of iterations
                                ,"-o", self.tmp_output ## -o : Location to save output
                                ,"-x", config      ## -x : Exact configuration to run
                                ,"-e", entry_point ## -e : Main file to execute
                                ,self.project_name]))
        return self.parse_rkt_results()

    def setup_config(self, config):
        """
            Assume `project_name` is a directory in the current folder
            (This is justified by __init__)
            Check that a benchmark/ folder exists with the right variation.
            If not, create it.
        """
        benchmark_dir = "%s/benchmark" % self.project_name
        variation_dir = "%s/variation%s" % (benchmark_dir, config)
        both_dir      = "%s/both" % self.project_name
        ty_dir      = "%s/typed" % self.project_name
        un_dir      = "%s/untyped" % self.project_name
        if not os.path.exists(benchmark_dir):
            print("INFO: creating directory '%s'" % benchmark_dir)
            os.mkdir(benchmark_dir)
        if not os.path.exists(variation_dir):
            print("INFO: creating and filling directory '%s'" % variation_dir)
            os.mkdir(variation_dir)
            if os.path.exists(both_dir):
                shell.execute("cp %s/* %s" % (both_dir, variation_dir))
            for i in range(len(config)):
                char = config[i]
                fname = self.module_names[i].rsplit("/", 1)[-1]
                home = ty_dir if char == "1" else un_dir
                shell.execute("cp %s/%s %s" % (home, fname, variation_dir))
        return

