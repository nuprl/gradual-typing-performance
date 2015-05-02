"""
Simulate 'ground truth' results by sampling.
"""

import constants
import config
import glob
import os
import re
import shell
import statistics
import latex
import util
from AbstractSummary import AbstractSummary
from ModuleGraph import ModuleGraph

class SrsSummary(AbstractSummary):
    ### Fields ################################################################
    setup_script = "setup.rkt"
    run_script   = "run.rkt"
    tmp_output   = "sampling_output.rktd"

    ###

    def __init__(self, fname):
        ### Find external scripts
        self.setup_script = shell.find_file(self.setup_script)
        if not self.setup_script:
            raise ValueError("Could not find setup script within current directory, goodbye.")
        self.run_script   = shell.find_file(self.run_script)
        if not self.run_script:
            raise ValueError("Could not find run script within current directory, goodbye.")
        ### Check assumptions
        self.check_directory_structure(fname)
        self.setup_variations(fname)
        ### Setup fields
        self.project_name = fname
        self.graph        = ModuleGraph(fname)
        self.module_names = glob.glob("%s/untyped/*.rkt" % self.project_name)

    def results_of_config(self, config):
        # Execute the config for a pre-set number of iterations
        print("Running configuration '%s'" % config)
        return self.run_config(config)

    def render(self, output_port):
        self.render_title(output_port, "Simple Random Sampling: %s" % self.project_name)
        self.render_summary(output_port)
        self.render_overall(output_port
                           ,("untyped", config.is_untyped)
                           ,("typed", config.is_typed))
        self.render_absolute(output_port
                            ,*[(str(n), self.random_sample(num_modules=n))
                              for n in range(self.get_num_modules())])
        print(latex.end(), file=output_port)

    def render_sample_results(self, output_port):
        print(latex.subsection("Notes"), file=output_port)
        num_sampled = sum((1 for v in self.stats_by_config.values() if v))
        print("Sampled %s of %s configurations." % (num_sampled, self.get_num_configurations()))
        print("Took %s measurements for each sample" % self.get_num_measurements())

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

    def get_num_measurements(self):
        """
            Return the number of measurements taken for each sampled
            configuration.
            Fail if the number is not the same across all configurations.
        """
        meas_set = set((len(v) for v in self.stats_by_config.values()))
        if len(meas_set) == 0:
            raise ValueError("Cannot give number of measurements: No measurements recorded")
        if len(meas_set) > 1:
            raise ValueError("Detected unequal sample sizes in set '%s'" % meas_set)
        return meas_set[0]

    def setup_variations(self, dirname):
        """
            Create all untyped/typed variations for files
            in the experiment directory `dirname`.
            Clobber existing variations folder, if it exists.
        """
        return shell.execute("racket %s %s" % (self.setup_script, dirname))

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
        shell.execute(" ".join(["racket" , self.run_script
                                ,"-i", str(self.num_iters) ## -i : Number of iterations
                                ,"-o", self.tmp_output ## -o : Location to save output
                                ,"-x", config      ## -x : Exact configuration to run
                                ,"-e", entry_point ## -e : Main file to execute
                                ,self.project_name]))
        return self.parse_rkt_results()

