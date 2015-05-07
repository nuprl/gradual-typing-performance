"""
Simulate 'ground truth' results by sampling.

Under construction.
"""

import constants
import os
import re
import shell
import statistics
import util

### Constants
# Location of setup script
SETUP = "setup.rkt"
# Location of run script
RUN = "tools/run.rkt"
# File to save temporary results to
OUTPUT = "sampling_output.rktd"

def check_directory_structure(dirname):
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

def setup_variations(dirname):
    """
        Create all untyped/typed variations for files
        in the experiment directory `dirname`.
        Clobber existing variations folder, if it exists.
    """
    if not os.path.exists(SETUP):
        raise ValueError("Cannot find '%s' script. Shutting down..." % SETUP)
    return shell.execute("racket %s %s" % (SETUP, dirname))

def simple_random_sampling(TODO):
    """
        Return a list of variations to sample
    """
    raise NotImplementedError

def parse_rkt_results():
    """
        Parse experiment results from a .rktd file containing a
        (Vector (Vectorof Int))
    """
    raw = None
    with open(OUTPUT, "r") as f:
        raw = next(f)
    if raw.startswith("#((") and raw.endswith("))"):
        return [int(x) for x in raw[3:-2].split(" ")]
    raise ValueError("Could not parse results from %s. Results were:\n%s" % (OUTPUT, raw))

def run_config(base_folder, config, entry_point="main.rkt", iters=50):
    """
        Sample the configuration `config`, return a list of
        observed runtimes.
    """
    if not os.path.exists(RUN):
        raise ValueError("Cannot find '%s' script. Shutting down." % RUN)
    shell.execute(" ".join(["racket" ,RUN
                            ,"-i", iters       ## -i : Number of iterations
                            ,"-o", OUTPUT      ## -o : Location to save output
                            ,"-x", config      ## -x : Exact configuration to run
                            ,"-e", entry_point ## -e : Main file to execute
                            ,base_folder]))
    return parse_rkt_results()

def main(dirname, graph):
    """
        Compute statistics for the experiment folder `dirname`
        by sampling.
    """
    print("Sampling '%s'" % dirname)
    check_directory_structure(dirname)
    raise NotImplementedError
    setup_variations(dirname)
    results = {}
    for cfg in simple_random_sampling():
        results[cfg] = run_config(dirname, cfg)
    return results

