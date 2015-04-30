"""
Simulate 'ground truth' results by sampling.

Under construction.
"""

import constants
import os
import re
import shell
import statistics

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
    raise NotImplementedError

def setup_variations(dirname):
    """
        Create all untyped/typed variations for files
        in the experiment directory `dirname`.
        Clobber existing variations folder, if it exists.
    """
    if not os.path.exists(SETUP):
        print("Cannot find '%s' script. Shutting down..." % SETUP)
        sys.exit(1)
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
        print("Cannot find '%s' script. Shutting down." % RUN)
        sys.exit(1)
    shell.execute("racket %s -i %s -o %s -x %s -e %s %s" \
      % (RUN, iters, OUTPUT, config, entry_point base_folder))
    return parse_rkt_results()

def main(dirname, graph):
    """
        Compute statistics for the experiment folder `dirname`
        by sampling.
    """
    check_directory_structure(dirname)
    setup_variations(dirname)
    results = {}
    for cfg in simple_random_sampling():
        results[cfg] = run_config(dirname, cfg)
    return results

