"""
Simulate 'ground truth' results by sampling.

Under construction.
"""

import constants
import os
import statistics

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

        TODO: just use existing setup script
    """
    raise NotImplementedError

def run_file(fname, iters=50, drop=3):
    """
        Execute `fname` many times, collecting results into a temporary file
        and finally returning the average of all results.
        Use `drop` to set the number of warm-up trials.

        Expects that running `fname` causes Racket's time output to print to STDOUT.
    """
    if not os.path.exists(fname):
        raise ValueError("Cannot run '%s'. File not found." % fname)
    dname, main = fname.rsplit("/", 1)
    cwd = os.getcwd()
    os.chdir(dname)
    with open(constants.TMP_RESULTS, "w") as f:
        f.write("")
    make_cmd = "raco make %s" % main
    drop_cmd = "for i in {1..%s}; do racket %s; done" % (drop, main)
    run_cmd  = "for i in {1..%s}; do racket %s >> %s; done" % (iters, main, constants.TMP_RESULTS)
    os.system("; ".join([make_cmd, drop_cmd, run_cmd]))
    times = []
    with open(constants.TMP_RESULTS, "r") as f:
        for line in f:
            times.append(int(re.match(r'^cpu: ([0-9]+) .*', line).group(1)))
    return statistics.mean(times)

def run_config(base_folder, config, entry_point="main.rkt", iters=50, drop=3):
    """
        Sample the configuration `config`
    """
    fname = "%s/variation%s/%s" % (base_folder, config, entry_point)
    return sample_file(fname, iters=iters, drop=drop)


def main(dirname, graph):
    """
        Compute statistics for the experiment folder `dirname`
        by sampling.
    """
    check_directory_structure(dirname)
    setup_variations(dirname)
    raise NotImplementedError

