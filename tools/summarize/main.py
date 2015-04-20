"""
Entry point to the summary script.
Accepts raw data as input
  (either a .tab file, or a raw .rktd file)
Collects varied statistics on the data and outputs results to a .tex file

TODO: accept project root folders as input, simulate results by sampling
"""

import os
import parser
import render
import sampling
import tabfile
import util

def main(*args, **options):
    """ (-> (Listof Any) (Dictof String Any) Void)
        Collect summary information from the input args by dispatching to the
        appropriate helper function.
        Pretty-print and save results.
    """
    results = None
    if len(args) == 2 and args[0].endswith(".rktd"):
        # Parse the .rktd file into a .tab file, parse the .tab file
        tabfile = tabfile.of_rktd(args[0])
        results = tabfile.main(tabfile, args[1])
    elif len(args) == 2 and args[0].endswith(".tab"):
        # Collect results from the .tab file
        results = tabfile.main(args[0], args[1])
    elif len(args) == 2 and os.path.isdir(args[0]):
        # Sampling mode!
        results = sampling.main(args[0], args[1])
    else:
        raise ValueError("unexpected arguments '%s'" % str(args))
    render.as_tex(results, "%s.tex" % util.strip_suffix(args[0]).rsplit("/", 1)[-1])
    return

def print_help():
    """ (-> Void)
        Print usage information
    """
    print("Usage: summarize.py FILE.tab")

### Entry point

if __name__ == "__main__":
    args, options = parser.parse_args(sys.argv[1::])
    if bool(args):
        main(*args, **options)
    else:
        print_help()

