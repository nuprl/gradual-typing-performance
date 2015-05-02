#! /usr/bin/env python3
"""
Entry point to the summary script.
Accepts raw data as input
  (either a .tab file, or a raw .rktd file)
Collects varied statistics on the data and outputs results to a .tex file

"""

import constants
import os
import sys
import util
from TabfileSummary import TabfileSummary
# from SrsSummary import SrsSummary

def init():
   """
       Pre-processing step.
   """
   if not os.path.exists(constants.OUTPUT_DIR):
       os.mkdir(constants.OUTPUT_DIR)
   return

def main(input_file):
    """ (-> (Listof Any) (Dictof String Any) Void)
        Collect summary information from the input args by dispatching to the
        appropriate helper function.
        Pretty-print and save results.
    """
    summary = None
    init()
    if input_file.endswith(".rktd") or input_file.endswith(".tab"):
       summary = TabfileSummary(input_file)
    # elif os.path.isdir(input_file):
    #     # Sampling mode!
    #     summary = SrsSummary(input_file)
    else:
        raise ValueError("unexpected arguments '%s'" % str(args))
    out_file = "%s/%s.tex" % (constants.OUTPUT_DIR, util.strip_suffix(input_file).rsplit("/", 1)[-1])
    out_port = open(out_file, "w")
    summary.render(out_port)
    out_port.close()
    print("Results saved as '%s'" % out_file)
    return

def print_help():
    """ (-> Void)
        Print usage information
    """
    print("Usage: ./summary FILE.rktd")

### Entry point

if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        print_help()

