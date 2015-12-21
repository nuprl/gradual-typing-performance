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
import plot
import util
from AggregateSummary import AggregateSummary
from LmnSummary import LmnSummary
from TabfileSummary import TabfileSummary
from SrsSummary import SrsSummary

import latex

def init(out_dir):
   """
       Pre-processing step.
   """
   if not os.path.exists(out_dir):
       os.mkdir(out_dir)
   return

def main(input_file, sample_size=50, iters=50, verbose=0, default_out=None, default_port=None):
    """ (-> Path-String (Dictof String Any) Void)
        Collect summary information from the input args by dispatching to the
        appropriate helper function.
        Pretty-print and save results.
    """
    print("Processing '%s'" % input_file)
    summary = None
    tag = util.strip_suffix(input_file).rsplit("/", 1)[-1]
    out_dir = default_out or "%s-%s" % (constants.OUTPUT_DIR, tag)
    #init(out_dir)
    if input_file.endswith(".rktd") or input_file.endswith(".tab"):
        # summary = TabfileSummary(input_file)
        summary = LmnSummary(input_file, out_dir=out_dir)
    elif os.path.isdir(input_file):
        # Sampling mode!
        summary = SrsSummary(input_file, sample_size=sample_size, num_iters=iters)
    else:
        print("Cannot read input file '%s'" % input_file)
        return
    print("Rendering output for '%s'" % summary.project_name)
    out_file = "%s/%s.tex" % (out_dir, tag)
    out_port = default_port or open(out_file, "w")
    summary.render(out_port)
    if default_port is None:
        out_port.close()
    # print("Results saved as '%s'" % out_file)
    return summary

def aggregate(summaries):
    """
        Plot all summaries
    """
    agg = AggregateSummary(summaries)
    out_file = "aggregate.tex"
    out_port = open(out_file, "w")
    agg.render(out_port)
    out_port.close()
    print("Aggregate results saved as '%s'" % out_file)
    return agg

def print_help():
    """ (-> Void)
        Print usage information
    """
    print("Usage: ./summary FILE.rktd")

### Entry point, accepts a list of files.

if __name__ == "__main__":
    if len(sys.argv) < 2:
       print_help()
    elif sys.argv[1] in ["-a", "--all", "--aggregate"]:
       main_aggregate(sys.argv[2::])
    else:
       init(constants.OUTPUT_DIR)
       results = "%s/results.tex" % constants.OUTPUT_DIR
       output_port = open(results, "w")
       print(latex.PREAMBLE, file=output_port)
       print("\\begin{figure}", file=output_port)
       print(latex.L_HEADER, file=output_port)
       summs = []
       for fname in sys.argv[1::]:
          print("\\hbox{", file=output_port)
          summs.append(main(fname, default_out=constants.OUTPUT_DIR, default_port=output_port))
          print("}", file=output_port)
       print("\\caption{\\emph{L-step, M/N-usable} results for selected benchmarks.}", file=output_port)
       print("\\end{figure}", file=output_port)
       print(latex.end(), file=output_port)
       output_port.close()
       ## BEGIN HACKS
       os.system("cd output-summary; xelatex results.tex; cd ..;")
       os.system("cp output-summary/results.pdf /home/ben/Downloads/results.pdf")
       ## END HACKS

