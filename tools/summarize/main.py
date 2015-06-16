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

def main(input_file, sample_size=50, iters=50, verbose=0):
    """ (-> Path-String (Dictof String Any) Void)
        Collect summary information from the input args by dispatching to the
        appropriate helper function.
        Pretty-print and save results.
    """
    print("Processing '%s'" % input_file)
    summary = None
    tag = util.strip_suffix(input_file).rsplit("/", 1)[-1]
    out_dir = "%s-%s" % (constants.OUTPUT_DIR, tag)
    init(out_dir)
    if input_file.endswith(".rktd") or input_file.endswith(".tab"):
        # summary = TabfileSummary(input_file)
        summary = LmnSummary(input_file, out_dir=out_dir)
    elif os.path.isdir(input_file):
        # Sampling mode!
        summary = SrsSummary(input_file, sample_size=sample_size, num_iters=iters)
    else:
        print("Cannot read input file '%s'" % input_file)
        return
    out_file = "%s/%s.tex" % (out_dir, tag)
    out_port = open(out_file, "w")
    summary.render(out_port)
    out_port.close()
    print("Results saved as '%s'" % out_file)
    ## BEGIN HACKS
    cwd = os.getcwd()
    os.system("cd %s; xelatex %s; cd %s" % (out_dir, tag, cwd))
    os.system("cp %s/%s.pdf /home/ben/Downloads/%s.pdf" % (out_dir, tag, tag.split("-", 1)[0]))
    ## END HACKS
    return summary

def aggregate(summaries):
    """
        Plot all summaries
    """
    percents = [[len(row)/s.num_configs for row in s.make_Nmap()]
             for s in summaries]
    maxlen = max([len(x) for x in percents])
    res = plot.dots(range(0, maxlen)
                   ,[util.pad(p, 1, maxlen) for p in percents]
                   ,"Percent acceptable vs N, all graphs"
                   ,"N"
                   ,"Percent acceptable"
                   ,output="aggregate.png"
                   ,labels=[x.project_name for x in summaries]
                   ,vlines = [{"xpos" : constants.DELIVERABLE
                              ,"color" : "r"
                              ,"style" : "solid"
                              ,"width" : 1
                             }])

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
       summs = []
       for fname in sys.argv[1::]:
          summs.append(main(fname))
       # aggregate(summs)

