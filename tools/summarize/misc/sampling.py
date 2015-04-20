"""
sampling.py

Simple random sampling
- Pick a small number N
- For each population in the dataset (i.e., num typed modules)
  sample N configurations (or runs, haven't decided yet)
  (This is not quite 'statified sampling' because we don't combine the results)
- Use the t-test to extrapolate population mean & variance
- Graph results
"""

### require

import math
import sys
import util
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle
import numpy as np
import statistics

### constants

MAX_SAMPLES = 30
MIN_MODULES = 10
# Student's t statistics for two-sided confidence intervals. From Wikipedia
T_TABLE = {
  95 :
    { 9 : 2.262
    ,10 : 2.228
    ,11 : 2.201
    ,12 : 2.179
    ,13 : 2.160
    ,14 : 2.145
    ,15 : 2.131
    ,16 : 2.120
    ,17 : 2.110
    ,18 : 2.101
    ,19 : 2.093
    ,20 : 2.086
    ,21 : 2.080
    ,22 : 2.074
    ,23 : 2.069
    ,24 : 2.064
    ,25 : 2.060
    ,26 : 2.056
    ,27 : 2.052
    ,28 : 2.048
    ,29 : 2.045
    ,30 : 2.042}
  }

### core functions

def run_config(config, fname):
    # "Run" the configuration `config`
    # i.e., look up the cached average using fname
    result = None
    with open(fname, "r") as f:
        next(f)
        for line in f:
            rows = line.split("\t")
            if rows[0] == config:
                # Success!
                result = statistics.mean((int(x) for x in rows[1::]))
                break
    return result

# Data definition: STAT
# A STAT object is a dictionary with two fields
# - mean : Nat
# - ci   : (List Nat Nat)

# (-> Path-String Nat Nat (Listof (Listof STAT)))
def sample_by_num_typed_modules(fname, num_modules, num_samples):
    # Partition the data in `fname` by number of typed modules
    # Take `num_samples` samples from each partition, return inferences
    results = []
    # t_stat for 95% confidence with num_modules-1 degrees of freedom
    t_stat = T_TABLE[95][num_modules-1]
    # Used for confidence intervals
    sqrt_nm = math.sqrt(num_modules)
    for N in range(1+num_modules):
        population = (["1"] * N) + (["0"] * (num_modules - N))
        sample_results = []
        # Take samples
        for _ in range(num_samples):
            config = "".join(np.random.permutation(population))
            sample_results.append(run_config(config, fname))
        # Extrapolate from results
        sample_mean = statistics.mean(sample_results)
        sample_std = statistics.stdev(sample_results)
        delta = t_stat * (sample_std / sqrt_nm)
        sample_ci = [sample_mean - delta
                    ,sample_mean + delta]
        results.append({"mean" : sample_mean
                       ,"ci" : sample_ci})
    return results

### main function / make graph

def main(fname, num_modules):
    num_samples = min(num_modules, MAX_SAMPLES)
    data = sample_by_num_typed_modules(fname, num_modules, num_samples)
    fig,ax1 = plt.subplots()
    #ax1.set_ylim(0, 18000) #funkytown
    ax1.set_ylim(400, 900) # tetris
    ## Graph data. For each number of modules, graph the mean & ci.
    for i in range(len(data)):
        ci_rect = Rectangle([i+1-0.25, data[i]['ci'][0]], 0.5, data[i]['ci'][1] - data[i]['ci'][0], facecolor='royalblue')
        ax1.add_patch(ci_rect)
        plt.plot(i+1, [data[i]['mean']], color='w', marker='*', markeredgecolor='k')
    ## Save graph
    plt.xticks(range(1,1+len(data)), range(0, len(data)))
    #plt.yticks([5000,10000,15000]) #funkytown
    plt.yticks([100 * i for i in range(4,10)])
    ax1.yaxis.grid(True, linestyle='-', which='major', color='lightgrey', alpha=0.5)
    ax1.set_axisbelow(True)
    ax1.set_title(fname.rsplit("/",1)[-1].rsplit(".")[0])
    ax1.set_xlabel("Num. Typed Modules")
    ax1.set_ylabel("Runtime (ms)")
    new_name = util.gen_name(fname, "sampling-%s" % num_samples, "png")
    plt.savefig(new_name)
    plt.clf()
    print("Saved results to '%s'" % new_name)
    return

if __name__ == "__main__":
    if len(sys.argv) == 2 and sys.argv[1].endswith(".tab"):
        fname = sys.argv[1]
        num_mods = util.count_modules(fname)
        if num_mods >= MIN_MODULES:
            main(fname, num_mods)
        else:
            print("Error: too few modules to sample. Need at least %s" % MIN_MODULES)
    else:
        print("Usage: sampling.py FILE.tab")
