""" grouping.py
Group data points by confidence interval & standard deviation.
"""
import util
import math
import statistics
import sys
import numpy as np
import matplotlib
matplotlib.use('Agg') # disables the display
import matplotlib.pyplot as plt

# Constants for confidence intervals.
# Z95 is the constant for 95% confidence
Z95 = 1.96

def stats_of_tab(fname):
    # Create a list of "statistics" objects, keyed by title from the spreadsheet
    tbl = []
    with open(fname, "r") as f:
        next(f) # Ignore first line, its just an index
        for line in f:
            cols = line.strip().split("\t")
            title = cols[0]
            # Collect statistics on each row
            data = [int(x) for x in cols[1::]]
            obj = {}
            obj["key"] = cols[0]
            obj["num-samples"] = len(data)
            obj["mean"] = statistics.mean(data)
            obj["median"] = statistics.median(data)
            obj["min"] = min(data)
            obj["max"] = max(data)
            obj["range"] = obj["max"] - obj["min"]
            obj["std"] = statistics.stdev(data)
            obj["variance"] = statistics.variance(data)
            ci_offset = (Z95 * obj["std"]) / (math.sqrt(obj["num-samples"]))
            obj["confidence-interval"] = [obj["mean"] - ci_offset,
                                          obj["mean"] + ci_offset]
            tbl.append(obj)
    return tbl

def make_bins(tbl, num_bins):
    # Cluster stat objects in `tbl` into `num_bins` buckets.
    # Evenly space the buckets across the interval
    max_runtime = max((obj["max"] for obj in tbl))
    min_runtime = min((obj["min"] for obj in tbl))
    rng = max_runtime - min_runtime
    chunk_width = rng / num_bins
    # Each bucket is a (min,max) tuple
    bins = [(i * chunk_width , (1 + i) * chunk_width)
            for i in range(0, num_bins)]
    # Create a bucket (interval) object for each bin.
    bkts = []
    for (lo, hi) in bins:
        b = {}
        b["min"] = lo
        b["max"] = hi
        b["data"] = [obj for obj in tbl if lo <= obj["mean"] < hi]
        b["mean"] = statistics.mean([obj["mean"] for obj in b["data"]])
        b["median"] = statistics.median([obj["median"] for obj in b["data"]])
        b["count"] = len(b["data"])
        bkts.append(b)
    return bkts, ("BINS-%d" % num_bins)

def every_point_in(fname):
    pts = []
    with open(fname, "r") as f:
        next(f)
        for line in f:
            for pt in line.strip().split("\t")[1::]:
                pts.append(int(pt))
    return pts

def count_lines(fname):
    count = -1
    with open(fname, "r") as f:
        for line in f:
            count += 1
    return count

def save_graph(data, fname, tag):
    # `data` are the groups (by ID)
    # `fname` is the raw data file
    # `tag` helps create a uniquely-named output file
    new_name = util.gen_name(fname, tag, "png")
    plt.xlabel("Avg. Runtime (milliseconds)")
    plt.ylabel("Num. Configs (percentage of total #configurations)")
    plt.title(new_name.rsplit(".", 1)[0].rsplit("/", 1)[-1])
    num_configs = count_lines(fname) - 1
    bin_width = data[0]["max"] - data[0]["min"]
    # Plot bins
    for bkt in data:
        # x-position is average runtime, height is count
        plt.bar(bkt["mean"],
                int(100 * (bkt["count"] / num_configs)),
                alpha=0.6, # opacity
                bottom=0,
                label="%s - %s" % (bkt["min"], bkt["max"]),
                width=bin_width)
    # Add median line (take median of alread-computed rows' medians)
    plt.axvline(statistics.median(bkt["median"] for bkt in data),
                color='r', linestyle="dashed", linewidth=5)
    plt.savefig(new_name)
    print("Saved plot to '%s'" % new_name)
    plt.clf()
    return new_name

def save_file(data, fname, tag):
    # Save data to a file, using fname and tag to create a hopefully-unique name.
    new_name = util.gen_name(fname, tag, "tab")
    with open(new_name, "w") as f:
        f.write("Avg.Runtime\tKeys\n")
        for bkt in data:
            f.write(str(int(bkt["mean"])))
            f.write("\t")
            f.write("\t".join([obj["key"] for obj in bkt["data"]]))
            f.write("\n")
    print("Saved file to '%s'" % new_name)
    return new_name

def save(data, fname, tag):
    save_graph(data, fname, tag)
    return save_file(data, fname, tag)

def main(fname):
    # Try grouping by CI and std-dev. Print results and save actual numbers to a file.
    tbl = stats_of_tab(fname)
    print("Starting with %d points\n" % len(tbl))
    results = [
        make_bins(tbl, 5),
        # make_bins(tbl, 10),
        # make_bins(tbl, 15),
    ]
    x = [save(g, fname, tag) for (g, tag) in results]
    print("All done.")

if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        print("Usage: grouping.py TAB-FILE")
