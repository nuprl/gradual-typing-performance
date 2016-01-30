""" grouping.py
Group data points by confidence interval & standard deviation.

TODO add 'thermometer' picture that projects counts as 'hot' colors, instead of 2-d histogram?
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

# Data definition: StatTable
#   A StatTable summarizes a row from the input spreadsheet.
# fields:
# - key : first column from the sheet
# - num-samples : num. columns - 1
# - mean, median, min, max, range, std, variance, confidence-interval : computed from column values
# Implemented as a Python dictionary

# Data definition: BinObject
#   A BinObject represents data grouped together
# fields:
# - count : number of items in bin
# - data : raw data from the bin
# - min, max, median : computed stats

# (-> Path-String StatTable)
def stats_of_tab(fname):
    # Create a list of "statistics" objects, keyed by title from the spreadsheet
    tbl = []
    with open(fname, "r") as f:
        next(f) # Ignore first line, its just an index
        for line in f:
            cols = line.strip().split()
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

# (-> StatTable Nat (Listof BinObject))
def make_bins(tbl, num_bins):
    # Cluster stat objects in `tbl` into `num_bins` buckets.
    # Evenly space the buckets across the interval
    max_runtime = max((obj["max"] for obj in tbl))
    min_runtime = min((obj["min"] for obj in tbl))
    rng = max_runtime - min_runtime
    chunk_width = rng / num_bins
    # Each bucket is a (min,max) tuple
    bins = [(min_runtime + int(i * chunk_width) , min_runtime + int((1 + i) * chunk_width))
            for i in range(0, num_bins)]
    # Create a bucket (interval) object for each bin.
    bkts = []
    for (lo, hi) in bins:
        b = {}
        b["min"] = lo
        b["max"] = hi
        b["data"] = [obj for obj in tbl if lo <= obj["mean"] < hi]
        b["mean"] = statistics.mean([obj["mean"] for obj in b["data"]]) if b["data"] else None
        b["median"] = statistics.median([obj["median"] for obj in b["data"]]) if b["data"] else None
        b["count"] = len(b["data"])
        bkts.append(b)
    return bkts, ("BINS-%d" % num_bins)

# (-> Path-String (Listof Nat))
def every_point_in(fname):
    # Collect every single data point in the spreadsheet `fname` as a list of ints
    pts = []
    with open(fname, "r") as f:
        next(f)
        for line in f:
            for pt in line.strip().split()[1::]:
                pts.append(int(pt))
    return pts

# (-> Path-String String (Listof Nat))
def get_row(fname, key):
    # Return the integers in the row keyed by `key`
    # i.e., ignore the first column and parse the rest as Python ints
    with open(fname, "r") as f:
        next(f)
        for line in f:
            data = line.strip().split()
            if data[0] == key:
                return [int(x) for x in data[1::]]
    raise ValueError("could not find key '%s'" % key)

# (-> (Listof BinObj) Path-String String String)
def save_graph(data, fname, tag):
    # `data` are the groups (by ID)
    # `fname` is the raw data file
    # `tag` helps create a uniquely-named output file
    new_name = util.gen_name(fname, tag, "png")
    plt.xlabel("Avg. Runtime (ms)", )
    plt.ylabel("Num. Configs")
    plt.title(new_name.rsplit(".", 1)[0].rsplit("/", 1)[-1])
    num_configs = util.count_lines(fname) - 1
    bin_width = data[0]["max"] - data[0]["min"]
    # Plot bins
    for bkt in data:
        # x-position is average runtime, height is count
        plt.bar(bkt["min"],
                bkt["count"],# / num_configs,
                alpha=0.6,
                bottom=0,
                # label="%s - %s" % (bkt["min"], bkt["max"]),
                width=bin_width)
    # Add median line (take median of alread-computed rows' medians)
    md = statistics.median([bkt["median"] for bkt in data if bkt["median"] is not None])
    plt.axvline(md, color='k', linestyle="solid", linewidth=5, label="median = %s" % int(md))
    # Add UNTYPED  & TYPED configuration line
    arbitrary_key = next((bkt["data"][0]["key"] for bkt in data if bkt["data"]))
    untyped = statistics.mean(get_row(fname, "".join(("0" for _ in arbitrary_key))))
    typed   = statistics.mean(get_row(fname, "".join(("1" for _ in arbitrary_key))))
    plt.axvline(untyped, color='g', linestyle='dashed', linewidth=5, label="untyped = %s" % int(untyped))
    plt.axvline(typed, color='r', linestyle='dotted', linewidth=5, label="typed = %s" % int(typed))
    plt.xticks(sorted([bkt["min"] for bkt in data] + [data[-1]["max"]]))#, rotation="vertical")
    plt.xlim(xmax=data[-1]["max"])
    #plt.ylim(ymax=0.5) #50%
    lgd = plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
    plt.savefig(new_name,bbox_extra_artists=(lgd,), bbox_inches='tight')
    print("Saved plot to '%s'" % new_name)
    plt.clf()
    return new_name

# (-> (Listof BinObj) Path-String String String)
def save_file(data, fname, tag):
    # Save data to a file, using fname and tag to create a hopefully-unique name.
    new_name = util.gen_name(fname, tag, "tab")
    with open(new_name, "w") as f:
        f.write("Avg.Runtime\tKeys\n")
        for bkt in ((x for x in data if x["data"])):
            f.write(str(int(bkt["mean"])))
            f.write("\t")
            f.write("\t".join([obj["key"] for obj in bkt["data"]]))
            f.write("\n")
    print("Saved file to '%s'" % new_name)
    return new_name

# (-> (Listof BinObj) Path-String String String)
def save(data, fname, tag):
    save_graph(data, fname, tag)
    return save_file(data, fname, tag)

# (-> Path-String Void)
def main(fname):
    # Try grouping by CI and std-dev. Print results and save actual numbers to a file.
    tbl = stats_of_tab(fname)
    print("Starting with %d points\n" % len(tbl))
    results = [
        make_bins(tbl, 5),
        make_bins(tbl, 6),
        make_bins(tbl, 7),
        make_bins(tbl, 10),
    ]
    x = [save(g, fname, tag) for (g, tag) in results]
    print("All done.")

if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        print("Usage: grouping.py TAB-FILE")
