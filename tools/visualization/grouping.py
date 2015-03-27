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
            tbl.append((title, obj))
    return tbl

def group_by(tbl, measure):
    # Cluster stat objects in `tbl` by the binary relation `measure`
    # Each object appears in just one group
    groups = []
    seen = set([])
    for (title1, obj1) in tbl:
        # For every un-sorted entry in the table
        if title1 not in seen:
            seen.add(title1)
            t1_group = [title1]
            for (title2, obj2) in tbl: # Could skip over entried that come before `obj1`...
                # For every other un-sorted entry, check if the confidence intervals overlap
                if (title2 not in seen) and (title2 != title1) and measure(obj1, obj2):
                    # Add this title to the small group we're building
                    seen.add(title2)
                    t1_group.append(title2)
            # Add the small group to the overall, sorted table
            groups.append(t1_group)
    return groups

def overlap(pair1, pair2):
    # For each of the 4 points (lo & hi, for each object)
    # just check that the opposite points surround it.
    l1 = pair1[0]
    h1 = pair1[1]
    l2 = pair2[0]
    h2 = pair2[1]
    return ((l1 < h2 and l1 > l2)
            or (h1 < h2 and h1 > l2)
            or (l2 < h1 and l2 > l1)
            or (h2 < h1 and h2 > l1))

def ci_overlap(obj1, obj2):
    # Measure: Dumb algorithm to check if confidence intervals overlap.
    return overlap(obj1["confidence-interval"] , obj2["confidence-interval"])

def std_overlap(obj1, obj2, num_devs):
    # Measure: Check if standard deviations of two objects overlap
    m1 = obj1["mean"]
    m2 = obj2["mean"]
    s1 = obj1["std"] * num_devs
    s2 = obj2["std"] * num_devs
    return overlap((m1 - s1 , m1 + s1) , (m2 - s2 , m2 + s2))

def group_by_confidence(tbl):
    # Cluster stat objects in `tbl` by their confidence intervals.
    return group_by(tbl, ci_overlap)

def group_by_std(tbl, num_devs):
    # Cluster stat objects in `tbl` by their standard deviation.
    # `num_devs` sets the number of STD to use for the interval (should be a natural number between 0 and 4)
    if num_devs < 1 or num_devs > 20:
        print("WARNING: resetting argument num_devs=%s to the more reasonable value 1" % num_devs)
        num_devs = 1
    return group_by(tbl, (lambda obj1, obj2 : std_overlap(obj1, obj2, num_devs)))


def save_file(data, fname, tag):
    # Save data to a file, using fname and tag to create a hopefully-unique name.
    new_name = util.gen_name(fname, tag, "txt")
    with open(new_name, "w") as f:
        for group in data:
            f.write("\t".join(group))
            f.write("\n")
    print("Saved file to '%s'" % new_name)
    return new_name

def fold_all_values(fname, g, init):
    # Apply g to the list of values in each row of the table, plus an accum.
    acc = init
    with open(fname, "r") as f:
        next(f)
        for line in f:
            acc = g([acc] + [int(x) for x in line.strip().split("\t")[1::]])
    return acc

def count_lines(fname):
    lines = 0
    with open(fname, "r") as f:
        for line in f:
            lines += 1
    return lines

def get_values(key, fname):
    # Return values associated with `key` in `fname`
    res = None
    with open(fname, "r") as f:
        for line in f:
            data = line.strip().split("\t")
            if data[0] == key:
                res = [int(x) for x in data[1::]]
                break
    return res

def mean_of_bucket(bkt, fname):
    # For each key in `bkt`
    # Get the mean of values associated with the key in `fname`
    # Return the average of all these means
    return statistics.mean([statistics.mean(get_values(key, fname))
                            for key in bkt])

def save_graph(data, fname, tag):
    # `data` are the groups (by ID)
    # `fname` is the raw data file
    # `tag` helps create a uniquely-named output file
    plt.xlabel("Avg. Runtime")
    plt.ylabel("Pct. Configs")
    plt.title(tag)
    ### pyplot already picks a good axis -- no need to set manually
    #max_runtime = fold_all_values(fname, max, 0)
    #min_runtime = fold_all_values(fname, min, max_runtime)
    #plt.axis([min_runtime, max_runtime])
    num_configs = count_lines(fname) - 1
    # xs = x-position = average runtime of each bucket
    xs = [mean_of_bucket(bkt, fname) for bkt in data]
    # ys = height = percent of total values in each bucket
    ys = [len(x) / num_configs for x in data]
    plt.bar(xs, ys, linewidth=5)
    new_name = util.gen_name(fname, tag, "png")
    plt.savefig(new_name)
    print("Saved plot to '%s'" % new_name)
    plt.clf()
    return new_name

def save(data, fname, tag):
    save_graph(data, fname, tag)
    return save_file(data, fname, tag)

def report_group(groups, tag):
    counts = [len(x) for x in groups]
    print("Grouping by %s gives %d groups\n- at most %d in each group\n- average of %d across groups\n- median of %d across groups\n"
          % (tag, len(groups), max(counts), statistics.mean(counts), statistics.median(counts)))
    return (groups, tag)

def main(fname):
    # Try grouping by CI and std-dev. Print results and save actual numbers to a file.
    TBL = stats_of_tab(fname)
    print("Starting with %d points\n" % len(TBL))
    results = [
        report_group(group_by_confidence(TBL), "95-confidence"),
        report_group(group_by_std(TBL, 1), "STD-1"),
        report_group(group_by_std(TBL, 2), "STD-2"),
        report_group(group_by_std(TBL, 3), "STD-3"),
        # report_group(group_by_std(TBL, 4), "STD-4"),
        # report_group(group_by_std(TBL, 5), "STD-5"),
        # report_group(group_by_std(TBL, 6), "STD-6"),
        # report_group(group_by_std(TBL, 7), "STD-7"),
        # report_group(group_by_std(TBL, 8), "STD-8"),
        # report_group(group_by_std(TBL, 9), "STD-9"),
        # report_group(group_by_std(TBL, 10), "STD-10"),
        # report_group(group_by_std(TBL, 11), "STD-11"),
        # report_group(group_by_std(TBL, 12), "STD-12"),
        # report_group(group_by_std(TBL, 13), "STD-13"),
        # report_group(group_by_std(TBL, 14), "STD-14"),
    ]
    x = [save(g, fname, tag) for (g, tag) in results]
    print("All done.")

if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        print("Usage: grouping.py TAB-FILE")
