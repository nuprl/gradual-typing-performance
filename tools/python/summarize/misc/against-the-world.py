"""
  Assess the influence of one group of nodes on overall performance.
"""

import itertools
import math
import statistics
import sys
import util
import modulegraph
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
import matplotlib.pyplot as plt

def is_boundary(title, group, d):
    # True if the group of modules `group` are
    # 1. all typed or all untyped
    # 2. interact with another module across a boundary
    indices = [d[mname][0] for mname in group]
    all_typed = all((title[i] == "1" for i in indices))
    all_untyped = all((title[i] == "0" for i in indices))
    any_boundary = any((title[d[mod][0]] != title[d[req][0]]
                        for mod in group
                        for req in d[mod][1]))
    return (all_typed or all_untyped) and any_boundary

def rows_where_group_is_boundary(fname, group, d):
    # Generate the rows in `fname` for which the group of modules `group`
    # are part of a typed/untyped boundary
    with open(fname, "r") as f:
        next(f)
        for line in f:
            data = line.strip().split(modulegraph.SEP)
            title, cols = data[0], data[1::]
            if is_boundary(title, group, d):
                yield (title, [int(x) for x in cols])
    return # Stop Iteration

def pretty_stable(runtimes, group):
    if len(runtimes) < 2:
        print("Warning: less than 2 runtimes-as-boundary for group '%s'. Ignoring." % group)
        return False
    std = statistics.stdev(runtimes)
    avg = statistics.mean(runtimes)
    return std < (avg / 4)

def count_stable_groups(fname, gname):
    """ Search the space of all configurations for "significant groups"
     - For all groups of (n/2) modules, where n is the number of modules
     - Collect the runtimes of all configurations where the group is typed
       and part of a boundary (determined by the .graph file)
     - If these runtimes are pretty stable, print the group & runtimes
    """
    d = util.dict_of_file(gname)
    num_modules = len(d)
    ## init counters
    num_groups, num_stable, num_unstable = 0, 0, 0
    stable_groups = []
    for group in itertools.combinations(d.keys(), int(num_modules / 2)):
        group = list(group)
        # print("Checking group '%s'" % group)
        num_groups += 1
        runtimes = []
        titles = []
        for title, row in rows_where_group_is_boundary(fname, group, d):
            runtimes.append(statistics.mean(row))
            titles.append(title)
        if pretty_stable(runtimes, group):
            num_stable += 1
            stable_groups.append((group, [(title, int(x)) for (title, x) in zip(titles, runtimes)]))
        else:
            num_unstable += 1
    print("Finished checking %d groups. %d stable, %d unstable." % (num_groups, num_stable, num_unstable))
    print("Stable groups are:\n- %s" % ("\n- ".join((str(x) for x in stable_groups))))
    return

def profile_group(fname, gname, *group):
    # Get information about the modules `group` with respect to the other files.
    # TODO
    group = list(group)
    d = util.dict_of_file(gname)
    print("Profiling group '%s'" % group)
    print("Key:")
    with open(gname, "r") as f:
        next(f)
        for line in f:
            print("  " + line.strip())
    tr_list = [(title, int(statistics.mean(row))) for (title, row) in rows_where_group_is_boundary(fname, group, d)]
    tr_list.sort(key=lambda x:x[1])
    for title, val in tr_list:
        print("Config '%s' has avg. runtime %d" % (title, val))
    ## Plot bar graph
    wd = 200
    num_bars = len(tr_list)
    ind = [i * wd for i in range(0, num_bars)]
    xlabels = [x[0] for x in tr_list]
    xvals = [x[1] for x in tr_list]
    plt.axis([0, wd * (num_bars-1), 0, max(xvals)])
    fig,ax = plt.subplots()
    ax.bar(ind, xvals, width=100)
    ax.set_xticks(ind)
    ax.set_xticklabels(xlabels)
    fig.set_size_inches(25,10)
    plt.xlabel("Config. (%s)" % (sorted([(key, int(val[0])) for (key, val) in d.items()], key=lambda x:x[1])))
    plt.ylabel("Avg. Runtime (ms)")
    plt.title("%s, where group %s is a boundary" % (fname.rsplit("/", 1)[-1], group))
    new_name = util.gen_name(fname, "+".join((x.rsplit(".", 1)[0] for x in group)), "png")
    plt.savefig(new_name)
    plt.clf()
    print("Saved figure as '%s'" % new_name)
    return

if __name__ == "__main__":
    if len(sys.argv) == 3 and sys.argv[1].endswith(".tab") and sys.argv[2].endswith(".graph"):
        count_stable_groups(sys.argv[1], sys.argv[2])
    elif len(sys.argv) > 3:
        profile_group(*sys.argv[1::])
    else:
        print("Usage: against-the-world.py FILE.tab FILE.graph")
