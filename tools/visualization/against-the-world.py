"""
  Assess the influence of one group of nodes on overall performance.
"""

import itertools
import math
import statistics
import sys
import modulegraph

def dict_of_file(gname):
    # Generate a dictionary-graph from a .graph file
    # Keys are module names, like "a.rkt"
    # Values are pairs of indices and requires, like (3, [x.rkt, y.rkt])
    d = {}
    with open(gname, "r") as f:
        modulegraph._check_colnames(next(f).strip().split(modulegraph.SEP), gname)
        for line in f:
            [mname, i, requires] = modulegraph._check_col(line.strip().split(modulegraph.SEP))
            d[mname] = (int(i), requires)
    return d

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
        print("Warning: less than 2 runtimes-as-boundary for group '%s'. Ignoring.")
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
    d = dict_of_file(gname)
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
    print("Profiling group '%s'" % group)
    raise NotImplementedError("bear")

if __name__ == "__main__":
    if len(sys.argv) == 3 and sys.argv[1].endswith(".tab") and sys.argv[2].endswith(".graph"):
        count_stable_groups(sys.argv[1], sys.argv[2])
    elif len(sys.argv) > 3:
        profile_group(*sys.argv[1::])
    else:
        print("Usage: against-the-world.py FILE.tab FILE.graph")
