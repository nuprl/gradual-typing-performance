"""
edge-violin.py

The violin plots look good for configurations.
Let's try them for understanding edges.

Create a graph based on a SINGLE EDGE from the module graph.
Show the running times partitioned by number of typed modules
for all configurations where the edge is included,
and for all configs where it is not included.

This script ignores the direction of requires.
"""

import itertools
import sys
import violinplot
import util
import os
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
import matplotlib.pyplot as plt

def is_edge(config, edge):
    # True if (i,j) is a typed/untyped boundary edge
    return config[edge[0]] != config[edge[1]]

def infer_module_names(fname, *args):
    # Try finding a .graph file near the filename,
    # if so, return a name in place of each argument index.
    # if not, print a warning and return the arguments
    gfile1 = "%s.graph" % fname.rsplit(".", 1)[0]
    gfile2 = "%s.graph" % fname.rsplit(".", 1)[0].split("-", 1)[0]
    gfile = None
    if os.path.exists(gfile1):
        gfile = gfile1
    elif os.path.exists(gfile2):
        gfile = gfile2
    else:
        print("Warning: could not find .graph file")
        return args
    d = util.dict_of_file(gfile)
    name_of_index = {}
    for (k, (i, rqs)) in d.items():
        name_of_index[i] = k
    return [name_of_index[index].rsplit(".", 1)[0] for index in args]

def make_plot(fname, edge):
    source_file, dest_file = infer_module_names(fname, *edge)
    data_with    = violinplot.data_by_numtyped(fname, lambda cfg: is_edge(cfg, edge))
    data_without = violinplot.data_by_numtyped(fname, lambda cfg: not is_edge(cfg, edge))
    ## Draw violins
    if all((len(x) == 0 for x in data_with)):
        print("Skipping edge %s , %s for lack of data" % (source_file, dest_file))
        return
    fig,ax1 = plt.subplots() #add figsize?
    violinplot.draw_violin(data_with[1:-1], alpha=0.8, color='royalblue', meanmarker='*')
    violinplot.draw_violin(data_without[1:-1], alpha=0.8, color='darkorange', meanmarker='o')
    ## add a light-colored horizontal grid
    ax1.yaxis.grid(True, linestyle='-', which='major', color='lightgrey', alpha=0.5)
    ## Legend
    ymin,ymax = ax1.get_ylim()
    ax1.set_ylim(ymin-5, ymax)
    plt.figtext(0.80, 0.04, "-", color='royalblue', backgroundcolor='royalblue', weight='roman', size='x-small')
    plt.figtext(0.82, 0.04, "With boundary", color='k', weight='roman', size='x-small')
    plt.figtext(0.80, 0.01, '-', color='darkorange', backgroundcolor='darkorange', weight='roman', size='x-small')
    plt.figtext(0.82, 0.01, 'Without boundary', color='black', weight='roman', size='x-small')
    ax1.set_axisbelow(True)
    ax1.set_title("%s-%s-%s" % (fname.rsplit("/",1)[-1].rsplit(".")[0], source_file, dest_file))
    ax1.set_xlabel("Num. Typed Modules")
    ax1.set_ylabel("Runtime (ms)")
    plt.xticks(range(1,1+len(data_with[1:-1])), range(1, 1+len(data_with[1:-1])))
    new_name = util.gen_name(fname, "edge-%s-%s" % (source_file, dest_file), "png")
    plt.savefig(new_name)
    plt.clf()
    print("Saved figure to %s" % new_name)
    return

def main(fname):
    # Generate all edge combinations,
    # make all the graphs
    num_modules = util.count_modules(fname)
    for edge in itertools.combinations(range(num_modules), 2):
        make_plot(fname, edge)
    return

if __name__ == "__main__":
    if len(sys.argv) == 2 and sys.argv[1].endswith(".tab"):
        main(sys.argv[1])
    else:
        print("Usage: edge-violin.py FILE.tab SOURCE DEST\n- SOURCE and DEST are natural numbers; indexes into configuration bitstrings.")

