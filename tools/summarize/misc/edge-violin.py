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
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
import matplotlib.pyplot as plt

def non_overlapping(d1, d2):
    # True if 
    return all((min(set1) > max(set2)
                or min(set2) > max(set1)
               for (set1, set2) in zip(d1, d2)))

def do_not_make_graph(data_with, data_without, fnames):
    # Check the datasets, make sure it's worthwhile to make a graph
    dont_graph = False
    # - All-empty data is worthless
    if not data_with:
        #print("No 'data_with', skipping %s" % fnames)
        dont_graph = True
    elif not data_without:
        #print("No 'data_without', skipping %s" % fnames)
        dont_graph = True
    elif all((len(x) == 0 for x in data_with)):
        #print("Skipping edge %s for lack of data" % fnames)
        dont_graph = True
    elif len(data_with) != len(data_without):
        raise ValueError("WTF data with/without have different length")
    elif not non_overlapping(data_with, data_without):
        #print("Skipping edge %s because some data overlap" % fnames)
        dont_graph = True
    else:
        print("Ready to graph %s" % fnames)
    return dont_graph

def remove_empty(d1, d2):
    # Zip through datasets (pairwise),
    # make sure both are non-empty,
    # erase otherwise
    # Return the non-empty corresponding pairs in two lists
    # and a list of their original indices
    xs, ys, posns = [], [], []
    for i in range(len(d1)):
        if d1[i] and d2[i]:
            xs.append(d1[i])
            ys.append(d2[i])
            posns.append(i)
    return xs, ys, posns

def make_plot(fname, edge_list):
    # `edge_list` is a list of edges to check
    # Compare performance when these edges are not boundaries vs.
    # when any one edge is a boundary
    fnames = "+".join(("-".join(util.infer_module_names(fname, *edge)) for edge in edge_list))
    data_with    = violinplot.data_by_numtyped(fname, lambda cfg: any((util.is_boundary(cfg, edge) for edge in edge_list)))
    data_without = violinplot.data_by_numtyped(fname, lambda cfg: all((not util.is_boundary(cfg, edge) for edge in edge_list)))
    data_with, data_without, posns = remove_empty(data_with, data_without)
    ## Make sure data's worth graphing
    if do_not_make_graph(data_with, data_without, fnames):
        return
    ## Draw violins
    fig,ax1 = plt.subplots() #add figsize?
    violinplot.draw_violin(data_with, alpha=0.8, color='royalblue', meanmarker='*', positions=posns)
    violinplot.draw_violin(data_without, alpha=0.8, color='darkorange', meanmarker='o', positions=posns)
    ## add a light-colored horizontal grid
    ax1.yaxis.grid(True, linestyle='-', which='major', color='lightgrey', alpha=0.5)
    ## Legend
    ymin,ymax = ax1.get_ylim()
    ax1.set_ylim(ymin-5, ymax)
    plt.figtext(0.80, 0.04, "-", color='royalblue', backgroundcolor='royalblue', weight='roman', size='x-small')
    plt.figtext(0.82, 0.04, "Any edge is a boundary", color='k', weight='roman', size='x-small')
    plt.figtext(0.80, 0.01, '-', color='darkorange', backgroundcolor='darkorange', weight='roman', size='x-small')
    plt.figtext(0.82, 0.01, 'No edges are boundaries', color='black', weight='roman', size='x-small')
    ax1.set_axisbelow(True)
    ax1.set_title("%s=%s" % (fname.rsplit("/",1)[-1].rsplit(".")[0], fnames))
    ax1.set_xlabel("Num. Typed Modules")
    ax1.set_ylabel("Runtime (ms)")
    plt.xticks(posns, posns)
    new_name = util.gen_name(fname, "edge=%s" % fnames, "png")
    plt.savefig(new_name)
    plt.clf()
    plt.close(fig)
    print("Saved figure to %s" % new_name)
    return

def main(fname):
    # Generate all edge combinations,
    # make all the graphs
    num_modules = util.count_modules(fname)
    edges = util.infer_edges(fname)
    if edges is None:
        print("Error: could not find graph file for '%s'" % fname)
        return
    ## All combos that are reasonably small
    for group_size in range(1, min(4, len(edges))):
        for edge_list in itertools.combinations(edges, group_size):
            make_plot(fname, edge_list)
    return

if __name__ == "__main__":
    if len(sys.argv) == 2 and sys.argv[1].endswith(".tab"):
        main(sys.argv[1])
    else:
        print("Usage: edge-violin.py FILE.tab SOURCE DEST\n- SOURCE and DEST are natural numbers; indexes into configuration bitstrings.")

