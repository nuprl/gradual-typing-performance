"""
summarize.py

Summarize a project, output results to console, .png, and .tex
"""

### Imports

import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon
import numpy as np
import os
import statistics
import sys

### Constants

# If <=0, print no debugging information.
# Otherwise, print more.
# A higher number means more printouts, up to an un-documented max.
DEBUG = 0
# Separator for .tag and .graph files
SEP = "\t"

### Data Definitions

## GraphDict
# Associate module names to their bitstring index and requires
# Key: String. By convention, ends with .rkt suffix
# Value: (List Index (Listof String)).
## END GraphDict

## Result
#
## END Result

### General Utils

def fold_file(fname, acc, fn, ignore_first_line=True):
    """
        Iterate over lines in `fname`,
        apply `fn` to `acc` and line at each step,
        Return the generated accumulator.
    """
    with open(fname, "r") as f:
        if ignore_first_line:
            next(f)
        for line in f:
            acc = fn(acc, line.strip().split(SEP))
    return acc

def bucketize(fname, get_bkt, num_buckets):
    """
        Group the rows in `fname` into buckets,
        using `get_bkt` to assign a row of the file to a bucket.
        Return the collected list of lists
        (todo? optionally allow 'edit_row' function,
          instead of mushing all values into a list)
    """
    init = [[] for _ in range(1+num_buckets)]
    def add_row(acc, row):
        # TODO maybe broken, idk about this 'functional/imp'
        acc[get_bkt(row[0])].extend([int(x) for x in row[1::]])
        return acc
    return fold_file(fname, init, add_row)

def strip_suffix(fname):
    """
        Remove everything after the rightmost "." in the string `fname`
    """
    return fname.rsplit(".", 1)[0]

### GraphDict helpers

def check_graphfile_titles(col_names, fname):
    # Check that column titles are well-formed
    len_ok = (len(col_names) == 3)
    if not len_ok:
        raise ValueError("expected 3 columns in '%s', got %d columns" % (fname, len(col_names)))
    c0_ok = col_names[0] == "MODULE"
    if not c0_ok:
        raise ValueError("expected first column of '%s' to have title 'MODULE', instead has title '%s'" % (fname, col_names[0]))
    c1_ok = col_names[1] == "INDEX"
    if not c1_ok:
        raise ValueError("expected second column of '%s' to have title 'INDEX', instead has title '%s'" % (fname, col_names[1]))
    c2_ok = col_names[2] == "REQUIRES"
    if not c2_ok:
        raise ValueError("expected third column of '%s' to have title 'REQUIRES', instead has title '%s'" % (fname, col_names[2]))
    #Everything's good!
    return

def check_graphfile_column(values):
    # Check that column values are well-formed.
    # If so, return the parsed values.
    len_ok = 1 < len(values) < 4 # Expecting 2-3 columns (requires are optional)
    if not len_ok:
        raise ValueError("expected 2 or 3 columns in row, got %d columns" % len(values))
    module_name = values[0]
    if not module_name.endswith(".rkt"):
        raise ValueError("expected module name to end with '.rkt', instead got '%s'" % module_name)
    index = None
    try:
        index = int(values[1])
    except ValueError:
        raise ValueError("expected integer INDEX, got %s" % values[1])
    requires = values[2].split(",") if len(values) == 3 else []
    if not (all((rq.endswith(".rkt") for rq in requires))):
        raise ValueError("expected each REQUIRE to be a .rkt filename, instead got '%s'" % requires)
    #if not(bool(requires)):
    #    print("WARNING: module '%s' has no requires" % values[0])
    return [module_name, index, requires]

def infer_graph(fname):
    """
        Try to find the .graph file associated with `fname`
    """
    prefix = strip_suffix(fname)
    gfile1 = "%s.graph" % prefix
    gfile2 = "%s.graph" % prefix.split("-", 1)[0]
    if os.path.exists(gfile1):
        return gfile1
    elif os.path.exists(gfile2):
        return gfile2
    else:
        return None

def dict_of_graphfile(gname):
    """ (-> Path-String GraphDict)
        Convert a .graph file to a GraphDict object
    """
    # Generate a dictionary-graph from a .graph file
    # Keys are module names, like "a.rkt"
    # Values are pairs of indices and requires, like (3, [x.rkt, y.rkt])
    d = {}
    with open(gname, "r") as f:
        check_graphfile_titles(next(f).strip().split(SEP), gname)
        for line in f:
            [mname, i, requires] = check_graphfile_column(line.strip().split(SEP))
            d[mname] = (int(i), requires)
    return d

def basic_row_stats(dataset):
    """ (-> (Listof Nat) (List Nat Nat Nat Nat)
        Compute basic statistics for list `dataset`
    """
    return {
        "mean"     : int(statistics.mean(dataset)),
        "median"   : int(statistics.median(dataset)),
        "variance" : int(statistics.variance(dataset)),
        "min"      : min(dataset),
        "max"      : max(dataset),
    }

def edges_iter(d):
    """
        Iterate over the "require" edges represented in
        the GraphDict `d`.
    """
    for (k, v) in d.items():
        for req in v[1]:
            yield(k, req)

# (-> String (List Nat Nat) Boolean)
def is_boundary(config, i, j):
    """
        True if (i,j) is a typed/untyped boundary edge
    """
    return config[i] != config[j]

def modules_of_config(config, d):
    """
        Given a configuration bitstring and a GraphDict `dict`,
        return the module names (in-order) corresponding to indices
        in the string.
    """
    # Transform dict, so index keys to a module name (drop requires)
    name_of_index = dict([(v[0],k) for (k,v) in d.items()])
    return [name_of_index[i] for i in range(len(config))]

def boundaries_of_config(config, d):
    """
        Return a list of boundary edges in the bitstring `config`.
        Use `d` to identify edges.
    """
    boundaries = []
    for (m1, m2) in edges_iter(d):
        i1 = d[m1][0]
        i2 = d[m2][0]
        if is_boundary(config, i1, i2):
            boundaries.append((m1, m2))
    return boundaries

def basic_config_stats(config, time, graph):
    """
        Basic summary information for a configuration bitstring
    """
    return {"id"         : config
           ,"boundaries" : boundaries_of_config(config, graph)
           ,"time"       : time
           }

### Graphing

def box_plot(dataset, title, xlabel, ylabel, alpha=1, color='royalblue', sym="+"):
    """
        Create and save a boxplot from the list `dataset`.
        Save to filename `output`.
        - alpha : Opacity of graph colors
        - color : box color
        - sym   : symbol for outliers
    """
    fig,ax1 = plt.subplots()
    bp = plt.boxplot(dataset, notch=0, sym=sym, vert=True, whis=1)
    plt.setp(bp['boxes'], color='black')
    plt.setp(bp['whiskers'], color='black')
    plt.setp(bp['fliers'], color='red', marker=sym)
    ax1.yaxis.grid(True, linestyle='-', which='major', color='lightgrey', alpha=0.5)
    # Fancier boxes
    for i in range(len(dataset)):
        box = bp['boxes'][i]
        coords = []
        ## Color the boxes
        for j in range(0,5):
            coords.append((box.get_xdata()[j], box.get_ydata()[j]))
        boxPolygon = Polygon(coords, facecolor=color, alpha=alpha)
        ax1.add_patch(boxPolygon)
        ## Re-draw median lines
        med = bp['medians'][i]
        mx, my = [], []
        for j in range(2):
            mx.append(med.get_xdata()[j])
            my.append(med.get_ydata()[j])
            plt.plot(mx,my, 'k')
        ## Draw avg. dot
        plt.plot([np.average(med.get_xdata())], [np.average(dataset[i])], color='w', marker='*', markeredgecolor='k')
    ## plot axis: runtime + num types
    ax1.set_axisbelow(True)
    ax1.set_title(title)
    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(ylabel)
    #if xlabels:
    #    plt.xticks(posns, xlabels)
    #else:
    #    plt.xticks(posns)
    ## Legend
    # Reset y limit
    ymin,ymax = ax1.get_ylim()
    ax1.set_ylim(ymin-5, ymax)
    plt.figtext(0.80, 0.01, '*', color='white', backgroundcolor=color,weight='roman', size='medium')
    plt.figtext(0.82, 0.01, ' Average Value', color='black', weight='roman', size='x-small')
    ## Save & clear
    output = "%s-boxplot.png" % title
    plt.savefig(output)
    plt.clf()
    print("Saved figure to %s" % output)
    return output

def violin_plot(dataset, title, xlabel, ylabel, alpha=1, color='royalblue', meanmarker='*', positions=None, xlabels=None):
    """
        Create and save a violin plot representing the list `dataset`.
        Optional parameters modify the graph:
        - `alpha` = Float [0,1], the opacity of colors in the output graph
        - `color` = Color to use for violins in the output graph
        - `meanmarker` = Symbol to use to mark the violin's mean
        - `positions` = x-axis positions for each violin
    """
    # Set default values
    posns = positions or list(range(1,1+len(dataset)))
    fig,ax1 = plt.subplots() #add figsize?
    vp = plt.violinplot(dataset, positions=posns, showmeans=True, showextrema=True, showmedians=True)
    ## Re-color bodies
    for v in vp['bodies']:
        v.set_edgecolors('k')
        v.set_facecolors(color)
        v.set_alpha(alpha)
    ## Re-color median, min, max lines to be black
    for field in ['cmaxes', 'cmins', 'cbars', 'cmedians']:
        vp[field].set_color('k')
    ## Draw stars, for means
    # Make original mean line invisible
    vp['cmeans'].set_color(color)
    vp['cmeans'].set_alpha(0)
    # Draw the mean marker
    for i in range(len(dataset)):
        plt.plot(posns[i], [np.average(dataset[i])], color='w', marker=meanmarker, markeredgecolor='k')
    # Light gridlines
    ax1.yaxis.grid(True, linestyle='-', which='major', color='lightgrey', alpha=0.5)
    # Titles + legend
    ax1.set_axisbelow(True)
    ax1.set_title(title)
    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(ylabel)
    if xlabels:
        plt.xticks(posns, xlabels)
    else:
        plt.xticks(posns)
    ## Legend
    # Reset y limit
    ymin,ymax = ax1.get_ylim()
    ax1.set_ylim(ymin-5, ymax)
    plt.figtext(0.80, 0.01, meanmarker, color='white', backgroundcolor=color, weight='roman', size='medium')
    plt.figtext(0.82, 0.01, ' Average Value', color='black', weight='roman', size='x-small')
    output = "%s-violin.png" % title
    plt.savefig(output)
    plt.clf()
    print("Saved violin plot to '%s'" % output)
    return output

### Parsing

def parse_options(argv):
    """
        Pre-processing for command line arguments.
        Pull the options out, return the filtered arguments
        and the parsed dictionary of options
    """
    # TODO actually implement this
    options = { "verbose" : DEBUG }
    return argv[-1], options

def parse_args(argv):
    """ (-> (Listof String) (Pairof (Listof Any) (Dictof String Any)))
        Parse command-line arguments.
        Return the parsed args and a dictionary of option settings.
    """
    if len(argv) == 0:
        return None, None
    target, options = parse_options(argv)
    gfile = infer_graph(target)
    if gfile is None:
        raise ValueError("Could not find corresponding graph for file %s.\n  Suggestion: create a file '%s.graph' with columns 'MODULE\tINDEX\tREQUIRES' documenting\n  - The important modules in the project\n  - Their indexes in the configuration bitstrings\n  - The files these modules require." % (target, strip_suffix(target)))
    d = dict_of_graphfile(gfile)
    return [target, d], {"verbose" : DEBUG}

### Summaries

def tabfile_of_rktd(rktdfile):
    """ (-> Path-String Path-String)
        Input: a .rktd file; the results of running the benchmarks.
        Output: the string name of a newly-generated .tab file
                (a more human-readable and Python-parse-able version of the .rktd)
    """
    # We have a Racket script to generate the .tab file,
    # make sure it exists.
    if not os.path.exists("sexp-to-tab.rkt"):
        raise ValueError("internal error: cannot file the 'sexp-to-tab.rkt' script. Sorry.")
    # TODO replace with a call to Python's 'subprocess'
    os.system("racket sexp-to-tab.rkt %s" % rktdfile)
    # Strip the suffix from the input file, replace with .tab
    return "%s.tab" % rktdfile.rstrip(".", 1)[0]

def all_cells_matching(tabfile, config_pred):
    """
        Return a list of all data cells in rows whose titles
        match `config_pred`.
    """
    def process_row(row):
        title = row[0]
        if config_pred(title):
            return [int(x) for x in row[1::]]
        else:
            return []
    return fold_file(tabfile, [], lambda acc,row: acc + process_row(row))

## config manipulation

def is_untyped_config(s):
    """
        True if `s` is the fully-UNtyped configuration.
    """
    return all((c == "0" for c in s))

def is_typed_config(s):
    """
        True if `s` is the fully-typed configuration.
    """
    return all((c == "1" for c in s))

def num_typed_modules(s):
    """
        Count the number of typed modules in the configuration
        represented by `s`.
        Examples:
        - 0010, 0100, 1000 all have 1 typed module
        - 1111             has 4 typed modules
    """
    return sum((1 for c in s if c == "1"))

def best_row(tabfile, metric, val_of_row, init=None):
    """ (->* (Path-String (-> A A Boolean) (-> (Listof Nat) A)) (A) A)
        Return the "value" of the best row, according to `metric`.
        i.e. the row R such that for any other row r: metric(val_of_row(r), val_of_row(R)) = True
        - `tabfile` filename, a tab-separated file
        - `metric`  a < judgment on rows
        - `val_of_row` a pre-processing transformation on rows (used to filter, take mean, etc)
        - `init` optionally, a first value to compare to
    """
    def process_row(acc, row):
        # Get value from row.
        # Return whichever is greater: this val or acc
        val = val_of_row(row)
        if acc is None:
            return val
        elif metric(acc, val):
            return val
        else:
            return acc
    return fold_file(tabfile, init, process_row)

def results_of_tab(tabfile, dgraph):
    """ (-> Path-String GraphDict Result)
        Input: a .tab file, an overall summary of running all configurations
               of a project.
        Output: a Result object.
    """
    fname = strip_suffix(tabfile).rsplit("/", 1)[-1]
    ## Collect over-all running times, plot
    #o_raw = all_cells_matching(tabfile, lambda x: True)
    #o_violin = violin_plot([o_raw]           #dataset
    #                      ,"%s_overall-runtime" % fname
    #                      ,""
    #                      ,"Runtime (ms)")
    # Collect untyped, in-between, and typed running times, plot
    u_raw = all_cells_matching(tabfile, is_untyped_config)
    g_raw = all_cells_matching(tabfile, lambda x: not (is_typed_config(x) or is_untyped_config(x)))
    t_raw = all_cells_matching(tabfile, is_typed_config)
    ugt_violin = violin_plot([u_raw, g_raw, t_raw]
                            ,"%s_untyped-vs-gradual-vs-typed" % fname
                            ,"Configuration"
                            ,"Runtime (ms)"
                            ,xlabels=["untyped","gradual\n(all configs)","typed"])
    # Collect absolute BEST and WORST times+configs
    # TODO generalize to remember the top 10, or best 10%
    b_config, b_time = best_row(tabfile
                        ,lambda acc,tmp: tmp[1] < acc[1]
                        ,lambda row:(row[0], int(statistics.mean([int(x) for x in row[1::]]))))
    w_config, w_time = best_row(tabfile
                        ,lambda acc,tmp: acc[1] < tmp[1]
                        ,lambda row:(row[0], int(statistics.mean([int(x) for x in row[1::]]))))
    num_modules = len(w_config)
    stats = {
        #"overall"  : {"img"     : [o_violin]
        #             ,"summary" : basic_row_stats(o_raw)}
        "ugt"     : {"img" : ugt_violin
                     ,"summary" : {"untyped" : basic_row_stats(u_raw)
                                  ,"gradual" : basic_row_stats(g_raw)
                                  ,"typed"   : basic_row_stats(t_raw)}}
        ,"best"    : basic_config_stats(b_config, b_time, dgraph)
        ,"worst"   : basic_config_stats(w_config, w_time, dgraph)
        ,"bucketed": {"img" : violin_plot(bucketize(tabfile, num_typed_modules, num_modules)
                                         ,"%s_by-typed-modules" % fname
                                         ,"Number of Typed Modules"
                                         ,"Runtime (ms)"
                                         ,positions=range(0, 1+num_modules)
                                         ,xlabels=range(1, 2+num_modules))}
    }
    return stats

def pretty_print(results):
    """ (-> Result Void)
        Given a Result object, pretty print summary information
        to console.
    """
    print("HERE ARE YOUR RESULTS\n%s" % results)

def save_as_tex(results):
    """ (-> Result Void)
        Given an Result object, save results to a nicely-formatted .tex file.
    """
    raise NotImplementedError("save_as_tex")

### Main functions, dispatch

def main(*args, **options):
    """ (-> (Listof Any) (Dictof String Any) Void)
        Collect summary information from the input args by dispatching to the
        appropriate helper function.
        Pretty-print and save results.
    """
    results = None
    if len(args) == 2 and args[0].endswith(".rktd"):
        # Parse the .rktd file into a .tab file, parse the .tab file
        tabfile = tabfile_of_rkt(args[0])
        results = results_of_tab(tabfile, args[1])
    elif len(args) == 2 and args[0].endswith(".tab"):
        # Collect results from the .tab file
        results = results_of_tab(args[0], args[1])
    else:
        raise ValueError("unexpected arguments '%s'" % str(args))
    #save_as_tex(results)
    pretty_print(results)
    return

def print_help():
    """ (-> Void)
        Print usage information
    """
    print("Usage: summarize.py FILE.tab")

### Entry point

if __name__ == "__main__":
    args, options = parse_args(sys.argv[1::])
    if bool(args):
        main(*args, **options)
    else:
        print_help()

