"""
summarize.py

Summarize a project, output results to console, .png, and .tex
"""

### Imports

import math
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon
import networkx as nx
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
# Place to store intermediate results, when sampling.
TMP_RESULTS = "summarize-tmp-results.txt"

### Data Definitions

## GraphDict
# Associate module names to their bitstring index and requires
# Key: String. By convention, ends with .rkt suffix
# Value: (List Index (Listof String)).
##

## Result
# it's a giant dictionary sorry guys
##

### General Utils

def count_runs(fname):
    with open(fname, "r") as f:
        hdr = next(f)
    title = hdr.split("\t", 1)[0]
    if title != "Run":
        raise ValueError("Expected first column of '%s' to be labeled 'Run', got label '%s'" % (fname, title))
    last_index = hdr.rsplit("\t", 1)[-1]
    return int(last_index)

def count_modules(fname):
    """ (-> Path-String Nat)
        Count the number of modules in the project.
    """
    with open(fname, "r") as f:
        _     = next(f)
        row   = next(f)
        title = row.split("\t", 1)[0]
    return len(title)

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

def bucketize(fname, get_bkt, num_buckets, pred=None):
    """
        Group the rows in `fname` into buckets,
        using `get_bkt` to assign a row of the file to a bucket.
        Return the collected list of lists
        (todo? optionally allow 'edit_row' function,
          instead of mushing all values into a list)
    """
    init = [[] for _ in range(1+num_buckets)]
    def add_row(acc, row):
        # TODO don't graph all points because ... (I had a reason at some point)
        if (pred is None) or (pred(row[0])):
            acc[get_bkt(row[0])].extend([int(x) for x in row[1::]])
        return acc
    return fold_file(fname, init, add_row)

def strip_suffix(fname):
    """
        Remove everything after the rightmost "." in the string `fname`
    """
    return fname.rsplit(".", 1)[0]

def check_directory_structure(dname):
    """ (-> Path-String Void)
        Assert that the contents of directory `dname` are well-formed.
        - contains typed/ untyped/ and base/ directories
        - contains main.rkt file (in any above dir, or both/)
    """
    raise NotImplementedError("todo")

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
    gfile3 = "%s.graph" % prefix.rsplit("/", 1)[-1]
    if os.path.exists(gfile1):
        return gfile1
    elif os.path.exists(gfile2):
        return gfile2
    elif os.path.exists(gfile3):
        return gfile3
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
    if not dataset:
        return None
    stat = {
        "mean"     : int(statistics.mean(dataset)),
        "median"   : int(statistics.median(dataset)),
        "variance" : int(statistics.variance(dataset)),
        "min"      : min(dataset),
        "max"      : max(dataset),
    }
    Z = 2.04 # Close to t-stat for 30 degrees of freedom (TODO, make less magic)
    delta = Z * (math.sqrt(stat["variance"]) / math.sqrt(len(dataset)))
    stat["ci"] = [int(stat["mean"] - delta), int(stat["mean"] + delta)]
    return stat

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

def bar_plot(xvalues, yvalues, title, xlabel, ylabel, alpha=1, color='royalblue', xlabels=None, width=0.8):
    fig,ax1 = plt.subplots()
    bar = plt.bar(xvalues, yvalues, width=width, color=color, alpha=alpha)
    if xlabels:
        plt.xticks([x + width/2 for x in xvalues], xlabels)
    ax1.set_title(title)
    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(ylabel)
    output = "%s-bar.png" % title
    plt.savefig(output)
    plt.clf()
    plt.close()
    print("Saved bar chart to '%s'" % output)
    return output

def module_plot(graph, fname, title, alpha=1, boundaries=[], edgecolor="k", untypedcolor='royalblue', typedcolor='darkorange'):
    config = title.split(":", 1)[0].rsplit(" ", 1)[-1]
    ## Make networkx graph
    g = nx.DiGraph()
    for (k,v) in graph.items():
        g.add_node(k)
        for req in v[1]:
            g.add_edge(k, req)
    ## Make pyplot
    pos = nx.circular_layout(g, scale=1)
    fig,ax1 = plt.subplots()
    # Untyped nodes, or the default
    nx.draw_networkx_nodes(g, pos, node_size=1000, alpha=alpha
                           ,nodelist=[k for (k,v) in graph.items() if is_untyped(v[0], config)]
                           ,node_color=untypedcolor)
    # Typed nodes
    nx.draw_networkx_nodes(g, pos, node_size=1000, alpha=alpha
                           ,nodelist=[k for (k,v) in graph.items() if is_typed(v[0], config)]
                           ,node_color=typedcolor)
    nx.draw_networkx_labels(g, pos, dict([(k,k) for k in graph.keys()]))
    nx.draw_networkx_edges(g, pos, edge_color=edgecolor, alpha=alpha)
    ## Draw boundaries
    nx.draw_networkx_edges(g, pos, edgelist=boundaries, edge_color="r", width=4)
    output = "%s-module-graph-%s.png" % (fname, config)
    ax1.set_title(title)
    plt.axis("off")
    plt.savefig(output)
    plt.clf()
    plt.close()
    print("Saved module graph to '%s'" % output)
    return output

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
    plt.close()
    print("Saved figure to %s" % output)
    return output

def draw_violin(dataset, posns, alpha=1, color='royalblue', meanmarker="*"):
    """
        Draw a violin to the current plot.
        Color the mean point.
        (Shared helper for `violin_plot` and `double_violin`)
    """
    ## Add data
    vp = plt.violinplot(dataset, positions=posns, showmeans=True, showextrema=True, showmedians=True)
    ## Re-color bodies
    for v in vp['bodies']:
        v.set_edgecolors('k')
        v.set_facecolors(color)
        v.set_alpha(alpha)
    ## Re-color median, min, max lines to be black
    for field in ['cmaxes', 'cmins', 'cbars', 'cmedians']:
        vp[field].set_color('k')
    ## Draw mean markers
    # Make original mean line invisible
    vp['cmeans'].set_alpha(0)
    # Draw the mean marker
    for i in range(len(dataset)):
        plt.plot(posns[i], [np.average(dataset[i])], color='w', marker=meanmarker, markeredgecolor='k')
    return

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
    draw_violin(dataset, posns, alpha=alpha, color=color, meanmarker=meanmarker)
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
    plt.close()
    print("Saved violin plot to '%s'" % output)
    return output

def remove_empty(d1, d2):
    """
       Zip through datasets (pairwise),
       make sure both are non-empty; erase if empty.
       Return the non-empty corresponding pairs in two lists
       and a list of their original indices
    """
    xs, ys, posns = [], [], []
    for i in range(len(d1)):
        if d1[i] and d2[i]:
            xs.append(d1[i])
            ys.append(d2[i])
            posns.append(i)
    return xs, ys, posns

def double_violin_plot(series, title, xlabel, ylabel, alpha=0.8, colors=['royalblue','darkorange'], markers=['*','o'], legend=False):
    """
        Plot 2 violins to the same plot.
        TODO think of more ways to compare 2 datasets
        TODO get working for more than 2 datasets, if necessary
    """
    # Set default values
    series1, series2, posns = remove_empty(series[0], series[1])
    fig,ax1 = plt.subplots() #add figsize?
    draw_violin(series1, posns, alpha=alpha, color=colors[0], meanmarker=markers[0])
    draw_violin(series2, posns, alpha=alpha, color=colors[1], meanmarker=markers[1])
    ## Light gridlines
    ax1.yaxis.grid(True, linestyle='-', which='major', color='lightgrey', alpha=0.5)
    ## Legend
    if legend and len(legend) == len(series):
        ymin,ymax = ax1.get_ylim()
        ax1.set_ylim(ymin-5, ymax)
        plt.figtext(0.70, 0.04, "-", color=colors[0], backgroundcolor=colors[0], weight='roman', size='x-small')
        plt.figtext(0.72, 0.04, legend[0], color='k', weight='roman', size='x-small')
        plt.figtext(0.70, 0.01, '-', color=colors[1], backgroundcolor=colors[1], weight='roman', size='x-small')
        plt.figtext(0.72, 0.01, legend[1], color='black', weight='roman', size='x-small')
    # Titles + legend
    ax1.set_axisbelow(True)
    ax1.set_title(title)
    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(ylabel)
    output = "%s-dv.png" % title
    plt.savefig(output)
    plt.clf()
    plt.close()
    print("Saved double violin plot to '%s'" % output)
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
    elif any((x in argv for x in ["help", "-help", "--help"])):
        return None, None
    target, options = parse_options(argv)
    gfile = infer_graph(target)
    if gfile is None:
        print("Error: Could not find corresponding graph for file %s.\n  Suggestion: create a file '%s.graph' with columns 'MODULE\tINDEX\tREQUIRES' documenting\n  - The important modules in the project\n  - Their indexes in the configuration bitstrings\n  - The files these modules require." % (target, strip_suffix(target)))
        sys.exit(1)
    d = dict_of_graphfile(gfile)
    return [target, d], {"verbose" : DEBUG}

### Sampling

def sample_file(fname, iters=50, drop=3):
    """
        Execute `fname` many times, collecting results into a temporary file
        and finally returning the average of all results.
        Use `drop` to set the number of warm-up trials.

        Expects that running `fname` causes Racket's time output to print to STDOUT.
    """
    if not os.path.exists(fname):
        raise ValueError("Cannot run '%s'. File not found." % fname)
    dname, main = fname.rsplit("/", 1)
    cwd = os.getcwd()
    os.chdir(dname)
    with open(TMP_RESULTS, "w") as f:
        f.write("")
    make_cmd = "raco make %s" % main
    drop_cmd = "for i in {1..%s}; do racket %s; done" % (drop, main)
    run_cmd  = "for i in {1..%s}; do racket %s >> %s; done" % (iters, main, TMP_RESULTS)
    os.system("; ".join([make_cmd, drop_cmd, run_cmd]))
    times = []
    with open(TMP_RESULTS, "r") as f:
        for line in f:
            times.append(int(re.match(r'^cpu: ([0-9]+) .*', line).group(1)))
    return statistics.mean(times)

def sample_config(base_folder, config, entry_point="main.rkt", iters=50, drop=3):
    """
        Sample the configuration `config`
    """
    fname = "%s/variation%s/%s" % (base_folder, config, entry_point)
    return sample_file(fname, iters=iters, drop=drop)

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
    return "%s.tab" % rktdfile.rsplit(".", 1)[0]

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

def is_untyped(key, s):
    return s[key] == "0"

def is_typed(key, s):
    return s[key] == "1"

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

def best_rows(tabfile, limit, metric, val_of_row, init=None):
    """ (->* (Path-String (-> A A Boolean) (-> (Listof Nat) A)) (A) A)
        Return the "value" of the best row, according to `metric`.
        i.e. the row R such that for any other row r: metric(val_of_row(r), val_of_row(R)) = True
        - `tabfile` filename, a tab-separated file
        - `metric`  a < judgment on rows
        - `val_of_row` a pre-processing transformation on rows (used to filter, take mean, etc)
        - `init` optionally, a first value to compare to
    """
    cache = [None] * limit
    cache[0] = init
    def process_row(acc, row):
        # Get value from row.
        # Insert into the list `acc`
        if is_untyped_config(row[0]) or is_typed_config(row[0]):
            # Ignore fully typed / untyped configs
            return acc
        val = val_of_row(row)
        return insert_sorted_bounded(acc, val, metric, 0)
    return fold_file(tabfile, cache, process_row)

def insert_sorted_bounded(xs, val, metric, i):
    """
        Insert `val` into the reverse-order sorted list `xs`.
        The function `metric` is the sorting function (<)
        (i.e, the greatest element is at the head of the list)
        Do not increase the length of `xs`.
    """
    if i == len(xs):
        # Done, ignore `val`
        return xs
    elif xs[i] is None:
        # List unpopulated, just overwrite
        xs[i] = val
        return xs
    elif metric(xs[i], val):
        # `val` beats current list element,
        # replace and push current element back.
        tmp = xs[i]
        xs[i] = val
        val = tmp
    return insert_sorted_bounded(xs, val, metric, i+1)

def results_of_tab(tabfile, dgraph):
    """ (-> Path-String GraphDict Result)
        Input: a .tab file, an overall summary of running all configurations
               of a project.
        Output: a Result object.
    """
    fname = strip_suffix(tabfile).rsplit("/", 1)[-1]
    num_modules = count_modules(tabfile)
    num_configs = 2 ** num_modules
    print("Project contains %s modules (%s configurations)" % (num_modules, num_configs))
    u_raw = all_cells_matching(tabfile, is_untyped_config)
    g_raw = all_cells_matching(tabfile, lambda x: not (is_typed_config(x) or is_untyped_config(x)))
    t_raw = all_cells_matching(tabfile, is_typed_config)
    ugt_violin = violin_plot([u_raw, g_raw, t_raw]
                            ,"%s_untyped-vs-gradual-vs-typed" % fname
                            ,"Configuration"
                            ,"Runtime (ms)"
                            ,xlabels=["untyped","gradual\n(all configs)","typed"]) if u_raw and g_raw and t_raw else None
    # Collect absolute BEST and WORST times+configs
    ten_percent = max(3, min(10, int(0.10 * num_configs)))
    best_cfg_and_times  = best_rows(tabfile
                        ,ten_percent
                        ,lambda acc,tmp: tmp[1] < acc[1]
                        ,lambda row:(row[0], int(statistics.mean([int(x) for x in row[1::]]))))
    worst_cfg_and_times = best_rows(tabfile
                        ,ten_percent
                        ,lambda acc,tmp: acc[1] < tmp[1]
                        ,lambda row:(row[0], int(statistics.mean([int(x) for x in row[1::]]))))
    stats = {
        "title"    : fname
        ,"runs"    : count_runs(tabfile)
        ,"graph"   : dgraph
        ,"ugt"     : {"img" : ugt_violin
                     ,"summary" : {"untyped" : basic_row_stats(u_raw)
                                  ,"gradual" : basic_row_stats(g_raw)
                                  ,"typed"   : basic_row_stats(t_raw)}}
        ,"best"    : [basic_config_stats(v[0], v[1], dgraph)
                      for v in best_cfg_and_times if v is not None]
        ,"worst"   : [basic_config_stats(v[0], v[1], dgraph)
                      for v in worst_cfg_and_times if v is not None]
        ,"bucketed": violin_plot(bucketize(tabfile, num_typed_modules, num_modules)
                                ,"%s_by-typed-modules" % fname
                                ,"Number of Typed Modules"
                                ,"Runtime (ms)"
                                ,positions=range(0, 1+num_modules)
                                ,xlabels=range(0, 1+num_modules))
        ,"fixed"   : [double_violin_plot([bucketize(tabfile, num_typed_modules, num_modules, pred=lambda cfg: cfg[v[0]] == "0")
                                         ,bucketize(tabfile, num_typed_modules, num_modules, pred=lambda cfg: cfg[v[0]] == "1")]
                                 ,"%s_fixing-%s" % (fname, k)
                                 ,"Number of Typed Modules"
                                 ,"Runtime (ms)"
                                 ,legend=["%s is untyped" % k
                                         ,"%s is typed" % k])
                      for (k,v) in sorted(dgraph.items(), key=lambda item:item[1][0])]
    }
    return stats

def results_of_sampling(dname, graph):
    # Emulate the `results_of_tabfile` function using simple random sampling
    # TODO:
    # - run setup.rkt
    # - use run.rkt, instead of the solutions in this file
    # - execute sample (get up to steady state, etc)
    raise NotImplementedError('nope')

def percent_diff(n1, n2):
    """ (-> Nat Nat (List Nat String))
       Percent-difference between `n1` and `n2`.
       Think of `n1` as the observed value and `n2` as the expected.
    """
    val = round(n1 / n2, 2)
    if val >= 1:
        descr = "slower"
    else:
        descr = "faster"
    return val, descr

def pretty_print(results):
    """ (-> Result Void)
        Given a Result object, pretty print summary information
        to console.
    """
    print("RESULTS:")
    u_summ = results["ugt"]["summary"]["untyped"]
    print("Average runtime of untyped configuration: %s\n  (min: %s, max: %s, 95%%-confidence: [%s,%s])" % (u_summ["mean"], u_summ["min"], u_summ["max"], u_summ["ci"][0], u_summ["ci"][1]))
    t_summ = results["ugt"]["summary"]["typed"]
    print("Average runtime of typed configuration: %s\n  (min: %s, max: %s, 95%%-confidence: [%s,%s])" % (t_summ["mean"], t_summ["min"], t_summ["max"], t_summ["ci"][0], t_summ["ci"][1]))
    g_summ = results["ugt"]["summary"]["gradual"]
    g_vs_u, descr = percent_diff(g_summ["mean"], u_summ["mean"])
    print("Average gradually-typed runtimes are %sx %s than untyped average." % (g_vs_u, descr))
    best_vs_u, descr = percent_diff(results["best"][0]["time"], u_summ["mean"])
    print("Best observed gradual runtime is %sx %s than untyped average. Observed on configuration %s" % (best_vs_u, descr, results["best"][0]["id"]))
    worst_vs_u, descr = percent_diff(results["worst"][0]["time"], u_summ["mean"])
    print("Worst observed gradual runtime is %sx %s than untyped average. Observed on configuration %s" % (worst_vs_u, descr, results["worst"][0]["id"]))
    bavg = int(statistics.mean([x["time"] for x in results["best"]]))
    bavg_vs_u, descr = percent_diff(bavg, u_summ["mean"])
    print("Average of top %d gradual running times is %sx %s than untyped average." % (len(results["best"]), bavg_vs_u, descr))
    wavg = int(statistics.mean([x["time"] for x in results["worst"]]))
    wavg_vs_u, descr = percent_diff(wavg, u_summ["mean"])
    print("Average of worst %s gradual running times is %sx %s than untyped average." % (len(results['worst']), wavg_vs_u, descr))
    print("The worst %s configurations and their boundaries are (in order of badness):" % (len(results["worst"])))
    for bad_cfg in results["worst"]:
        vs_u, descr = percent_diff(bad_cfg["time"], u_summ["mean"])
        edges_str = "\n    ".join(("(%s =(require)=> %s)" % (src,dst) for (src,dst) in bad_cfg["boundaries"]))
        print("%s (%sx %s than untyped avg.):\n    %s" % (bad_cfg["id"], vs_u, descr, edges_str))
    return

def save_as_tex(results, outfile):
    """ (-> Result Path-String Void)
        Given an Result object, save results to a nicely-formatted .tex file.
    """
    untyped = results["ugt"]["summary"]["untyped"]
    typed   = results["ugt"]["summary"]["typed"]
    gradual = results["ugt"]["summary"]["gradual"]
    with open(outfile, "w") as f:
        def render(s):
            print(s, file=f)
        render("\\documentclass{article}")
        render("\\usepackage{graphicx,enumitem}")
        render("\\newcommand{\\mono}[1]{\\texttt{#1}}")
        render("\\begin{document}")
        render("\\setlist[enumerate,1]{start=0}")
        render("\n\n\\section{Results: %s}" % results["title"])
        render("\n\\subsection{Module Summary}")
        render("\\begin{enumerate}\n\\item \\mono{%s}\\end{enumerate}" % "}\n\item \mono{".join([k for (k,v) in sorted([(k,v[0]) for (k,v) in results["graph"].items()])]))
        render("Total of %s configurations" % (2 ** (len(results["graph"].keys()))))
        render("Ran each configuration %s times" % results["runs"])
        render("\n\\subsection{Overall Runtimes}")
        render("\\begin{itemize}")
        render("\\item Average \\emph{untyped} runtime: %s" % untyped["mean"])
        render("  \\begin{itemize}\n  \\item Median: %s\n  \\item Min: %s\n  \\item Max: %s\n  \\item 95\\%% confidence: %s~\\textendash~%s\n  \\end{itemize}" % (untyped["median"], untyped["min"], untyped["max"], untyped["ci"][0], untyped["ci"][1]))
        t_vs_u, t_vs_u_descr = percent_diff(typed['mean'], untyped['mean'])
        render("\\item Average \\emph{typed} runtime: %s (%s times %s than untyped average)" % (typed["mean"], t_vs_u, t_vs_u_descr))
        render("  \\begin{itemize}\n  \\item Median: %s\n  \\item Min: %s\n  \\item Max: %s\n  \\item 95\\%% confidence: %s~\\textendash~%s\n  \\end{itemize}" % (typed["median"], typed["min"], typed["max"], typed["ci"][0], typed["ci"][1]))
        if not gradual:
          render("\\end{itemize}")
        else:
          g_vs_u = percent_diff(gradual["mean"], untyped["mean"])
          render("\\item Average gradually-typed runtime is %s times %s than untyped average." % g_vs_u)
          render("\\item Best gradually-typed runtime is %s times %s than untyped average." % percent_diff(results["best"][0]["time"], untyped["mean"]))
          render("\\begin{itemize}\\item Configuration: \\mono{%s}\\end{itemize}" % results["best"][0]["id"])
          render("\\item Worst gradually-typed runtime is %s times %s than untyped average." % percent_diff(results["worst"][0]["time"], untyped["mean"]))
          render("\\begin{itemize}\\item Configuration: \\mono{%s}\\end{itemize}" % results["worst"][0]["id"])
          bavg_vs_u = percent_diff(statistics.mean([x["time"] for x in results["best"]]), untyped["mean"])
          render("\\item Average of top %s gradually-typed configurations is %s times %s than untyped average" % (len(results["best"]), bavg_vs_u[0], bavg_vs_u[1]))
          wavg_vs_u = percent_diff(statistics.mean([x["time"] for x in results["worst"]]), untyped["mean"])
          render("\\item Average of bottom %s gradually-typed configurations is %s times %s than untyped average" % (len(results["worst"]), wavg_vs_u[0], wavg_vs_u[1]))
          render("\\end{itemize}")
          render("\n\\subsection{Aggregate Figures}")
          bar = bar_plot(range(5)
                        ,[1, g_vs_u[0], bavg_vs_u[0], wavg_vs_u[0], percent_diff(typed["mean"], untyped["mean"])[0]]
                        ,"%s-normalized-runtimes" % results["title"]
                        ,"Group"
                        ,"Runtime (Normalized to untyped)"
                        ,xlabels=["Untyped", "All\nGradually-Typed", "Top %s" % (len(results["best"])), "Bottom %s" % (len(results["worst"])), "Typed"])
          render("\\includegraphics[width=\\textwidth]{%s}" % bar)
          render("\\includegraphics[width=\\textwidth]{%s}" % results["ugt"]["img"])
          render("\\includegraphics[width=\\textwidth]{%s}" % results["bucketed"])
          render("\n\\subsection{Worst (gradual) Configurations}")
          render("The worst %s configurations and their boundaries are:" % len(results["worst"]))
          render("\\begin{itemize}")
          for bad_c in results["worst"]:
              vs_u, descr = percent_diff(bad_c["time"], untyped["mean"])
              edges_str = "  \\begin{itemize}\n  \\item %s\n  \\end{itemize}" % "\n  \\item ".join(("(\\mono{%s} $\\rightarrow$ \\mono{%s})" % (src,dst) for (src,dst) in bad_c["boundaries"]))
              render("\\item %s (%s times %s than untyped average)\n%s" % (bad_c["id"], vs_u, descr, edges_str))
          render("\\end{itemize}")
          num_mg_figs = min(5, len(results["worst"]))
          render("\n\\subsection{Top %s Worst (gradual) Configurations}" % num_mg_figs)
          render("Untyped modules are \\textbf{blue} and typed modules are \\emph{orange}.\n")
          for i in range(num_mg_figs):
              config = results["worst"][i]["id"]
              time   = results["worst"][i]["time"]
              bnds   = results["worst"][i]["boundaries"]
              vs_u, descr = percent_diff(time, untyped["mean"])
              fname = module_plot(results["graph"]
                                 ,results["title"]
                                 ,"Config %s: %s times %s than untyped" % (config, vs_u, descr)
                                 ,boundaries=bnds)
              render("\\includegraphics[width=\\textwidth]{%s}" % fname)
          render("\n\\subsection{Best (gradual) Configurations}")
          render("The best %s configurations and their boundaries are:" % len(results["best"]))
          render("\\begin{itemize}")
          for bad_c in results["best"]:
              vs_u, descr = percent_diff(bad_c["time"], untyped["mean"])
              edges_str = "  \\begin{itemize}\n  \\item %s\n  \\end{itemize}" % "\n  \\item ".join(("(\\mono{%s} $\\rightarrow$ \\mono{%s})" % (src,dst) for (src,dst) in bad_c["boundaries"]))
              render("\\item %s (%s times %s than untyped average)\n%s" % (bad_c["id"], vs_u, descr, edges_str))
          render("\\end{itemize}")
          num_mg_figs = min(5, len(results["best"]))
          render("\n\\subsection{Top %s Best (gradual) Configurations}" % num_mg_figs)
          render("Untyped modules are \\textbf{blue} and typed modules are \\emph{orange}.\n")
          for i in range(num_mg_figs):
              config = results["best"][i]["id"]
              time   = results["best"][i]["time"]
              bnds   = results["best"][i]["boundaries"]
              vs_u, descr = percent_diff(time, untyped["mean"])
              fname = module_plot(results["graph"]
                                 ,results["title"]
                                 ,"Config %s: %s times %s than untyped" % (config, vs_u, descr)
                                 ,boundaries=bnds)
              render("\\includegraphics[width=\\textwidth]{%s}" % fname)
          if "fixed" in results and results["fixed"]:
              render("\n\\subsection{Fixing individual modules}")
              for fig in results["fixed"]:
                  render("\\includegraphics[width=\\textwidth]{%s}" % fig)
        render("\\end{document}")
    print("Results saved as %s" % outfile)
    return

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
        tabfile = tabfile_of_rktd(args[0])
        results = results_of_tab(tabfile, args[1])
    elif len(args) == 2 and args[0].endswith(".tab"):
        # Collect results from the .tab file
        results = results_of_tab(args[0], args[1])
    elif len(args) == 2 and os.path.isdir(args[0]):
        # Sampling mode!
        check_directory_structure(args[0])
        results = results_of_sampling(args[0], args[1])
    else:
        raise ValueError("unexpected arguments '%s'" % str(args))
    # pretty_print(results)
    save_as_tex(results, "%s.tex" % strip_suffix(args[0]).rsplit("/", 1)[-1])
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

