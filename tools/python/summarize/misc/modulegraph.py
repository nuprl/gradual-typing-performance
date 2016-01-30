"""
Create a module-dependence graph (from the input) and a few pictures.

Input should be two files
- a .tab file of running-time data
- a .graph file with columns MODULE  INDEX  RUNTIME

"""

### imports

import util
import statistics
import sys
import networkx as nx
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
import matplotlib.pyplot as plt

### constants

# Factor away from minumum runtime a path must be to be considered "unacceptably bad"
UNACCEPTABLE = 20
# Separator for input files
SEP = "\t"

### "type" checking

def _check_colnames(col_names, fname):
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

def _check_col(values):
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
    if not(bool(requires)):
        print("WARNING: module '%s' has no requires" % values[0])
    return [module_name, index, requires]

# (-> Path-String Nat)
def max_runtime(fname):
    max_time = 0
    with open(fname, "r") as f:
        next(f)
        for line in f:
            data = [int(x) for x in line.strip().split(SEP)[1::]]
            runtime = statistics.mean(data)
            max_time = max(max_time, runtime)
    return max_time

# (-> Path-String Nat)
def min_runtime(fname):
    min_time = None
    with open(fname, "r") as f:
        next(f)
        for line in f:
            data = [int(x) for x in line.strip().split(SEP)[1::]]
            runtime = statistics.mean(data)
            min_time = runtime if min_time is None else min(min_time, runtime)
    return min_time

### core functions

def gen_unacceptable(fname):
    min_time = min_runtime(fname)
    very_bad_runtime = min_time * UNACCEPTABLE
    return (lambda x: x > very_bad_runtime)

def module_graph_init(fname):
    # Input should be a tab-separated file with 3 columns:
    #     MODULE	INDEX	REQUIRES
    # MODULE is a string filename, like `main.rkt`
    # INDEX is a natural number; the file's position in `.rktd` bitstrings (from left, zero-indexed)
    #   For example, the module with index "0" is the sole typed module in the bitstring "100"
    # REQUIRES is a comma-separated list of filenames, like `foo.rkt,bar.rkt`
    #   These are the files that `MODULE` requires. Used to put directed edges in the graph.
    g = nx.DiGraph()
    with open(fname, "r") as f:
        column_names = next(f).strip().split(SEP)
        _check_colnames(column_names,fname)
        for line in f:
            [mname, i, requires] = _check_col(line.strip().split(SEP))
            # add_node appends keywords if the node already exists
            g.add_node(mname, index=i)
            # add_edge implicitly creates nodes
            for rq in requires:
                g.add_edge(mname, rq)
    return g

def edge_labels_by_runtime(g, fname, agg_f):
    # Generate edge labels by collecting the runtimes
    # of all data samples where the edge is present, and
    # aggregating these values with `agg_f : (-> (Pairof (List Nat) (List Nat)) Nat)`
    # FIRST ARG to agg is "boundary times"
    # SECOND ARG is "no-boundary times"
    labels = {}
    for (u,v) in g.edges_iter():
        # Collect node's index, skip if there is no index
        if ('index' not in g.node[u]) or ('index' not in g.node[v]):
            # Skip base nodes (that are not in the bitstring)
            continue
        index1 = g.node[u]['index']
        index2 = g.node[v]['index']
        bnd_times = []
        nobnd_times = []
        # For each edge, go through the data file and collect all runtimes
        with open(fname, "r") as dataf:
            next(dataf)
            for line in dataf:
                rows = line.strip().split(SEP)
                title = rows[0]
                # If this edge is a typed/untyped boundary...
                if title[index1] != title[index2]:
                    bnd_times.append(statistics.mean([int(x) for x in rows[1::]]))
                else:
                    nobnd_times.append(statistics.mean([int(x) for x in rows[1::]]))
        labels[(u,v)] = agg_f(bnd_times, nobnd_times)
    return labels

def gen_positions(g, tag, edge_labels):
    # Generate positions for nodes in a graph
    # f_layout = nx.spring_layout
    # f_layout = nx.spectral_layout
    # pos = None
    # if edge_labels is not None:
    #     for (i,j), val in edge_labels.items():
    #         g.add_edge(i,j, tag = val)
    #     pos = f_layout(g, weight=tag)
    # else:
    #     pos = f_layout(g)
    pos = nx.circular_layout(g, scale=1)
    # pos = nx.random_layout(g)
    # pos = nx.shell_layout(g, scale=1)
    return pos

# def old_gen_widths(g, tag, edge_labels):
#     # Get max/min, each weight is the values % of max
#     min_w, max_w = None, None
#     for _,w in edge_labels.items():
#         min_w = min(min_w, w) if min_w else w
#         max_w = max(max_w, w) if max_w else w
#     edges = []
#     widths = []
#     for (i,j),w in edge_labels.items():
#         edges.append((i,j))
#         widths.append(int(100 * ((w - min_w) / (max_w - min_w))))
#     return edges, widths

def widths_by_pct(g, tag, edge_labels, f):
    widths = []
    for (i,j) in g.edges_iter():
        lbl = edge_labels.get((i,j), None)
        if lbl is not None:
            widths.append(f(lbl))
    return widths

def save_graph(g, fname, tag, edge_labels=None, edge_widths=None):
    new_name = util.gen_name(fname, tag, "png")
    plt.title(new_name.rsplit(".", 1)[0].rsplit("/", 1)[-1])
    pos = gen_positions(g, tag, edge_labels)
    ## draw nodes & labels
    nx.draw_networkx_nodes(g, pos, node_color="b", node_size=1000, alpha=0.6)
    node_labels = dict(( (str(node), str(node) ) for node in g.nodes_iter()))
    nx.draw_networkx_labels(g, pos, node_labels)
    ## draw edges (optionally with widths)
    if edge_widths is not None:
        nx.draw_networkx_edges(g, pos, edge_color='k', alpha=0.5, arrows=False, width=edge_widths)
    else:
        nx.draw_networkx_edges(g, pos, edge_color='k', alpha=0.5, arrows=False)
    ## draw edge labels
    if edge_labels is not None:
        nx.draw_networkx_edge_labels(g, pos, edge_labels, label_pos=0.2)
    ## Save figure
    plt.axis("off")
    plt.savefig(new_name)
    plt.clf()
    print("Saved graph to '%s'" % new_name)
    return new_name

def main(fname, graph_name):
    # Create a module graph, build & save figures
    g = module_graph_init(graph_name)
    min_time = min_runtime(fname)
    max_time = max_runtime(fname)
    print("min time is %d, max time is %d" % (min_time, max_time))
    ## Simple measures
    ### normal, boring module graph
    # save_graph(g,fname,"modules")
    ### Label edges by mean, median runtimes
    # tag = "edges-avg"
    # el_mean = edge_labels_by_runtime(g, fname, lambda: bs,nobs: statistics.mean(bs))
    # ew_mean = widths_by_pct(g, tag, el_mean, lambda x: int(x / min_time))
    # save_graph(g,fname, tag, el_mean, ew_mean)
    # el_median = edge_labels_by_runtime(g, fname, lambda bs,nobs: statistics.median(bs))
    # save_graph(g,fname,"edges-med", el_median)
    ###
    tag = "boundary+no-boundary=ratio"
    el_bnd = edge_labels_by_runtime(g, fname, lambda bnds,nobnds: "%s/%s = %s" % (int(statistics.mean(bnds)), int(statistics.mean(nobnds)), round(statistics.mean(bnds) / statistics.mean(nobnds), 2)))
    ew_bnd = widths_by_pct(g, tag, el_bnd, lambda x: 1)
    save_graph(g, fname, tag, el_bnd, ew_bnd)
    ### Label edges by number of 'unacceptable' configs they're in
    # unacceptable = gen_unacceptable(fname)
    # unaccept_measure = lambda xs: sum((1 for x in xs if unacceptable(x)))
    # tag = "edges-over-%d" % UNACCEPTABLE
    # el_badconfs = edge_labels_by_runtime(g, fname, unaccept_measure)
    # ew_badconfs = widths_by_pct(g, tag, el_badconfs, lambda x,_: x / util.count_lines(fname) -1)
    # save_graph(g, fname, tag, el_badconfs, ew_badconfs)
    ## More interesting measures: TODO
    return None

if __name__ == "__main__":
    if len(sys.argv) == 3 and sys.argv[1].endswith(".tab") and sys.argv[2].endswith(".graph"):
        main(sys.argv[1], sys.argv[2])
    else:
        print("Usage: modulegraph.py FILE.tab FILE.graph")

