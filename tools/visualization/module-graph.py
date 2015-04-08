"""
Create a module-dependence graph (from the input) and a few pictures.

Input should be two files
- a .tab file of running-time data
- a .graph file with columns MODULE  INDEX  RUNTIME

Checklist:
- [X] create one module graph
- [X] label edges by num unacceptable configs active in
- [X] label edges by avg, med runtime
- [X] repeat for all files
- [ ] more clever label representations
- [ ] typed vs untyped
- [ ] try representing / identifying clusters
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

### core functions

def gen_unacceptable(fname):
    min_time = None
    with open(fname, "r") as f:
        next(f)
        for line in f:
            data = [int(x) for x in line.strip().split(SEP)[1::]]
            runtime = statistics.mean(data)
            min_time = runtime if min_time is None else min(min_time, runtime)
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
    # aggregating these values with `agg_f : (-> (List Nat) Nat)`
    labels = {}
    for (u,v) in g.edges_iter():
        # Collect node's index, skip if there is no index
        if ('index' not in g.node[u]) or ('index' not in g.node[v]):
            continue
        index1 = g.node[u]['index']
        index2 = g.node[v]['index']
        runtimes = []
        # For each edge, go through the data file and collect all runtimes
        with open(fname, "r") as dataf:
            next(dataf)
            for line in dataf:
                rows = line.strip().split(SEP)
                title = rows[0]
                # If this edge is a typed/untyped boundary...
                if title[index1] != title[index2]:
                    runtimes.extend([int(x) for x in rows[1::]])
        labels[(u,v)] = int(agg_f(runtimes))
    return labels

def gen_positions(g, tag, edge_labels):
    # Generate positions for nodes in a graph
    # f_layout = nx.spring_layout
    f_layout = nx.spectral_layout
    pos = None
    if edge_labels is not None:
        for (i,j), val in edge_labels.items():
            g.add_edge(i,j, tag = val)
        pos = f_layout(g, weight=tag)
    else:
        pos = f_layout(g)
    return pos

def gen_widths(g, tag, edge_labels):
    # Return a dictionary of edges -> weights, or a natural
    if edge_labels is None:
        return 1
    # Get max/min, each weight is the values % of max
    min_w, max_w = None, None
    for (i,j),w in edge_labels.items():
        min_w = min(min_w, w) if min_w else w
        max_w = max(max_w, w) if max_w else w
    widths = {}
    for (i,j),w in edge_labels.items():
        widths[(i,j)] = int(100 * ((w - min_w) / (max_w - min_w)))
    return widths

def save_graph(g, fname, tag, edge_labels=None):
    new_name = util.gen_name(fname, tag, "png")
    plt.title(new_name.rsplit(".", 1)[0].rsplit("/", 1)[-1])
    pos = gen_positions(g, tag, edge_labels)
    nx.draw_networkx_nodes(g, pos, node_color="b", node_size=1000, alpha=0.6)
    # wds = gen_widths(g, tag, edge_labels)
    # keys, vals = [], []
    # for k,v in wds.items():
    #     keys.append(k)
    #     vals.append(v)
    # nx.draw(g, pos, edges=keys, weight=vals, alpha=0.5) # UH OH not associating edges with the right weights
    nx.draw_networkx_edges(g, pos, alpha=0.5)
    labels = dict(( (str(node), str(node) ) for node in g.nodes_iter()))
    nx.draw_networkx_labels(g, pos, labels)
    if edge_labels is not None:
        nx.draw_networkx_edge_labels(g, pos, edge_labels, label_pos=0.5)
    plt.axis("off")
    plt.savefig(new_name)
    plt.clf()
    print("Saved graph to '%s'" % new_name)
    return new_name

def main(fname, graph_name):
    # Create a module graph, build & save figures
    g = module_graph_init(graph_name)
    ## Simple measures
    # - normal, boring module graph
    # save_graph(g,fname,"modules")
    # - Label edges by mean, median runtimes
    el_mean = edge_labels_by_runtime(g, fname, statistics.mean)
    save_graph(g,fname,"edges-avg", el_mean)
    el_median = edge_labels_by_runtime(g, fname, statistics.median)
    save_graph(g,fname,"edges-med", el_median)
    # - Label edges by number of 'unacceptable' configs they're in
    # (seeming like a dead-end the number is just A LOT -- ah nvm, just need a better layout)
    unacceptable = gen_unacceptable(fname)
    unaccept_measure = lambda xs: sum((1 for x in xs if unacceptable(x)))
    el_badconfs = edge_labels_by_runtime(g, fname, unaccept_measure)
    save_graph(g, fname, "edges-over-%d" % UNACCEPTABLE, el_badconfs)
    ## More interesting measures: TODO
    return None

if __name__ == "__main__":
    if len(sys.argv) == 3 and sys.argv[1].endswith(".tab") and sys.argv[2].endswith(".graph"):
        main(sys.argv[1], sys.argv[2])
    else:
        print("Usage: module-graph.py FILE.tab FILE.graph")

