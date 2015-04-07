"""
Create a module-dependence graph (from the input) and a few pictures.

Input should be two files
- a .tab file of running-time data
- a .graph file with columns MODULE  INDEX  RUNTIME

Checklist:
- [X] create one module graph
- [ ] color edges by num unacceptable configs active in
- [ ] label edges by avg, med runtime
- [ ] repeat for all files
- [ ] try representing / identifying clusters
"""

import util
import statistics
import sys
import networkx as nx
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
import matplotlib.pyplot as plt

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
    len_ok = len(values) == 3
    if not len_ok:
        raise ValueError("expected 3 columns in row, got %d columns" % len(values))
    module_name = values[0]
    if not module_name.endswith(".rkt"):
        raise ValueError("expected module name to end with '.rkt', instead got '%s'" % module_name)
    index = None
    try:
        index = int(values[1])
    except ValueError:
        raise ValueError("expected integer INDEX, got %s" % values[1])
    requires = values[2].split(",")
    if not (all((rq.endswith(".rkt") for rq in requires))):
        raise ValueError("expected each REQUIRE to be a .rkt filename, instead got '%s'" % requires)
    return [module_name, index, requires]

### core functions

def gen_unacceptable(fname):
    min_time = None
    with open(fname, "r") as f:
        next(f)
        for line in f:
            data = line.strip().split(SEP)[1::]
            runtime = statistics.mean(data)
            min_time = min(min_time, runtime)
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
    # aggregating these values with `agg_f`
    raise NotImplementedError("oh noes")

def save_graph(g, fname, tag, edge_labels=None):
    new_name = util.gen_name(fname, tag, "png")
    plt.title(new_name.rsplit(".", 1)[0].rsplit("/", 1)[-1])
    pos = nx.spring_layout(g)
    # pos = nx.spectral_layout(g)
    nx.draw_networkx_nodes(g, pos, node_color="b", node_size=1000, alpha=0.6)
    nx.draw_networkx_edges(g, pos, width=1, alpha=0.5)
    labels = dict(( (str(node), str(node) ) for node in g.nodes_iter()))
    nx.draw_networkx_labels(g, pos, labels)
    if edge_labels is not None:
        mx.draw_networkx_edge_labels(g, pos, edge_labels)
    plt.axis("off")
    plt.savefig(new_name)
    print("Saved graph to '%s'" % new_name)
    return new_name

def main(fname, graph_name):
    # Create a module graph,
    # Build and save figures.
    g = module_graph_init(graph_name)
    save_graph(g,fname,"modules")
    # el_mean = edge_labels_by_runtime(g, fname, statistics.mean)
    # save_graph(g,fname,"edges-avg", el_mean)
    # el_median = edge_labels_by_runtime(g, fname, statistics.median)
    # save_graph(g,fname,"edges-med", el_median)
    # unacceptable = gen_unacceptable(fname)
    # el_badconfs = edge_labels_by_runtime(g, fname, (lambda xs: sum((1 for x in xs if unacceptable(x)))))
    # save_graph(g, fname, el_badconfs)
    return None

if __name__ == "__main__":
    if len(sys.argv) == 3 and sys.argv[1].endswith(".tab") and sys.argv[2].endswith(".graph"):
        main(sys.argv[1], sys.argv[2])
    else:
        print("Usage: module-graph.py FILE.tab FILE.graph")

