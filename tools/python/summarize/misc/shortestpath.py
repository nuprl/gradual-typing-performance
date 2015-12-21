""" shortest-path.py
Run a shortest path algorithm on the lattice.
Try to make beautiful output.
"""
import util
import statistics
import sys
import networkx as nx
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
import matplotlib.pyplot as plt

# (-> String (Listof String))
def prev_keys(key):
    # `key` is a bitstring
    # Return all bitstrings reachable by decrementing any
    # single 1 in `key`.
    prevs = []
    for i in range(0, len(key)):
        if key[i] == '1':
            prevs.append("".join((key[j] if j != i else '0' for j in range(0, len(key)))))
    return prevs

# (-> Path-String Graph)
def graph_of_file(fname):
    # Read `fname` as a networkx graph
    # Edge weights are the avg. runtime of target config
    g = nx.DiGraph()
    # Add all nodes
    with open(fname, "r") as f:
        next(f)
        for line in f:
            title = line.strip().split(maxsplit=1)[0]
            g.add_node(title)#, key=title)
    # Add edges
    with open(fname, "r") as f:
        next(f)
        for line in f:
            data = line.strip().split()
            key = data[0]
            mean = statistics.mean([int(x) for x in data[1:]])
            for key2 in prev_keys(key):
                g.add_edge(key2, key, weight=mean)
    # Get the first and last nodes, using that left-over `title` variable
    zerod = "".join(("0" for _ in title))
    oned = "".join(("1" for _ in title))
    return g, zerod, oned

# (-> String (Dict String (List Nat Nat)))
def positions_of_file(fname):
    # Build up a dictionary of "bitstring->(x,y)"
    # x position = among configs on same level, 0 1 -1 2 -2 3 ...
    # y position = number of typed modules
    posn = {}
    i_level = {} #index into current level
    voffset = None #vertical offset, number of bits in our strings
    max_x = 0
    # Walk over the file to get all keys
    with open(fname, "r") as f:
        next(f)
        for line in f:
            key = line.strip().split(maxsplit=1)[0]
            num_ones = sum((1 for c in key if c == "1"))
            # Init table entry, if not already
            if num_ones not in i_level:
                i_level[num_ones] = 0
            else:
                non_pos = int(i_level[num_ones] <= 0)
                i_level[num_ones] = (-1 * i_level[num_ones]) + non_pos
                max_x = max(max_x, i_level[num_ones])
            posn[key] = (i_level[num_ones], 2 * len(key) * num_ones)
    return posn, (max_x + len(key))

# (-> String Graph (Listof Edge) Node String)
def save_path(fname, g, path):
    ## Save a picture of the graph g, with `path` highlighted
    new_name = util.gen_name(fname, "dijkstra", "png")
    positions, max_x = positions_of_file(fname)
    p = dict([(n, positions[n]) for n in g.nodes()])
    ## Add labels
    title_str = fname.rsplit(".", 1)[0].rsplit("/", 1)[-1]
    plt.title("%s\n\n%s"
      % (title_str
        ,"\n\n".join(path[::-1]))
      , loc="left", position=(0,0))
    keylen = len(str(path[0]))
    ## Draw graph
    pos = nx.spring_layout(g, pos=p, fixed=g.nodes())
    nx.draw(g, pos, node_color="k", font_size=80)
    ## draw path in RED
    path_edges = [x for x in zip(path, path[1:])]
    nx.draw_networkx_nodes(g, pos, nodelist=path, node_color="r")
    nx.draw_networkx_edges(g, pos, edgelist=path_edges, edge_color="r", width=2, arrows=False)
    ## Save plot
    plt.axis('equal')
    plt.savefig(new_name)
    plt.clf()
    print("Saved graph to '%s'" % new_name)
    return new_name

# (All (A) (-> (-> A Edge A) A (Listof Edge) Graph A))
def fold_edge_weights(f, acc, path, g):
    # For building measures: call `f` with accumulator and edge weight
    prev = None
    for node in path:
        if prev is not None:
            acc = f(acc, g.get_edge_data(prev, node)["weight"])
        prev = node
    return acc

# (-> (Listof Edge) Graph Nat)
def min_runtime(path, g):
    # Measure: Return the min of all weights in path
    init = g.get_edge_data(path[0], path[1])["weight"]
    return fold_edge_weights(min, init, path, g)

# (-> (Listof Edge) Graph Nat)
def max_runtime(path, g):
    # Measure: Return the max of all weights in path
    return fold_edge_weights(max, 0, path, g)

# (-> (Listof Edge) Graph Nat)
def sum_runtime(path,g):
    # Measure: Return the sum of all edge weights in path
    f = lambda x, y: x + y
    return fold_edge_weights(f, 0, path, g)

# (-> (-> (Sequenceof Edge)) Graph Path-String String Nat (-> (Listof Edge) Graph Nat) String)
def save_all_paths(gen_paths, g, fname, tag, num_bins, measure):
    # Create a histogram of all paths in the graph, save it to `fname`
    plt.xlabel("Runtime (ms)")
    plt.ylabel("Num. Paths")
    plt.title("%s-%s" % (fname.rsplit(".", 1)[0].rsplit("/", 1)[-1], tag))
    gen_runtimes = lambda: (measure(p,g) for p in gen_paths())
    min_runtime = min(gen_runtimes())
    max_runtime = max(gen_runtimes())
    med_runtime = statistics.median(gen_runtimes())
    bkt_width = (max_runtime - min_runtime) / num_bins
    bins = ((min_runtime + int(i * bkt_width), min_runtime + int((1 + i) * bkt_width)) for i in range(0, num_bins))
    for (lo, hi) in bins:
        plt.bar(lo
               ,sum((1 for rt in gen_runtimes() if lo <= rt <= hi))
               ,alpha=0.6
               ,width=bkt_width)
    # Max, Min, Median path lines
    plt.axvline(med_runtime, color="k", linestyle="solid", linewidth=5, label="median = %s" % int(med_runtime))
    # invisible lines
    plt.axvline(min_runtime, alpha=0, color="0", linestyle="dashed", linewidth=0, label="min = %s" % int(min_runtime))
    plt.axvline(max_runtime, alpha=0, color="0", linestyle="dotted", linewidth=0, label="max = %s" % int(max_runtime))
    # Save figure
    new_name = util.gen_name(fname, tag, "png")
    lgd = plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
    plt.savefig(new_name,bbox_extra_artists=(lgd,), bbox_inches='tight')
    print("Saved plot to '%s'" % new_name)
    ## Save data
    save_runtimes(zip(gen_paths(), gen_runtimes()), util.gen_name(fname, tag, "tab"))
    plt.clf()
    return new_name

# (-> (Sequenceof (List (Sequenceof Edge) (Sequenceof Nat))) String String)
def save_runtimes(path_and_time, fname):
    # Save a spreadsheet of runtimes
    with open(fname, "w") as f:
        f.write("Measure\tPath\n")
        for (path, time) in path_and_time:
            f.write(str(int(time)))
            f.write("\t")
            f.write("\t".join(path))
            f.write("\n")
    print("Saved data to '%s'" % fname)
    return fname

def least_sum_path(fname):
    g, bot, top = graph_of_file(fname)
    return nx.dijkstra_path(g, bot, top)

# (-> String Void)
def main(fname):
    # Read fname as a networkx graph
    # Each key is a node, has edges to all keys +1 bit away
    # (00 -> 01 ; 00 -> 10 ; 10 -> 11 ; 01 -> 11)
    g, bot, top = graph_of_file(fname)
    ## Compute shortest path
    p1 = nx.dijkstra_path(g, bot, top)
    print("Shortest path (SUM) is: %s" % p1)
    save_path(fname, g, p1)
    ## Compute all paths
    gen_paths = lambda: nx.all_simple_paths(g, bot, top)
    for n in [20]:#5, 10, 15, 20]:
        save_all_paths(gen_paths, g, fname, "PATH-SUM-%s" % n, n, sum_runtime)
        #save_all_paths(gen_paths, g, fname, "PATH-MIN-%s" % n, n, min_runtime)
        save_all_paths(gen_paths, g, fname, "PATH-MAX-%s" % n, n, max_runtime)
    print("All done")

if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        print("Usage: shortest-path.py TAB-FILE")
