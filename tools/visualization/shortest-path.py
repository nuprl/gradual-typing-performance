""" shortest-path.py
Run a shortest path algorithm on the lattice.
Try to make beautiful output.
"""
import util
import statistics
import sys
import networkx as nx
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

def prev_keys(key):
    # `key` is a bitstring
    # Return all bitstrings reachable by decrementing any
    # single 1 in `key`.
    prevs = []
    for i in range(0, len(key)):
        if key[i] == '1':
            prevs.append("".join((key[j] if j != i else '0' for j in range(0, len(key)))))
    return prevs

def graph_of_file(fname):
    # Read `fname` as a networkx graph
    # Edge weights are the avg. runtime of target config
    g = nx.DiGraph()
    # Add all nodes
    with open(fname, "r") as f:
        next(f)
        for line in f:
            title = line.strip().split("\t", 1)[0]
            g.add_node(title)#, key=title)
    # Add edges
    with open(fname, "r") as f:
        next(f)
        for line in f:
            data = line.strip().split("\t")
            key = data[0]
            mean = statistics.mean([int(x) for x in data[1::]])
            for key2 in prev_keys(key):
                g.add_edge(key2, key, weight=mean)
    # Get the first and last nodes, using that left-over `title` variable
    zerod = "".join(("0" for _ in title))
    oned = "".join(("1" for _ in title))
    return g, zerod, oned


def count_ones(s):
    # Count the number of "1" in a bitstring
    return sum((1 for c in s if c == "1"))

def pos_of_str(s):
    return [num_swaps(s), count_ones(s)]

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
            key = line.strip().split("\t", 1)[0]
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

def save_path(fname, g, path, start):
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

def sum_runtime(path,g):
    # Return the sum of all edge weights in path
    total = 0
    prev = None
    for node in path:
        if prev is not None:
            total += g.get_edge_data(prev, node)["weight"]
        prev = node
    return total

def save_all_paths(all_paths, g, fname, tag):
    plt.xlabel("Path Runtime (sum)")
    plt.ylabel("Nothing")
    plt.title("All paths' Runtimes")
    xs,ys = map(list, zip(*[(sum_runtime(p,g), 1) for p in all_paths]))
    plt.bar(xs, ys, bottom=0)
    # Save figure
    new_name = util.gen_name(fname, tag, "png")
    plt.savefig(new_name)
    plt.clf()
    return new_name

def main(fname):
    # Read fname as a networkx graph
    # Each key is a node, has edges to all keys +1 bit away
    # (00 -> 01 ; 00 -> 10 ; 10 -> 11 ; 01 -> 11)
    g, bot, top = graph_of_file(fname)
    ## Compute shortest path
    p1 = nx.dijkstra_path(g, bot, top)
    # p2 = shortest_path(g, max, g)
    print("Shortest path is: %s" % p1)
    save_path(fname, g, p1, bot)
    ## Compute all paths
    # all_paths = nx.all_simple_paths(g, bot, top)
    # lo_weight = min((e["weight"] for e in g.edges_iter()))
    # hi_weight = max((e["weight"] for e in g.edges_iter()))
    # avg_weight = statistics.mean(
    # med_weight = TODO
    # print("Lowest path weight is: %s" % lo_weight)
    # print("Highest path weight is: %s" % hi_weight)
    # print("Avg. path weight is: %s" % avg_weight)
    # print("Median path weight is: %s" % med_weight)
    # print("Saved graph of paths to file '%s'" % save_all_paths(all_paths, g, fname, "paths"))

if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        print("Usage: shortest-path.py TAB-FILE")
