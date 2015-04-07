"""
Create a module-dependence graph (from the input) and a few pictures.
"""

def module_graph_init(fname):
    # Input should be a tab-separated file with 3 columns:
    #     MODULE	INDEX	REQUIRES
    # MODULE is a string filename, like `main.rkt`
    # INDEX is a natural number; the file's position in `.rktd` bitstrings (from left, zero-indexed)
    #   For example, the module with index "0" is the sole typed module in the bitstring "100"
    # REQUIRES is a comma-separated list of filenames, like `foo.rkt,bar.rkt`
    #   These are the files that `MODULE` requires. Used to put directed edges in the graph.
    return None

def main(fname):
    # Create a module graph,
    # Build and save figures.
    g = module_graph_init(fname)
    return None

if __name__ == "__main__":
    if len(sys.argv) == 2 and sys.argv[1].endswith(".graph"):
        main(sys.argv[1])
    else:
        print("Usage: module-graph.py FILE.graph")
