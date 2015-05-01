import os

### Private
SEP = "\t"

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
    #if not(bool(requires)):
    #    print("WARNING: module '%s' has no requires" % values[0])
    return [module_name, index, requires]

### Public

# (-> String String String String)
def gen_name(fname, tag, suffix):
    fname = os.path.basename(fname)
    # Cut original suffix from `fname`, append `tag`, a dot, and `suffix`
    return "%s-%s.%s" % (os.path.splitext(fname)[0], tag, suffix)

# (-> Path-String Nat)
def count_lines(fname):
    # Count the number of lines in a file
    count = 0
    with open(fname, "r") as f:
        for line in f:
            count += 1
    return count

# (-> Path-String Nat)
def count_modules(fname):
    # Return the number of modules in the project that
    # the '.tab' file `fname` reports on
    with open(fname, "r") as f:
        _     = next(f)
        row   = next(f)
        title = row.split(maxsplit=1)[0]
    return len(title)

# (-> Path-String Dict)
def dict_of_file(gname):
    # Generate a dictionary-graph from a .graph file
    # Keys are module names, like "a.rkt"
    # Values are pairs of indices and requires, like (3, [x.rkt, y.rkt])
    d = {}
    with open(gname, "r") as f:
        _check_colnames(next(f).strip().split(), gname)
        for line in f:
            [mname, i, requires] = _check_col(line.strip().split())
            d[mname] = (int(i), requires)
    return d

# (-> String (List Nat Nat) Boolean)
def is_boundary(config, edge):
    # True if (i,j) is a typed/untyped boundary edge
    return config[edge[0]] != config[edge[1]]

def infer_graph(fname):
    gfile1 = "%s.graph" % fname.rsplit(".", 1)[0]
    gfile2 = "%s.graph" % fname.rsplit(".", 1)[0].split("-", 1)[0]
    if os.path.exists(gfile1):
        return gfile1
    elif os.path.exists(gfile2):
        return gfile2
    else:
        return None

def infer_module_names(fname, *args):
    # Try finding a .graph file near the filename,
    # if so, return a name in place of each argument index.
    # if not, print a warning and return the arguments
    gfile = infer_graph(fname)
    if gfile is None:
        print("Warning: could not find .graph file")
        return args
    d = dict_of_file(gfile)
    name_of_index = {}
    for (k, (i, rqs)) in d.items():
        name_of_index[i] = k
    return [name_of_index[index].rsplit(".", 1)[0] for index in args]

# (-> Path-String (U None (Listof (List Nat Nat))))
def infer_edges(fname):
    # Try to return a list of all edges (by indices) in the file
    gfile = infer_graph(fname)
    if gfile is None:
        return None
    d = dict_of_file(gfile)
    # Collect indices of edges
    return [(d[mod][0], d[req][0])
            for mod in d.keys()
            for req in d[mod][1]
            if req in d]

