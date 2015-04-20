"""
Parse and manipulate a dictionary representation of a module-dependence graph.

Data definition: GraphDict
- Keys are module names, like "a.rkt"
- Values are pairs of indices and required modules,
  like (3, [x.rkt, y.rkt])
"""

import os

def check_titles(col_names, fname):
    """
        Check that the given column titles are well-formed.
        (These titles probably came from a .graph file)
    """
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

def check_column(values):
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

def infer(fname):
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

def edges_iter(d):
    """
        Iterate over the "require" edges represented in
        the GraphDict `d`.
    """
    for (k, v) in d.items():
        for req in v[1]:
            yield(k, req)

def of_graphfile(gname):
    """ (-> Path-String GraphDict)
        Convert a .graph file to a GraphDict object
    """
    d = {}
    with open(gname, "r") as f:
        check_titles(next(f).strip().split(constants.SEP), gname)
        for line in f:
            [mname, i, requires] = check_column(line.strip().split(constants.SEP))
            d[mname] = (int(i), requires)
    return d

