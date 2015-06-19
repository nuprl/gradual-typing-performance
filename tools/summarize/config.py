import random

"""
Utilities for manipulating configuration bitstrings.

A 'configuration bitstring' is a string like "0100".
Each index refers to a module in the project.
A zero "0" indicates that module is untyped; a "1" means it is typed.
"""

def untyped_at(cfg, key):
    return cfg[key] == "0"

def typed_at(cfg, key):
    return cfg[key] == "1"

def has_typed_modules(n):
    return (lambda cfg: num_typed_modules(cfg) == n)

def is_untyped(cfg):
    """
        True if `cfg` is the fully-UNtyped configuration.
    """
    return all((c == "0" for c in cfg))

def is_gradual(cfg):
    """
        True if `cfg` is neither fully typed or fully untyped.
    """
    return not(is_untyped(cfg) or is_typed(cfg))

def is_typed(cfg):
    """
        True if `cfg` is the fully-typed configuration.
    """
    return all((c == "1" for c in cfg))

def num_typed_modules(cfg):
    """
        Count the number of typed modules in the configuration
        represented by `cfg`.
        Examples:
        - 0010, 0100, 1000 all have 1 typed module
        - 1111             has 4 typed modules
    """
    return sum((1 for c in cfg if c == "1"))

def is_boundary(cfg, i, j):
    """ (-> String (List Nat Nat) Boolean)
        True if (i,j) is a typed/untyped boundary edge
    """
    return cfg[i] != cfg[j]

def modules_of_graph(cfg, graph):
    """
        Given a configuration bitstring and a GraphDict `graph`,
        return the module names (in-order) corresponding to indices
        in the string.
    """
    # Transform dict, so index keys to a module name (drop requires)
    name_of_index = dict([(v[0],k) for (k,v) in graph.items()])
    return [name_of_index[i] for i in range(len(cfg))]

def boundaries(cfg, graph):
    """
        Return a list of boundary edges in the bitstring `cfg`.
        Use ModuleGraph `graph` to identify edges.
    """
    bds = []
    for (m1, m2) in graph.edges_iter():
        i1 = graph[m1][0]
        i2 = graph[m2][0]
        if is_boundary(cfg, i1, i2):
            bds.append((m1, m2))
    return bds

def basic_stats(cfg, time, graph):
    """
        Basic summary information for a configuration bitstring
    """
    return {"id"         : cfg
           ,"boundaries" : boundaries(cfg, graph)
           ,"time"       : time
           }

def abstract_iter(cfg, fuel, elem_pred, swap_with):
    """
        Abstracts common structure from `previous_iter` and `next_iter`.
        Sorry there's no better name right now.
        Args:
        - cfg : a configuration bitstring
        - fuel : max recursion depth
        - elem_pred : a test on elements of the configuration
        - swap_with : replacement for config elements. Should be '1' or '0'.
                      (If there's a use for something else, this whole file
                       should generalize to suit)
    """
    if fuel > 0:
        for i in range(0, len(cfg)):
            if elem_pred(cfg[i]):
                # A little awkward because strings are immutable
                cfg_2 = "".join((cfg[j] if j != i else swap_with
                                 for j in range(0, len(cfg))))
                yield cfg_2
                # Yield all recursive results
                yield from abstract_iter(cfg_2, (fuel - 1), elem_pred, swap_with)

def previous_iter(cfg, fuel):
    """
        Generate all configurations with `fuel` or fewer typed modules
        than the argument `cfg`.
    """
    return abstract_iter(cfg, fuel, (lambda x: x == "1"), "0")

def next_iter(cfg, fuel):
    """
        Generate all configurations with `fuel` or MORE typed modules
        than the argument.
    """
    return abstract_iter(cfg, fuel, (lambda x: x == "0"), "1")

def random_walk(cfg, transitivity=1):
    """
        Generate a random walk from this configuration
        to the fully-typed config.
    """
    path = [cfg]
    while not(is_typed(cfg)):
        # Convert to set to remove duplicates
        next_list = list(set(next_iter(cfg, transitivity)))
        cfg = next_list[random.randint(0, len(next_list)-1)]
        path.append(cfg)
    return path

def minus(cfg1, cfg2):
    """
        Subtract the second config from the first, pointwise.
        Return -1 in case subtraction is invalid
         and the sum of the remaining bits otherwise

        Examples!
          1111 - 1100 = 2
          0100 - 0100 = 0
          1010 - 0101 = -1
          0000 - 1000 = -1
    """
    total = 0
    for i in range(0, len(cfg1)):
        if cfg1[i] == "1":
            # Do nothing if cfg2[i] == 1
            if cfg2[i] == "0":
                total += 1
        # cfg1[i] == 0, do nothing if cfg2[i] is also 0
        elif cfg2[i] == "1":
            return -1
    return total

