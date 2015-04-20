"""
Utilities for manipulating configuration bitstrings.

A 'configuration bitstring' is a string like "0100".
Each index refers to a module in the project.
A zero "0" indicates that module is untyped; a "1" means it is typed.
"""

import graphdict

def untyped_at(cfg, key):
    return cfg[key] == "0"

def typed_at(cfg, key):
    return cfg[key] == "1"

def is_untyped(cfg):
    """
        True if `cfg` is the fully-UNtyped configuration.
    """
    return all((c == "0" for c in cfg))

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
        Use GraphDict `graph` to identify edges.
    """
    bds = []
    for (m1, m2) in graphdict.edges_iter(graph):
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
