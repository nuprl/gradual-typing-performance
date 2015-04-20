"""
Generic utility functions
"""

import constants

def fold_file(fname, acc, fn, ignore_first_line=True):
    """
        Iterate over lines in `fname`,
        apply `fn` to `acc` and line at each step,
        Return the generated accumulator.
    """
    with open(fname, "r") as f:
        if ignore_first_line:
            next(f)
        for line in f:
            acc = fn(acc, line.strip().split(constants.SEP))
    return acc

def strip_suffix(fname):
    """
        Remove everything after the rightmost "." in the string `fname`
    """
    return fname.rsplit(".", 1)[0]

