"""
Generic utility functions
"""

import constants
import math
import os
import statistics
import itertools

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

def strip_directory(fname):
    """
        Remove everything before the rightmost "/" in the string `fname`
    """
    return fname.rsplit("/", 1)[-1]

def contains_any_directories(dirname):
    """
        Returns true if the directory `dirname`
        contains any sub-directories
    """
    return any((True for name in os.listdir(dirname)
                if os.path.isdir(name)))

def get_files(dirname):
    """
        Return the files (not directories) contained in `dirname`
    """
    return (name for name in os.listdir(dirname) if os.path.isfile(name))

def count_files(dirname):
    """
        Return the number of files in the directory `dirname`
    """
    return sum((1 for name in os.listdir(dirname)
                if os.path.isfile(name)))

def stats_join(st1, st2):
    return stats_of_row(st1["raw"] + st2["raw"])

def stats_of_row(dataset):
    """ (-> (Listof Nat) (List Nat Nat Nat Nat)
        Compute basic statistics for list `dataset`
    """
    if not dataset:
        return None
    stat = {"raw"      : dataset
           ,"mean"     : int(statistics.mean(dataset))
           ,"median"   : int(statistics.median(dataset))
           ,"variance" : int(statistics.variance(dataset))
           ,"min"      : min(dataset)
           ,"max"      : max(dataset)
           }
    Z = 2.04 # Close to t-stat for 30 degrees of freedom (TODO, make less magic)
    delta = Z * (math.sqrt(stat["variance"]) / math.sqrt(len(dataset)))
    stat["ci"] = [int(stat["mean"] - delta), int(stat["mean"] + delta)]
    return stat

def sorted_buffer_insert(xs, val, metric, i):
    """
        Insert `val` into the reverse-order sorted list `xs`.
        The function `metric` is the sorting function (<)
        (i.e, the greatest element is at the head of the list)
        Do not increase the length of `xs`.
    """
    for i in range(i, len(xs)):
        if xs[i] is None:
            # List unpopulated, just overwrite
            xs[i] = val
        elif metric(xs[i], val):
            # `val` beats current list element,
            # replace and push current element back.
            tmp = xs[i]
            xs[i] = val
            val = tmp
    return

# for standard error of mean, use scipy.stats.sem
def sample_size95(width_pct):
    """
        Assuming an infinitely large population,
        return a sample size that will yield:
          We are (`confidence_pct`%) confident that the true
          mean is within (+/- [width_pct * sample_mean]) of the
          sample mean.
    """
    return ((1.96 ** 2) * 0.5 * 0.5) / (width_pct ** 2)

def pad(xs, default, length):
    return xs + ([default] * (length - len(xs)))

def partition(xs, f):
    """
        Partition the iterable `xs` into  lists matching and failing the predicate f.
    """
    t1, t2 = itertools.tee(xs)
    return filter(f, t1), itertools.filterfalse(f, t2)
