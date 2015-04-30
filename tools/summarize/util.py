"""
Generic utility functions
"""

import constants
import os

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
