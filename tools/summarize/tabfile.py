"""
Parsing, creating, and computing statistics over .tab files.

This is the canonical file for reasoning about 'ground truth'
experiment results.
"""

import config
import math
import os
import sorted_buffer
import statistics
import util

def all_cells_matching(tabfile, config_pred):
    """
        Return a list of all data cells in rows whose titles
        match `config_pred`.
    """
    def process_row(row):
        title = row[0]
        if config_pred(title):
            return [int(x) for x in row[1::]]
        else:
            return []
    return fold_file(tabfile, [], lambda acc,row: acc + process_row(row))

def basic_row_stats(dataset):
    """ (-> (Listof Nat) (List Nat Nat Nat Nat)
        Compute basic statistics for list `dataset`
    """
    if not dataset:
        return None
    stat = {
        "mean"     : int(statistics.mean(dataset)),
        "median"   : int(statistics.median(dataset)),
        "variance" : int(statistics.variance(dataset)),
        "min"      : min(dataset),
        "max"      : max(dataset),
    }
    Z = 2.04 # Close to t-stat for 30 degrees of freedom (TODO, make less magic)
    delta = Z * (math.sqrt(stat["variance"]) / math.sqrt(len(dataset)))
    stat["ci"] = [int(stat["mean"] - delta), int(stat["mean"] + delta)]
    return stat

def best_rows(tabfile, limit, metric, val_of_row, init=None):
    """ (->* (Path-String (-> A A Boolean) (-> (Listof Nat) A)) (A) A)
        Return the "value" of the best row, according to `metric`.
        i.e. the row R such that for any other row r: metric(val_of_row(r), val_of_row(R)) = True
        - `tabfile` filename, a tab-separated file
        - `metric`  a < judgment on rows
        - `val_of_row` a pre-processing transformation on rows (used to filter, take mean, etc)
        - `init` optionally, a first value to compare to
    """
    cache = [None] * limit
    cache[0] = init
    def process_row(acc, row):
        # Get value from row.
        # Insert into the list `acc`
        if is_untyped_config(row[0]) or is_typed_config(row[0]):
            # Ignore fully typed / untyped configs
            return acc
        val = val_of_row(row)
        return insert_sorted_bounded(acc, val, metric, 0)
    return fold_file(tabfile, cache, process_row)

def bucketize(fname, get_bkt, num_buckets, pred=None):
    """
        Group the rows in `fname` into buckets,
        using `get_bkt` to assign a row of the file to a bucket.
        Return the collected list of lists
        (todo? optionally allow 'edit_row' function,
          instead of mushing all values into a list)
    """
    init = [[] for _ in range(1+num_buckets)]
    def add_row(acc, row):
        # TODO don't graph all points because ... (I had a reason at some point)
        if (pred is None) or (pred(row[0])):
            acc[get_bkt(row[0])].extend([int(x) for x in row[1::]])
        return acc
    return util.fold_file(fname, init, add_row)

def count_modules(fname):
    """ (-> Path-String Nat)
        Count the number of modules in the project.
    """
    with open(fname, "r") as f:
        _     = next(f)
        row   = next(f)
        title = row.split(constants.SEP, 1)[0]
    return len(title)

def count_runs(fname):
    """
        Count the number of runs recorded for each configuration in
        `fname`.

        2015-04-20: Warning, the count is not 'exact'.
          It trusts the title row, so if any data row has fewer
          columns than the title does we do not catch the error.
    """
    with open(fname, "r") as f:
        hdr = next(f)
    title = hdr.split(constants.SEP, 1)[0]
    if title != "Run":
        raise ValueError("Expected first column of '%s' to be labeled 'Run', got label '%s'" % (fname, title))
    last_index = hdr.rsplit(constants.SEP, 1)[-1]
    return int(last_index)

def tabfile_of_rktd(rktdfile):
    """ (-> Path-String Path-String)
        Input: a .rktd file; the results of running the benchmarks.
        Output: the string name of a newly-generated .tab file
                (a more human-readable and Python-parse-able version of the .rktd)
    """
    # We have a Racket script to generate the .tab file,
    # make sure it exists.
    if not os.path.exists("sexp-to-tab.rkt"):
        raise ValueError("internal error: cannot file the 'sexp-to-tab.rkt' script. Sorry.")
    # TODO replace with a call to Python's 'subprocess'
    os.system("racket sexp-to-tab.rkt %s" % rktdfile)
    # Strip the suffix from the input file, replace with .tab
    return "%s.tab" % rktdfile.rsplit(".", 1)[0]

def main(tabfile, dgraph):
    """ (-> Path-String GraphDict Result)
        Input: a .tab file, an overall summary of running all configurations
               of a project.
        Output: a Result object.
    """
    fname = strip_suffix(tabfile).rsplit("/", 1)[-1]
    num_modules = count_modules(tabfile)
    num_configs = 2 ** num_modules
    print("Project contains %s modules (%s configurations)" % (num_modules, num_configs))
    u_raw = all_cells_matching(tabfile, is_untyped_config)
    g_raw = all_cells_matching(tabfile, lambda x: not (is_typed_config(x) or is_untyped_config(x)))
    t_raw = all_cells_matching(tabfile, is_typed_config)
    ugt_violin = violin_plot([u_raw, g_raw, t_raw]
                            ,"%s_untyped-vs-gradual-vs-typed" % fname
                            ,"Configuration"
                            ,"Runtime (ms)"
                            ,xlabels=["untyped","gradual\n(all configs)","typed"]) if u_raw and g_raw and t_raw else None
    # Collect absolute BEST and WORST times+configs
    ten_percent = max(3, min(10, int(0.10 * num_configs)))
    best_cfg_and_times  = best_rows(tabfile
                        ,ten_percent
                        ,lambda acc,tmp: tmp[1] < acc[1]
                        ,lambda row:(row[0], int(statistics.mean([int(x) for x in row[1::]]))))
    worst_cfg_and_times = best_rows(tabfile
                        ,ten_percent
                        ,lambda acc,tmp: acc[1] < tmp[1]
                        ,lambda row:(row[0], int(statistics.mean([int(x) for x in row[1::]]))))
    stats = {
        "title"    : fname
        ,"runs"    : count_runs(tabfile)
        ,"graph"   : dgraph
        ,"ugt"     : {"img" : ugt_violin
                     ,"summary" : {"untyped" : basic_row_stats(u_raw)
                                  ,"gradual" : basic_row_stats(g_raw)
                                  ,"typed"   : basic_row_stats(t_raw)}}
        ,"best"    : [basic_config_stats(v[0], v[1], dgraph)
                      for v in best_cfg_and_times if v is not None]
        ,"worst"   : [basic_config_stats(v[0], v[1], dgraph)
                      for v in worst_cfg_and_times if v is not None]
        ,"bucketed": violin_plot(bucketize(tabfile, num_typed_modules, num_modules)
                                ,"%s_by-typed-modules" % fname
                                ,"Number of Typed Modules"
                                ,"Runtime (ms)"
                                ,positions=range(0, 1+num_modules)
                                ,xlabels=range(0, 1+num_modules))
        ,"fixed"   : [double_violin_plot([bucketize(tabfile, num_typed_modules, num_modules, pred=lambda cfg: cfg[v[0]] == "0")
                                         ,bucketize(tabfile, num_typed_modules, num_modules, pred=lambda cfg: cfg[v[0]] == "1")]
                                 ,"%s_fixing-%s" % (fname, k)
                                 ,"Number of Typed Modules"
                                 ,"Runtime (ms)"
                                 ,legend=["%s is untyped" % k
                                         ,"%s is typed" % k])
                      for (k,v) in sorted(dgraph.items(), key=lambda item:item[1][0])]
    }
    return stats
