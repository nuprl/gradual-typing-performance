"""
Similar to boxplot.py, but use violin plots to show the distribution.
- Axis: Runtime vs. Num.Typed Modules
- Violins: distro of running types for configurations with N typed modules
- Overlay line for least-sum path, to show that not all points are connected
"""

import shortestpath
import statistics
import sys
import util
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon
import numpy as np

SEP = "\t"

# (-> String Nat)
def num_typed(xs):
    # Count the number of "1" characters in the string `xs`
    return sum((1 for c in xs if c == "1"))

# (-> Path-String (-> String Boolean) (Listof (Listof Nat)))
def data_by_numtyped(fname, pred=None):
    # Group data into a list of lists.
    # Index = Number of typed modules
    data = [[] for _ in range(1+util.count_modules(fname))]
    with open(fname, "r") as f:
        next(f)
        for line in f:
            cols = line.strip().split()
            title, vals = cols[0], cols[1:]
            if pred is None or pred(title):
                data[num_typed(title)].append([int(x) for x in vals])
    return data

# (-> Path-String (Listof String) (Listof Nat))
def runtimes_of_path(fname, path):
    # For each bitstring in the path, get its min runtime
    avgs_by_title = {}
    bitstrings = set(path)
    with open(fname, "r") as f:
        for line in f:
            rows = line.strip().split()
            title = rows[0]
            if title in bitstrings and title not in avgs_by_title:
                vals = [int(x) for x in rows[1:]]
                avgs_by_title[title] = min(vals)
    return [time for (_,time) in sorted(avgs_by_title.items(), key=lambda x:x[0])]

def draw_violin(data, alpha=1, color='royalblue', meanmarker="*", positions=None):
    ## Add data
    posns = positions or range(1,1+len(data))
    vp = plt.violinplot(data, positions=posns, showmeans=True, showextrema=True, showmedians=True)
    ## Re-color bodies
    for v in vp['bodies']:
        v.set_edgecolors('k')
        v.set_facecolors(color)
        v.set_alpha(alpha)
    ## Re-color median, min, max lines to be black
    for field in ['cmaxes', 'cmins', 'cbars', 'cmedians']:
        vp[field].set_color('k')
    ## Draw stars, for means
    # Make line invisible
    vp['cmeans'].set_color(color)
    vp['cmeans'].set_alpha(alpha)
    # Draw a *
    for i in range(len(data)):
        plt.plot(posns[i], [np.average(data[i])], color='w', marker=meanmarker, markeredgecolor='k')
    return

# (-> Path-String Void)
def main(fname):
    data = data_by_numtyped(fname)
    fig,ax1 = plt.subplots() #add figsize?
    draw_violin(data, color='royalblue')
    ## add a light-colored horizontal grid
    ax1.yaxis.grid(True, linestyle='-', which='major', color='lightgrey', alpha=0.5)
    ## Shortest-path line
    path = shortestpath.least_sum_path(fname)
    plt.plot(range(1,1+len(path)), runtimes_of_path(fname, path), color='r', linestyle="--", linewidth="1")
    ## plot axis: runtime + num types
    ax1.set_axisbelow(True)
    ax1.set_title(fname.rsplit("/",1)[-1].rsplit(".")[0])
    ax1.set_xlabel("Num. Typed Modules")
    ax1.set_ylabel("Runtime (ms)")
    plt.xticks(range(1,1+len(data)), range(0, len(data)))
    ## Legend
    # Reset y limit
    ymin,ymax = ax1.get_ylim()
    ax1.set_ylim(ymin-5, ymax)
    plt.figtext(0.80, 0.04, "---", color='r', weight='roman', size='x-small')
    plt.figtext(0.82, 0.04, "Least-sum path", color='k', weight='roman', size='x-small')
    plt.figtext(0.80, 0.01, '*', color='white', backgroundcolor='royalblue',weight='roman', size='medium')
    plt.figtext(0.82, 0.01, ' Average Value', color='black', weight='roman', size='x-small')
    ## Save & clear
    new_name = util.gen_name(fname, "violin", "png")
    plt.savefig(new_name)
    plt.clf()
    print("Saved figure to %s" % new_name)
    return

if __name__ == "__main__":
    if len(sys.argv) == 2 and sys.argv[1].endswith(".tab"):
        main(sys.argv[1])
    else:
        print("Usage: violinplot.py FILE.tab")
