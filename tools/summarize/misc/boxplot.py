"""
Create a boxplot for spreadsheet data.
- Axis: Runtime vs. Num.Typed Modules
- Boxes show the distribution of running types for configurations with N
  typed modules
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

# (-> Path-String (Listof (Listof Nat)))
def data_by_numtyped(fname):
    # Group data into a list of lists.
    # Index = Number of typed modules
    data = None
    with open(fname, "r") as f:
        next(f)
        for line in f:
            cols = line.strip().split(SEP)
            title, vals = cols[0], cols[1::]
            if data is None:
                # initialize, because we didn't have the right length before
                data = [[] for _ in range(0, 1+len(title))]
            data[num_typed(title)].append([int(x) for x in vals])
    return data

# (-> Path-String (Listof String) (Listof Nat))
def runtimes_of_path(fname, path):
    # For each bitstring in the path, get its min runtime
    avgs_by_title = {}
    bitstrings = set(path)
    with open(fname, "r") as f:
        for line in f:
            rows = line.strip().split(SEP)
            title = rows[0]
            if title in bitstrings and title not in avgs_by_title:
                vals = [int(x) for x in rows[1::]]
                avgs_by_title[title] = min(vals)
    return [time for (_,time) in sorted(avgs_by_title.items(), key=lambda x:x[0])]

# (-> Path-String Void)
def main(fname):
    data = data_by_numtyped(fname)
    ## Add data
    fig,ax1 = plt.subplots() #add figsize?
    bp = plt.boxplot(data, notch=0, sym="+", vert=True, whis=1)
    plt.setp(bp['boxes'], color='black')
    plt.setp(bp['whiskers'], color='black')
    plt.setp(bp['fliers'], color='red', marker='+')
    ## add a light-colored horizontal grid
    ax1.yaxis.grid(True, linestyle='-', which='major', color='lightgrey', alpha=0.5)
    ## Fancier boxes
    for i in range(len(data)):
        box = bp['boxes'][i]
        coords = []
        ## Color the boxes
        for j in range(0,5):
            coords.append((box.get_xdata()[j], box.get_ydata()[j]))
        boxPolygon = Polygon(coords, facecolor="royalblue")
        ax1.add_patch(boxPolygon)
        ## Re-draw median lines
        med = bp['medians'][i]
        mx, my = [], []
        for j in range(2):
            mx.append(med.get_xdata()[j])
            my.append(med.get_ydata()[j])
            plt.plot(mx,my, 'black')
        ## Draw avg. dot
        plt.plot([np.average(med.get_xdata())], [np.average(data[i])], color='w', marker='*', markeredgecolor='k')
    ## Shortest-path line
    path = shortestpath.least_sum_path(fname)
    plt.plot(range(1,1+len(path)), runtimes_of_path(fname, path), color='r', linestyle="--", linewidth="1")
    ## plot axis: runtime + num types
    ax1.set_axisbelow(True)
    ax1.set_title(fname.rsplit("/",1)[-1].rsplit(".")[0])
    ax1.set_xlabel("Num. Typed Modules")
    ax1.set_ylabel("Runtime (ms)")
    ax1.set_xticklabels(range(0, len(path)))
    ## Legend
    # Reset y limit
    ymin,ymax = ax1.get_ylim()
    ax1.set_ylim(ymin-5, ymax)
    plt.figtext(0.80, 0.04, "---", color='r', weight='roman', size='x-small')
    plt.figtext(0.82, 0.04, "Least-sum path", color='k', weight='roman', size='x-small')
    plt.figtext(0.80, 0.01, '*', color='white', backgroundcolor='royalblue',weight='roman', size='medium')
    plt.figtext(0.82, 0.01, ' Average Value', color='black', weight='roman', size='x-small')
    ## Save & clear
    new_name = util.gen_name(fname, "boxplot", "png")
    plt.savefig(new_name)
    plt.clf()
    print("Saved figure to %s" % new_name)
    return

if __name__ == "__main__":
    if len(sys.argv) == 2 and sys.argv[1].endswith(".tab"):
        main(sys.argv[1])
    else:
        print("Usage: boxplot.py FILE.tab")
