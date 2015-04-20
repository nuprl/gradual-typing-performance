"""
bigpicture.py

Show the distributions of all modules as a violin plot
(Alternatively, just graph min/max or variance)
"""

import grouping
import statistics
import sys
import util
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon
import numpy as np

def get_label(fname):
    return "\n".join(fname.rsplit(".",1)[0].rsplit("/", 1)[-1].split("-", 1))

# (-> Path-String Void)
def main(*args):
    data = [grouping.every_point_in(fname) for fname in args]
    ## Add data
    fig,ax1 = plt.subplots() #add figsize?
    posns = [1.5 * x for x in range(1,1+len(data))]
    vp = plt.violinplot(data, positions=posns, showmeans=True, showextrema=True, showmedians=True)
    ## Re-color bodies
    for v in vp['bodies']:
        v.set_edgecolors('k')
        v.set_facecolors('royalblue')
        v.set_alpha(1)
    ## Re-color median, min, max lines to be black
    for field in ['cmaxes', 'cmins', 'cbars', 'cmedians']:
        vp[field].set_color('k')
    ## Draw stars, for means
    # Make line invisible
    vp['cmeans'].set_color('royalblue')
    vp['cmeans'].set_alpha(1)
    # Draw a *
    for i in range(len(data)):
        plt.plot([1.5 * (i+1)], [np.average(data[i])], color='w', marker='*', markeredgecolor='k')
    ## add a light-colored horizontal grid
    ax1.yaxis.grid(True, linestyle='-', which='major', color='lightgrey', alpha=0.5)
    ## plot axis: runtime + num types
    new_name = "all-runtimes"
    ax1.set_axisbelow(True)
    ax1.set_title("All Runtimes")
    ax1.set_xlabel("Program")
    ax1.set_ylabel("Runtime (ms)")
    # Reset y limit
    ymin,ymax = plt.ylim()
    plt.ylim(ymin-20, ymax)
    plt.xticks(posns, [get_label(fname) for fname in args], rotation=0, size='x-small')
    # ## Legend
    # # Reset y limit
    # ymin,ymax = ax1.get_ylim()
    # ax1.set_ylim(ymin-5, ymax)
    # plt.figtext(0.80, 0.04, "---", color='r', weight='roman', size='x-small')
    # plt.figtext(0.82, 0.04, "Least-sum path", color='k', weight='roman', size='x-small')
    plt.figtext(0.80, 0.01, '*', color='white', backgroundcolor='royalblue',weight='roman', size='medium')
    plt.figtext(0.82, 0.01, ' Average Value', color='black', weight='roman', size='x-small')
    ## Save & clear
    plt.savefig(new_name)
    plt.clf()
    print("Saved figure to %s" % new_name)
    return

if __name__ == "__main__":
    if len(sys.argv) > 1:#and all((x.endswith("*.tab") for x in sys.argv[1::])):
        main(*sys.argv[1::])
    else:
        print("Usage: big-picture.py FILE.tab ...")
