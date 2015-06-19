from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
from mpl_toolkits.axes_grid1 import AxesGrid

import constants

# Set font, globally
default_font = {
    #'family': 'normal',
    'weight' : 'semibold',
    'size' : 24,
    #'linespacing' : 0.4,
}
title_font = {
    #'family': 'normal',
    'weight' : 'semibold',
    'size' : 30,
    #'linespacing' : 0.4,
}
matplotlib.rc('font', **default_font)
# Darken grid. For more options, see
# http://stackoverflow.com/questions/17925545/adjusting-gridlines-on-a-3d-matplotlib-figure
matplotlib.rc('lines', linewidth=3)

# reasonable colormaps
COLORMAPS = [
    cm.cubehelix,
    cm.CMRmap,
    cm.coolwarm_r,
    cm.gnuplot2,
    cm.nipy_spectral,
    cm.gist_rainbow,
    cm.gist_ncar,
    cm.gist_heat,
    cm.gist_earth,
    cm.gist_stern,
]

#############################################################################

# http://stackoverflow.com/questions/7404116/defining-the-midpoint-of-a-colormap-in-matplotlib
def shiftedColorMap(cmap, start=0, midpoint=0.5, stop=1.0, name='shiftedcmap'):
    '''
    Function to offset the "center" of a colormap. Useful for
    data with a negative min and positive max and you want the
    middle of the colormap's dynamic range to be at zero

    Input
    -----
      cmap : The matplotlib colormap to be altered
      start : Offset from lowest point in the colormap's range.
          Defaults to 0.0 (no lower ofset). Should be between
          0.0 and `midpoint`.
      midpoint : The new center of the colormap. Defaults to 
          0.5 (no shift). Should be between 0.0 and 1.0. In
          general, this should be  1 - vmax/(vmax + abs(vmin))
          For example if your data range from -15.0 to +5.0 and
          you want the center of the colormap at 0.0, `midpoint`
          should be set to  1 - 5/(5 + 15)) or 0.75
      stop : Offset from highets point in the colormap's range.
          Defaults to 1.0 (no upper ofset). Should be between
          `midpoint` and 1.0.
    '''
    cdict = {
        'red': [],
        'green': [],
        'blue': [],
        'alpha': []
    }
    # regular index to compute the colors
    reg_index = np.linspace(start, stop, 257)
    # shifted index to match the data
    shift_index = np.hstack([
        np.linspace(0.0, midpoint, 128, endpoint=False), 
        np.linspace(midpoint, 1.0, 129, endpoint=True)
    ])
    for ri, si in zip(reg_index, shift_index):
        r, g, b, a = cmap(ri)
        cdict['red'].append((si, r, r))
        cdict['green'].append((si, g, g))
        cdict['blue'].append((si, b, b))
        cdict['alpha'].append((si, a, a))
    newcmap = matplotlib.colors.LinearSegmentedColormap(name, cdict)
    plt.register_cmap(cmap=newcmap)
    return newcmap

def make_mesh(xbounds, ybounds, zfun, samples):
    """
        Create a mesh from:
        - the implicit 2d array delimited by xbounds
        - the implicit 2d array delimited by ybounds
        and call the function `zfun` on each point in the mesh.

        Return 3 matrices, each indexed along the Y axis first (then the X)
        - X : matrix of x positions
        - Y : matrix of y positions
        - Z : matrix of z positions
    """
    xs = np.linspace(xbounds[0], xbounds[1], num=samples)
    ys = np.linspace(ybounds[0], ybounds[1], num=samples)
    X, Y, Z = [], [], []
    for y in ys:
        X.append(xs)
        Y.append([y] * len(xs))
        Z.append([zfun(x, y) for x in xs])
    return X, Y, np.ma.array(Z)

# For info on colormaps, see
# http://matplotlib.org/api/pyplot_summary.html?highlight=colormaps#matplotlib.pyplot.colormaps
def contour(xbounds, ybounds, zfun, title=None, xlabel=None, ylabel=None, zlabel=None, samples=constants.GRAPH_SAMPLES, output=None, zlim=None, colormap=cm.cubehelix):
    fig = plt.figure()
    ax = fig.gca(projection='3d')
    X, Y, Z = make_mesh(xbounds, ybounds, zfun, samples)
    cmap = shiftedColorMap(colormap
                           ,start=0, midpoint=0.4, stop=.85, name='shifted%s' % colormap.name)
    surf = ax.plot_surface(X, Y, Z
                           ,rstride=1, cstride=1 # row/column step size (int, default=10)
                           ,cmap=cmap
                           ,vmin=0, vmax=zlim
                           ,linewidth=0, antialiased=False)
    # Override the x and y ticks
    xposns = list(range(xbounds[0], xbounds[1]+1))
    yposns = [(ybounds[1] - ybounds[0]) // 2, ybounds[1]]
    plt.xticks(xposns, ["%sx" % n for n in xposns])
    plt.yticks(yposns, ["%sx" % m for m in yposns])
    ax.zaxis.get_major_ticks()[0].label1.set_visible(False)
    # Set the title and z-axis label (z label is a hack)
    if title:
        plt.suptitle(title, fontdict=title_font, y=0.88)
    plt.title(zlabel, fontdict=default_font, x=0.1, y=0.87)
    #
    ax.set_xlim(xbounds[0], xbounds[1])
    ax.set_ylim(ybounds[0], ybounds[1])
    ax.set_zlim(0, zlim)
    ax.set_xlabel(xlabel, fontdict=default_font)
    ax.set_ylabel(ylabel, fontdict=default_font)
    # Save
    ax.view_init(elev=10, azim=240)
    out = "%s-%s.png" % (output, colormap.name)
    plt.savefig(out)
    plt.clf()
    print("Saved contour to '%s'" % output)
    plt.close()
    return out

def make_figs(output):
    """
        Produce a ton of views of the same graph,
        Return a list of all views
    """
    figs = []
    for e in range(10, 90, 30):
        for r in range(0, 360, 20):
            out = "%s-%se-%sr.png" % (output, e, r)
            ax.view_init(elev=e, azim=r)
            plt.savefig(out)
            figs.append(out)
    return figs
