from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
from mpl_toolkits.axes_grid1 import AxesGrid

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

# For info on colormaps, see
# http://matplotlib.org/api/pyplot_summary.html?highlight=colormaps#matplotlib.pyplot.colormaps
def contour(X_1d, Y_1d, Z_2d, title, xlabel=None, ylabel=None, zlabel=None, output=None, zlim=None):
    fig = plt.figure()
    ax = fig.gca(projection='3d')
    X, Y = np.meshgrid(X_1d, Y_1d)
    Z = np.array(Z_2d).reshape(Y.shape)
    # cm.coolwarm_r
    # cm.cubehelix
    cmap = shiftedColorMap(cm.CMRmap
                           ,start=0, midpoint=0.4, stop=.9, name='shiftedcmap')
    surf = ax.plot_surface(X, Y, Z
                           ,rstride=1, cstride=1 ## TODO, probably
                           ,cmap=cmap
                           ,linewidth=0, antialiased=False)
    ax.set_zlim(0, zlim+1)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.set_zlabel(zlabel)
    # Save
    figs = []
    for e in [10, 40, 70]:
      ax.view_init(elev=e, azim=240)
      out = "%s-%se.png" % (output, e)
      plt.savefig(out)
      figs.append(out)
    plt.clf()
    plt.close()
    print("Saved contour to '%s'" % output)
    return figs

