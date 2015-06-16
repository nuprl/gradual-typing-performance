from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation

def contour(X_1d, Y_1d, Z_2d, title, xlabel=None, ylabel=None, zlabel=None, output=None, zlim=None):
    fig = plt.figure()
    ax = fig.gca(projection='3d')
    X, Y = np.meshgrid(X_1d, Y_1d)
    Z = np.array(Z_2d).reshape(Y.shape)
    surf = ax.plot_surface(X, Y, Z
                           ,rstride=1, cstride=1 ## TODO, probably
                           ,cmap=cm.coolwarm, linewidth=0, antialiased=False)
    ax.set_zlim(0, zlim)
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

