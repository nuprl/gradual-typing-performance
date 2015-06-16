from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np
matplotlib.use('Agg') # Disable the display, does not affect graph generation

def contour(arr2d, title, xlabel=None, ylabel=None, zlabel=None, output=None, zlim=None):
    fig = plt.figure()
    ax = fig.gca(projection='3d')
    X = range(0, len(arr2d))
    Y = range(0, len(arr2d[0]))
    # print("X = %s" % X)
    # print("Y = %s" % Y)
    X, Y = np.meshgrid(X, Y)
    # print("AFTER MESHING")
    # print("X = %s" % X)
    # print("Y = %s" % Y)
    # zs = np.array([arr2d[l][n] for l,n in zip(np.ravel(X), np.ravel(Y))])
    # print("ARR2d = %s" % arr2d)
    # print("zs = %s" % zs)
    Z = arr2d #zs.reshape(X.shape)
    surf = ax.plot_surface(X, Y, Z
                           #,rstride=1, cstride=1 ## TODO, probably
                           ,cmap=cm.coolwarm, linewidth=0, antialiased=False)
    ax.set_zlim(0, zlim)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.set_zlabel(zlabel)
    # Save
    plt.savefig(output)
    plt.clf()
    plt.close()
    print("Saved contour to '%s'" % output)
    return output

