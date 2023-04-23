from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
import numpy as np
import matplotlib.pyplot as plt
from bosh_result import *
import matplotlib as mpl


def cuboid_data2(o, size=(1, 1, 1)):
    X = [[[0, 1, 0], [0, 0, 0], [1, 0, 0], [1, 1, 0]],
         [[0, 0, 0], [0, 0, 1], [1, 0, 1], [1, 0, 0]],
         [[1, 0, 1], [1, 0, 0], [1, 1, 0], [1, 1, 1]],
         [[0, 0, 1], [0, 0, 0], [0, 1, 0], [0, 1, 1]],
         [[0, 1, 0], [0, 1, 1], [1, 1, 1], [1, 1, 0]],
         [[0, 1, 1], [0, 0, 1], [1, 0, 1], [1, 1, 1]]]
    X = np.array(X).astype(float)
    for i in range(3):
        X[:, :, i] *= size[i]
    X += np.array(o)
    return X


def plotCubeAt2(positions, sizes=None, colors=None, **kwargs):
    if not isinstance(colors, (list, np.ndarray)): colors = ["C0"] * len(positions)
    if not isinstance(sizes, (list, np.ndarray)): sizes = [(1, 1, 1)] * len(positions)
    g = []
    for p, s, c in zip(positions, sizes, colors):
        g.append(cuboid_data2(p, size=s))
    return Poly3DCollection(np.concatenate(g),
                            facecolors=np.repeat(colors, 6), **kwargs)


mpl.use('TkAgg')

shelve = [1200, 2400, 650, 3000]
sx = shelve[0]
sy = shelve[2]
sz = shelve[3]

#
# positions = [(-20, 0, 0), (sx, 0, 0), (0, 0, 1000)]
# sizes = [(20, sy, sz), (20, sy, sz), (sx, sy, 40)]
# colors = ["black", "black", "limegreen"]

# positions = [(-20, 0, 0), (sx, 0, 0)] + RES[0]  # [0:10]
# sizes = [(20, sy, sz), (20, sy, sz)] + RES[1]  # [0:10]
# colors = ["b", "b"] + RES[2]  # [0:10]

max_res = 77
positions = RES[0][:max_res]
sizes = RES[1][:max_res]
colors = RES[2][:max_res]

print(positions)
print(sizes)

fig = plt.figure()
ax = fig.add_subplot(projection='3d')
ax.set_box_aspect([1200, 650, 2400])
# ax.set_box_aspect([1, 1, 1])
# ax = fig.gca(projection='3d')
# ax.set_aspect('auto')

pc = plotCubeAt2(positions, sizes, colors=colors, edgecolor="k")
ax.add_collection3d(pc)

ax.set_xlim([-20, sx + 20])
ax.set_ylim([0, sy])
ax.set_zlim([0, sz])

# plt.ion()
plt.show()
