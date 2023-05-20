import tkinter

from matplotlib.backends.backend_tkagg import (
    FigureCanvasTkAgg, NavigationToolbar2Tk)
# Implement the default Matplotlib key bindings.
from matplotlib.backend_bases import key_press_handler
from matplotlib.figure import Figure
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
import numpy as np
import matplotlib.pyplot as plt
from output.bosh_result import *
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


def update(canvas, fig, i):
    # if i>9:
    #     break;
    # sub = 111+i*10+i
    ax[i] = fig.add_subplot(1, 1, 1, projection='3d')
    ax[i].set_box_aspect([1200, 650, 2400])
    # ax.set_box_aspect([1, 1, 1])
    # ax = fig.gca(projection='3d')
    # ax.set_aspect('auto')

    pc = plotCubeAt2(positions[i], sizes[i], colors=colors[i], edgecolor="k")
    ax[i].add_collection3d(pc)

    ax[i].set_xlim([-20, sx + 20])
    ax[i].set_ylim([0, sy])
    ax[i].set_zlim([0, sz])

    # canvas.clf()
    canvas.draw()


root = tkinter.Tk()
root.wm_title("Embedding in Tk")

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

bays = 4
positions = RES[0]  # [0:max_res]
sizes = RES[1]
colors = RES[2]

ax = {}
# pc = []
fig = plt.figure()
canvas = FigureCanvasTkAgg(fig, master=root)  # A tk.DrawingArea.
update(canvas, fig, 0)
canvas.draw()

# pack_toolbar=False will make it easier to use a layout manager later on.
toolbar = NavigationToolbar2Tk(canvas, root, pack_toolbar=False)
toolbar.update()

canvas.mpl_connect(
    "key_press_event", lambda event: print(f"you pressed {event.key}"))
canvas.mpl_connect("key_press_event", key_press_handler)

button_quit = tkinter.Button(master=root, text="Quit", command=root.destroy)


def updateFig(newVal):
    update(canvas, fig, int(newVal)-1)


slider_update = tkinter.Scale(root, from_=1, to=len(positions), orient=tkinter.HORIZONTAL,
                              command=updateFig, label="Bay")

# Packing order is important. Widgets are processed sequentially and if there
# is no space left, because the window is too small, they are not displayed.
# The canvas is rather flexible in its size, so we pack it last which makes
# sure the UI controls are displayed as long as possible.
button_quit.pack(side=tkinter.BOTTOM)
slider_update.pack(side=tkinter.BOTTOM)
toolbar.pack(side=tkinter.BOTTOM, fill=tkinter.X)
canvas.get_tk_widget().pack(side=tkinter.TOP, fill=tkinter.BOTH, expand=True)

tkinter.mainloop()
