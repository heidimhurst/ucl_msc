import mpld3
from mpld3 import plugins
from mpld3.utils import get_id
import numpy as np
import collections
import matplotlib.pyplot as plt

N_paths = 1
N_steps = 100

x = np.linspace(0, 10, 100)
y = 0.1 * (np.random.random((N_paths, N_steps)) - 0.5)
y = y.cumsum(1)

fig = plt.figure()
ax1 = fig.add_subplot(2, 1, 1)
ax2 = fig.add_subplot(2, 1, 2)

labels = ["a"]
l1 = ax1.plot(x, y.T, lw=4, alpha=0.1)
s1 = []
for i in range(N_paths):
    s = ax2.scatter(x, y[i, :], c="blue", alpha=0.1)
    s1.append(s)

plugins.connect(fig, plugins.InteractiveLegendPlugin(l1, labels, ax=ax1))
plugins.connect(fig, plugins.InteractiveLegendPlugin(s1, labels, ax=ax2))

mpld3.show()
pass