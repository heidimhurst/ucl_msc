import numpy as np
import math
import os

# function for drawing circles
def circlepoints(npts, nround, rad, style = "stack"):
    pts = np.zeros(shape=[npts * nround, 3], dtype=float)
    for i in range(npts * nround):
        ang = i * 2 * math.pi / npts
        if style == "stack":
            z = (math.floor(i / npts) + 1) * 100
        elif style == "flat":
            z = 100
        pt = np.array([rad * math.cos(ang), rad * math.sin(ang), z])
        pt = pt + center
        pts[i] = pt
    return pts

# output location
out = os.getcwd()
name = "trial7.txt"
out = os.path.join(out, name)

# specify size, shape, etc of points to be ingested
# radius of circle
rad = 750.0
# number of points per circle
npts = 15
# number of times around a circle
nround = 20

# center of circle
center = np.array([432390.0, 436032.0, 0.0], dtype=float)

# initialize points list
# pts = np.zeros(shape=[npts * nround, 3], dtype=float)

# create xyz for points on a circle
pts = circlepoints(npts, nround, rad, "stack")
pts = np.vstack((pts, circlepoints(100, 1, 2*rad, "flat")))
pts = np.vstack((pts, circlepoints(100, 1, 3*rad, "flat")))


# output to txt file
np.savetxt(out, pts, header="x,y,z", fmt='%f', comments="", delimiter=",", newline="\r\n")

