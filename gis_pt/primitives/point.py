import numpy as np
from primitives.geom import Geom


class Point(Geom):
    """Simple point class"""
    # TODO documentation: https://www.jetbrains.com/help/pycharm/creating-documentation-comments.html
    def __init__(self, x=0.0, y=0.0, z=float('nan')):
        self._coords = np.array([x, y, z], dtype=float)
        self._coords.shape = (1, 3)

    @property
    def coords(self):
        return self._coords

    def getX(self):
        return self._coords[0, 0]

    def getY(self):
        return self._coords[0, 1]

    def getZ(self):
        return self._coords[0, 2]

    # @coords.setter # this doesn't work with decorator
    def setX(self, x):
        if type(x) == float or type(x) == int:
            self._coords[0, 0] = float(x)
        else:
            raise TypeError("Coordinates must be a float or an int.")

    def setY(self, y):
        if type(y) == float or type(y) == int:
            self._coords[0, 1] = float(y)
        else:
            raise TypeError("Coordinates must be a float or an int.")

    def setZ(self, z):
        if type(z) == float or type(z) == int:
            self._coords[0, 2] = float(z)
        else:
            raise TypeError("Coordinates must be a float or an int.")

    def addPoint(self, point):
        # overwrite method in geom
        raise Warning("Point cannot be added to point class.")