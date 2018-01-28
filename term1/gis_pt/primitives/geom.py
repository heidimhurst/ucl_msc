import numpy as np


class Geom(object):
    """Base geometry class from which points, lines, polygons will inherit."""
    def getStartPoint(self):
        return self.coords[0]

    def getEndPoint(self):
        return self.coords[-1]

    def getNumPoints(self):
        return self.coords.shape[0]

    # TODO: add point
    def addPoint(self, point):
        # is the point an array of coordinates?
        if isinstance(point, np.ndarray):
            self._coords = np.vstack((self.coords, point))

        # is the point an object containing coordinates? (work around to prevent circular referencing)
        elif isinstance(point.coords, np.ndarray):
            self._coords = np.vstack((self.coords, point.coords))

        else:
            raise TypeError("Point to add must be point with coordinates or np array of coordinates")


