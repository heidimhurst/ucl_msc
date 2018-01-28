import numpy as np


class Geom(object):
    """Base geometry class from which points, lines, polygons will inherit."""
    def getStartPoint(self):
        return self.coords[0]

    def getEndPoint(self):
        return self.coords[-1]

    def getNumPoints(self):
        return self.coords.shape[0]

    def addPoint(self, point):
        # is the point an array of coordinates?
        if isinstance(point, np.ndarray):
            self._coords = np.vstack((self.coords, point))

        # is the point an object containing coordinates? (work around to prevent circular referencing)
        elif isinstance(point.coords, np.ndarray):
            self._coords = np.vstack((self.coords, point.coords))

        else:
            raise TypeError("Point to add must be point with coordinates or np array of coordinates")

class Point(Geom):
    """Simple point class"""
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


class Line(Geom):
    """simple line class"""
    def __init__(self, points=[]):
        # points can be ordered list of point coordinates or list of point objects
        if not isinstance(points, list):
            raise TypeError("Points must be a list of coordinates OR point objects.")

        # case for ingesting point objects
        if all(isinstance(n, Point) for n in points):
            coord = np.empty(shape=[len(points), 3])
            for i in range(len(points)):
                coord[i] = points[i].coords
            self._coords = coord

        # case for ingesting list of coordinates
        elif all(isinstance(n, np.ndarray) for n in points):
            self._coords = np.concatenate(points, axis=0)

        # catchall case
        else:
            raise TypeError("Line must be created using an ordered list of point coordinates OR list of point objects.")

    @property
    def coords(self):
        return self._coords

    # @coords.setter #TODO: for some reason wasn't working when I just called it coords, dunno why?
    def setcoords(self, points):
        # points can be ordered list of point coordinates or list of point objects
        if not isinstance(points, list):
            raise TypeError("Points must be a list of coordinates OR point objects.")

        # case for ingesting point objects
        if all(isinstance(n, Point) for n in points):
            coord = np.empty(shape=[len(points), 3])
            for i in range(len(points)):
                coord[i] = points[i].coords
            self._coords = coord

        # case for ingesting list of coordinates
        elif all(isinstance(n, np.ndarray) for n in points):
            self._coords = np.concatenate(points, axis=0)

        # catchall case
        else:
            raise TypeError("Line coordinates must be updated using an ordered list of point coordinates OR list of point objects.")


# TEST
# uncomment print statements to view outputs
pointa = Point(1, 2)
pointb = Point(3, 4)
# pointa.addPoint(pointb)

# test line object creation with coordinates
linea = Line([pointa.coords, pointb.coords])
# print(linea.coords)

# test setter with point objects
lineb = Line([pointb, pointa])
# print(lineb.coords)

# test exception handling for line - should thrown an error
# linec = Line(["asdkjalskdfj", pointa])

# test coordinate setting from point coordinates
points = [pointa.coords, pointb.coords]
lineb.setcoords(points)
# print(lineb.coords)

# test coordinate setting from point objects
lineb.setcoords([pointb, pointa])
# print(lineb.coords)

# test adding points from coordinates
pointc = Point(0,0)
lineb.addPoint(pointc.coords)
# print(lineb.coords)

# test adding point from point objects
lineb.addPoint(pointc)
# print(lineb.getNumPoints())
# print(lineb.coords)
