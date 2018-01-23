import numpy as np
from primitives.geom import Geom
from primitives.point import Point


class Line(Geom):
    """simple line class"""
    def __init__(self, points=[]):
        # TODO: could we abstract this into a function as well???
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

