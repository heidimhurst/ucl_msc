import numpy as np

from primitives.point import Point
from primitives.line import Line

# testing point creation
pointa = Point(1, 2)
pointb = Point(3, 4)
print(pointa.coords)

# test line object creation with coordinates
linea = Line([pointa.coords, pointb.coords])

# test setter with point objects
lineb = Line([pointb, pointa])
print(lineb.coords)

# test exception handling for line
# linec = Line(["asdkjalskdfj", pointa])

# test coordinate setting from point coordinates
points = [pointa.coords, pointb.coords]
lineb.setcoords(points)

# test coordinate setting from point objects
lineb.setcoords([pointb, pointa])

# test adding points from coordinates
pointc = Point(0,0)
lineb.addPoint(pointc.coords)

# test adding point from point objects
lineb.addPoint(pointc)
