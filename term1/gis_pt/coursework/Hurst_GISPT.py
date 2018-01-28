#!/usr/bin/env python
"""
A program for testing whether a point is inside a polygon using one of several methods.
Includes functionality to read polygon and line object in from CSV files, formatted with labeled or
unlabeled X and Y columns, as well as functionality to create an interactive graph and save it as HTML.

Plot is interactive.  Toggle on/off labels, explore point data using tooltips, and zoom/pan.

Built as a final project for CEGEG082: GIS Principles and Technology
as part of the MSc GIS at University College London.

Note: mpld3 MUST be installed as a package prior to running this code to visualize any outputs.

Examples:
    The pip algorithm can be called either from a point/line via the point.isInside() method,
    or from a polygon via the polygon.contains() method.  More examples are at the end of this code.

    polygon1.contains(line1, plot=True) # plot and visualize if all vertices in line1 are within polygon1
    point1.isInside(polygon1, plot=True, save=True) # plot and save output if a point is inside a polygon.

__author__ = "VPFW6"
__date__ = "16 January 2018"

Note: Docstrings and comments based on Google's Python guide
(https://google.github.io/styleguide/pyguide.html#Comments).
Documentation has been amended though not updated for given classes (Geom, Point, Line, Poly).
References are placed in the docstring of the function which they informed.
"""


import numpy as np  # for np arrays
import os  # for confirming that files exist
import csv  # read in CSV files
import matplotlib.pyplot as plt  # basic plotting
from matplotlib.patches import Polygon as Pgon  # support for plotting polygon
import mpld3  # interactive plotting module


class Geom(object):    # Geometry class
    """
    Base class for creating geometry; provided in assignment.
    """
    def getStartPoint(self):
        return self.coords[0]

    def getEndPoint(self):
        return self.coords[-1]

    def getNumPoints(self):
        return self.coords.shape[0]

    def addPoint(self, point):
        self.coords = np.vstack([self.coords, point])

    def getBoundingBox(self):
        """
        Function to return the coordinates of a bounding box for any geometry

        :return: np array [2,4] containing coordinates of bounding box.
        """
        # use min/max to get coordinates for each corner
        corner1 = np.array([min(self.coords[:, 0]),  min(self.coords[:, 1])])
        corner2 = np.array([max(self.coords[:, 0]), min(self.coords[:, 1])])
        corner3 = np.array([max(self.coords[:, 0]), max(self.coords[:, 1])])
        corner4 = np.array([min(self.coords[:, 0]), max(self.coords[:, 1])])
        # create list of points (can be used to create polygon elsewhere
        bbox = np.vstack([corner1, corner2, corner3, corner4])
        # return list of points
        return(bbox)

    def isInside(self, polygon, plot=False, method='w+', save=False):
        """
        Function to determine if object is inside a given polygon.

        :param polygon: (Polygon) contains vertices of polygon
        :param plot: (Bool) true to enable plotting; default false.
                        Must be true to enable saving.
        :param method: (str) method of computing point in polygon.  Valid inputs:
                        'ol' - points on lines only
                        'ov' - points on vertices only
                        'lv' - points on lines or vertices
                        'w' - winding number algorithm
                        'w+' - winding number PLUS any points on lines/vertices (DEFAULT)
                        'rc' - ray casting algorithm
                        'rc+' - ray casting algorithm PLUS any points on lines/vertices
        :param save: (bool) true to enable saving; default false.
                        True saves image in code location folder as "PIP_visualization_{methodname}.html".

        :return: if plot = False, returns (list) of (bool) for each point indicating in/out.
        """
        # ERROR CHECKING: ensure correct formatting
        if isinstance(polygon, Polygon):
            # create PIP instance (?)
            pip = PIP(self.coords, polygon.coords)

            # plot, if desired, or return
            if plot:
                pip.viewPIP(method=method, save=save)
            else:
                return pip.pointInPolygon(method=method)
        else:
            return "Must supply a Polygon object."

    def fromCSV(self, filePath):
        """
        Allows geometry to be read in from CSV file, given path.  Contains moderate error checking.
        Not implemented/valid for points.

        :param filePath: (str) path to CSV file containing geometry information.
                First column contains x coord, second column contains y coord.  Text headers ignored.
        :return: If successful, no return: sets geometry coordinates.  If unsuccessful, error message.
        """
        # test if file exists
        if not os.path.isfile(filePath):
            raise NameError("File path is not valid. Please enter a correct path to CSV.")
        # if file exists, create vertices
        vertices = np.empty([0, 3])
        # open file to read in as CSV
        with open(filePath, 'rb') as csvfile:
            reader = csv.reader(csvfile)
            # read through each row of reader
            for row in reader:
                # only convert rows that are integer valued
                if row[0].isdigit() and row[1].isdigit():
                    # convert each row to point array
                    pt = np.array([float(row[0]), float(row[1]), np.nan])
                    # add to vertex array
                    vertices = np.vstack((vertices, pt))
        # ensure resultant set isn't empty
        if vertices.shape[1] > 0:
            self.coords = vertices
        else:
            print(vertices.shape[1])
            return "File must contain coordinates."


class Point(Geom):    # Point class
    """Point class; provided in assignment."""
    def __init__(self, x=0, y=0, z=float('nan'), filepath=None):
        self.__coords = np.array([x, y, z], dtype=float)
        self.__coords.shape = (1, 3)
    @property
    def x(self):
        return self.__coords[0, 0]
    @property
    def y(self):
        return self.__coords[0, 1]
    @property
    def z(self):
        return self.__coords[0, 2]
    @x.setter
    def x(self, x):
        self.__coords[0, 0] = x
    @y.setter   
    def y(self, y):
        self.__coords[0, 1] = y
    @z.setter
    def z(self, z):
        self.__coords[0, 2] = z
    @property
    def coords(self):
        return self.__coords
    def addPoint(self, point):
        return "Can't add a point to a point"

    def fromCSV(self, filePath):
        """Overwritten from Geom; single point cannot be read in as csv.  Read in as Line instead."""
        return "Cannot import point from CSV.  Import as Line instead."


class Line(Geom):   # Line class
    """ Line class; provided in assignment"""
    def __init__(self, points=None):
        if points is None:
            self.__coords = None
        # can initiate directly from CSV here
        elif isinstance(points, str):
            self.fromCSV(points)
        else:
            self.__coords = np.vstack(points)

    @property
    def coords(self):
        return self.__coords

    @coords.setter
    def coords(self, points):
        self.__coords = np.vstack(points)

    def addPoint(self, point):
        if self.__coords is None:
            self.__coords = point
            self.__coords.shape = (1,3)
        else:
            self.__coords = np.vstack([self.__coords, point])
        

class Polygon(Line, Geom):  # Polygon class
    """ Polygon class; provided in assessment. """
    def getEndPoint(self):
        return self.getStartPoint()

    def isInside(self, polygon, plot=False):
        """ Overwrites generic isInside function, since polygon inside polygon is out of scope. """
        return "isInside not implemented for polygon objects"

    # test pip for list of points (or singleton)
    # known bug: if used to plot output for only one point, a "phantom point" will appear for the
    # category not hit (e.g. a phantom outside will appear if the single point is inside)
    def contains(self, points, plot=False, method='w+', save=False):
        """
        Tests whether provided points are within boundaries of the Polygon.

        :param points: (Point, list of Points, Line)
        :param plot: (Bool) true to enable plotting; default false.
                        Must be true to enable saving.
        :param method: (str) method of computing point in polygon.  Valid inputs:
                        'ol' - points on lines only
                        'ov' - points on vertices only
                        'lv' - points on lines or vertices
                        'w' - winding number algorithm
                        'w+' - winding number PLUS any points on lines/vertices (DEFAULT)
                        'rc' - ray casting algorithm
                        'rc+' - ray casting algorithm PLUS any points on lines/vertices
        :param save: (bool) true to enable saving; default false.
                        True saves image in code location folder as "PIP_visualization_{methodname}.html".

        :return: if plot = False, returns (list) of (bool) for each point indicating in/out.
        """
        # ERROR CHECKING: ensure that points are correctly formatted
        if isinstance(points, Point):
            pts = points.coords

        # reformat list of points into np.ndarray of coordintates
        elif isinstance(points, list):
            if all(isinstance(p, Point) for p in points):
                # append all coords together
                pts = np.empty([0, 3])
                for p in points:
                    pts = np.vstack([pts, p.coords])
            else:
                return "List must contain only point objects."

        # extract all coordinates from line
        elif isinstance(points, Line):
            pts = points.coords

        # catchall case for improperly formatted points
        else:
            return "Please supply point or list of points."

        # run pip, plot if required
        pip = PIP(pts, self.coords)
        if plot:
            pip.viewPIP(method=method, save=save)
        else:
            return pip.pointInPolygon(method=method)


class PIP(object):
    """
    Class for Point in Polygon operations.  Provides access to plotting, saving methods.  Can be used independently of
    geometry classes.
    """
    # initiate object using np array for poly and nparray or list of nparrays for pts
    def __init__(self, pts, poly):
        """
        Initialization function.

        :param pts: (np.ndarray) array of point coordinates [n,3] or [n,2]
        :param poly: (np.ndarray) array of coordinates of vertices of a polygon (different start/end; assumed to close).
        """
        if isinstance(pts, np.ndarray):
            self.__points = pts
        if isinstance(pts, list) and all(isinstance(p, np.ndarray) for p in pts):
            self.points = np.vstack(pts)
        if isinstance(poly, np.ndarray):
            self.__polygon = poly

    # set/get points
    @property
    def points(self):
        return self.__points

    @points.setter
    def points(self, pts):
        if isinstance(pts,np.ndarray):
            self.__points = pts
        else:
            return "Points must be np.ndarray."

    # set/get polygon
    @property
    def polygon(self):
        return self.__polygon

    @polygon.setter
    def polygon(self, poly):
        if isinstance(poly, np.ndarray):
            self.__polygon = poly
        else:
            return "Polygon must be np.ndarray."

    def pointInBox(self):
        """
        Tests whether all points are within the bounding box of the polygon.

        :return: (list of bool) true/false for each point in the PIP object.
        """
        # list of T/F for each point
        retVal = []
        for point in self.__points:
            if (min(self.__polygon[:, 0]) <= point[0] <= max(self.__polygon[:, 0]) and
                    min(self.__polygon[:, 1]) <= point[1] <= max(self.__polygon[:, 1])):
                retVal.append(True)
            else:
                retVal.append(False)
        return retVal

    def isLeft(self, pt, e1, e2):
        """
        Function to determine if a point is left or right of a line.
        Adapted from pseudocode available at http://geomalgorithms.com/a01-_area.html

        :param pt: (np.ndarray) coordinates of point to test
        :param e1: (np.ndarray) coordinates of start of line segment
        :param e2: (np.ndarray) coordinates of end of line segment
        :return: 0 if on line, +1 if left, -1 if right
        """
        return np.sign((e2[0] - e1[0]) * (pt[1] - e1[1])
                       - (pt[0] - e1[0]) * (e2[1] - e1[1]))

    def windingNumber(self, pt):
        """
        Determines the winding number of a given point for the stored polygon.
        Adapted from pseudocode available at http://geomalgorithms.com/a03-_inclusion.html

        :param pt: (np.ndarray) coordinates of point to test
        :return: (int) winding number of point
        """
        # initialize counter
        wn = 0
        # iterate through all edges
        numEdge = self.__polygon.shape[0]
        for i in range(numEdge):
            e1 = self.__polygon[i, :]
            e2 = self.__polygon[(i + 1) % numEdge, :]

            # if edge crosses upward and p strictly left
            if e1[1] <= pt[1] < e2[1]:
                if pt[1] < e2[1]:
                    if self.isLeft(pt, e1, e2) == 1:  # must be left of line
                        wn += 1
            # if edge crosses downward
            elif e2[1] < pt[1] <= e1[1]: # second condition may not be needed here
                if self.isLeft(pt, e1, e2) == -1:  # must be right of line
                    wn -= 1

        return wn

    def rayCrossing(self, pt):
        """
        Determines the number of ray crossings for a given point for the stored polygon.

        :param pt: (np.ndarray) coordinates of point to test
        :return: (int) number of ray crossings. Inside if odd, outside if even.
        """
        # initialize counter
        rc = 0
        # iterate through all edges
        numEdge = self.__polygon.shape[0]
        for i in range(numEdge):
            # define edges
            e1 = self.__polygon[i, :]
            e2 = self.__polygon[(i + 1) % numEdge, :]
            # does line go upward?
            if e1[1] <= pt[1] < e2[1]:  # upcross
                # if so, is it on or to the left?
                if self.isLeft(pt, e1, e2) == 1:
                    rc += 1
            # does line go downward
            elif e2[1] <= pt[1] < e1[1]:
                # if so, is it on or to the "right"?
                if self.isLeft(pt, e1, e2) == -1:
                    rc += 1
        return rc

    def onLine(self, pt):
        """
        Function to determine if a point is on any line within the current polygon.
        May include vertices.

        :param pt: (np.ndarray) coordinates of point to test
        :return: (bool) true if point is on a line (incl on a vertex of that line)
        """
        # loop through all lines
        numEdge = self.__polygon.shape[0]
        for i in range(numEdge):
            # define edges
            e1 = self.__polygon[i, :]
            e2 = self.__polygon[(i + 1) % numEdge, :]
            # print(pt, e1, e2)
            # look for horizontal lines: if y values are equal...
            if e1[1] == pt[1] == e2[1]:
                # if x value is between them
                if min(e1[0], e2[0]) <= pt[0] <= max(e1[0], e2[0]):
                    # then it's on the horizontal line
                    return True
            # if they aren't equal, but pt is between them (inclusive)
            if min(e1[1], e2[1]) <= pt[1] <= max(e1[1], e2[1]):
                # if they are on the line
                if self.isLeft(pt, e1, e2) == 0:
                    return True
        return False

    def onVertex(self, pt):
        """
        Test to see if point is coincident with any vertex of the polygon.

        :param pt: (np.ndarray) point to test
        :return: (bool) True if on a vertex, false otherwise
        """
        numPts = self.__polygon.shape[0]
        for i in range(numPts):
            if pt[0] == self.__polygon[i, 0] and pt[1] == self.__polygon[i, 1]:
                return True
        return False

    def pointInPolygon(self, method='w+'):
        """
        Overall method to determine if a point is in a polygon, including improvements via bounding box and
        options to explicitly include points on edges and vertices.

        :param method: (str) method of computing point in polygon.  Valid inputs:
                        'ol' - points on lines only
                        'ov' - points on vertices only
                        'lv' - points on lines or vertices
                        'w' - winding number algorithm
                        'w+' - winding number PLUS any points on lines/vertices (DEFAULT)
                        'rc' - ray casting algorithm
                        'rc+' - ray casting algorithm PLUS any points on lines/vertices
        :return: (list of Bool), length = number of points to test.  True is inside, False is outside.
        """

        # narrow points to test with bounding box
        retVal = self.pointInBox()

        # for points inside the bounding box
        for i in range(len(retVal)):
            if retVal[i]:
                # see if its on a line or a vertex
                if method == 'lv':
                    lv = any([self.onLine(self.__points[i, :]), self.onVertex(self.__points[i, :])])
                    retVal[i] = all([lv, retVal[i]])

                # see if its on a line
                elif method == 'ol':
                    retVal[i] = all([retVal[i], self.onLine(self.__points[i, :])])

                # see if its on a vertex
                elif method == 'ov':
                    retVal[i] = all([retVal[i], self.onVertex(self.__points[i, :])])

                # call winding number method
                elif 'w' in method:
                    wn = self.windingNumber(self.__points[i, :])
                    if wn != 0:
                        retVal[i] = True
                    else:
                        retVal[i] = False

                # call ray casting algorithm
                elif 'rc' in method:
                    rc = self.rayCrossing(self.__points[i, :])
                    if rc % 2 == 0:
                        retVal[i] = False
                    else:
                        retVal[i] = True

                # zip - need to be true on inside OR vertex OR boundary
                if "+" in method:
                    retVal[i] = any([retVal[i], self.onLine(self.__points[i, :]),
                                    self.onVertex(self.__points[i, :])])

        # convert back to boolean
        retVal = [bool(val) for val in retVal]
        return retVal

    def viewPIP(self, method='w+', save=False):
        """
        Function to plot point, polygon and visualize results in interactive web session.
        Option to save as HTML with name "PIP_visualization_{methodname}.html".

        Note: this code MUST be granted permission to open a web browser to view plot.
        Otherwise, save as HTML and open.

        Colors from XKCD color survey https://xkcd.com/color/rgb/
        Scatterplot code adapted from https://mpld3.github.io/examples/scatter_tooltip.html
        Interactive legend code adapted from https://mpld3.github.io/examples/interactive_legend.html
        HTML tooltip code adapted from https://mpld3.github.io/examples/html_tooltips.html

        :param method: (str) method of computing point in polygon.  Valid inputs:
                        'ol' - points on lines only
                        'ov' - points on vertices only
                        'lv' - points on lines or vertices
                        'w' - winding number algorithm
                        'w+' - winding number PLUS any points on lines/vertices (DEFAULT)
                        'rc' - ray casting algorithm
                        'rc+' - ray casting algorithm PLUS any points on lines/vertices
        :param save: (bool) true to enable saving; default false.
                        True saves image in code location folder as "PIP_visualization_{methodname}.html".
        :return: none; opens web session, hit ctrl-c to exit.
        """
        # run PIP
        inOut = self.pointInPolygon(method=method)

        # colors drawn from
        outcolor = "#fd4659" # watermelon
        incolor = "#01386a" # marine blue

        # initialize plot with space for legend
        fig, ax = plt.subplots(subplot_kw=dict(axisbg='#EEEEEE'))
        fig.subplots_adjust(right=0.8)

        # add polygon to image
        poly = Pgon((self.__polygon[:, [0, 1]]),
                    alpha=0.7,
                    facecolor="grey",
                    edgecolor='none',
                    label="Polygon")
        ax.add_patch(poly)

        # collection of all plotted objects
        plotted_objects = [poly]

        # break into yes and no point lists
        outPts = np.empty([0, 3])
        inPts = np.empty([0, 3])

        # creator for tooltip labels
        outLabel = []
        inLabel = []

        # break into inpoints/outpoints collections, with appropriate labels in html
        for i in range(len(inOut)):
            if inOut[i]:
                inPts = np.vstack([inPts,self.__points[i, :]])
                inLabel.append('<p style="color:'+incolor+
                               ';"><i>Inside</i> <br> Pt {0}: ({1:.0f}, {2:.0f})</p>'.format(i + 1, self.__points[i, 0],
                                                                                             self.__points[i, 1]))
            else:
                outPts = np.vstack([outPts,self.__points[i, :]])
                outLabel.append('<p style="color:'+outcolor+
                                ';"><i>Outside</i> <br> Pt {0}: ({1:.0f}, {2:.0f})</p>'.format(i + 1, self.__points[i, 0],
                                                                                               self.__points[i, 1]))
        # scatterplot for points outside
        outScat = ax.scatter(outPts[:, 0],
                             outPts[:, 1],
                             c=outcolor,
                             s=500,
                             alpha=0.7,
                             cmap=plt.cm.jet,
                             edgecolors='none',
                             label="Outside")

        # scatterplot for points inside
        inScat = ax.scatter(inPts[:, 0],
                             inPts[:, 1],
                             c=incolor,
                             s=500,
                             alpha=0.7,
                             cmap=plt.cm.jet,
                             edgecolors='none',
                             label="Inside")

        # create html tooltips
        htmltooltip1 = mpld3.plugins.PointHTMLTooltip(outScat, labels=outLabel, hoffset=10, voffset=10)
        htmltooltip2 = mpld3.plugins.PointHTMLTooltip(inScat, labels=inLabel, voffset=10, hoffset=10)

        # add scatterplots to object collections
        plotted_objects.append(outScat)
        plotted_objects.append(inScat)

        # add title, axes
        title = "Point in Polygon"
        method_text = ""
        if 'w' in method:
            method_text += "Winding Number Algorithm"
        elif 'rc' in method:
            method_text += "Ray Casting Algorithm"
        elif method == 'ol':
            method_text += "Points on Lines (only)"
        elif method == 'ov':
            method_text += "Points on Vertices (only)"
        elif method == 'lv':
            method_text += "Points on Lines and Vertices (only)"

        if '+' in method:
            method_text += ", lines and vertices included."

        # add title
        ax.set_title(title, size=20)
        # add method information as text
        ax.text(min(min(self.__polygon[:, 0]), min(self.__points[:, 0])) - 0.5,
                min(min(self.__polygon[:, 1]), min(self.__points[:, 1])) - 0.5,
                method_text, size=10, style='italic')
        ax.grid(color='white', linestyle='solid')
        ax.set_xlabel("x position")
        ax.set_ylabel("y position")

        # add interactive legend to plot
        interactive_legend = mpld3.plugins.InteractiveLegendPlugin(plotted_objects,
                                                                   ["Polygon", "Outside", "Inside"],
                                                                   alpha_unsel=0.3,
                                                                   alpha_over=1.5)

        # connect all plugins to figure
        mpld3.plugins.connect(fig, htmltooltip1, htmltooltip2, interactive_legend)

        # if save is true, will save to an html as PIP_visualization.html
        if save:
            mpld3.save_html(fig, "PIP_visualization_"+method+".html")

        # display in local web browser (hit Ctrl-C to exit)
        mpld3.show()

        pass


# Example Usage (Supplied with Coursework)

p1 = Point(1,2) # Create some points
p2 = Point(2,3)
p3 = Point(4,5)

p1.x
p1.x = 10       # Change the x coordinate of a point
p1.x

l1 = Line([p1.coords, p2.coords, p3.coords])  # Create a line using the points coordinates
l1.coords
pg1 = Polygon([p1.coords, p2.coords, p3.coords]) # Create a polygon using the point coords
pg1.coords
pg2 = Polygon([l1.coords])                    # Create a polygon using the line coordinates
pg2.coords

p4 = Point(6,7)                               # Add a point to a line
l1.addPoint(p4.coords)
l1.coords

pg2.addPoint(Point(8,9).coords)               # Add a point to a polygon

p1.addPoint(p4.coords)                        # Try to add a point to a point

p1.getStartPoint()                            # Return start and end points of points, lines and polygons
p1.getEndPoint()
l1.getStartPoint()
l1.getEndPoint()


# Example of PIP Usage

# read in test polygon from file (relative to code location)
testPoly = "testData/testPoly.csv"
pg3 = Polygon(testPoly)

# read in test points into line object from file (relative to code location)
testPoints = "testData/testPoints.csv"
l4 = Line(testPoints)

# run only one of the following at a time
# plot is interactive - click to turn on/off each category of points
# use tools in the bottom left to zoom, pan, and reset
# see if polygon contains all points within "line", must view in web browser or open HTML
pg3.contains(l4, method='rc+', plot=True, save=True)
# see if all vertices on line are within polygon, must view in web browser or open HTML
# l4.isInside(pg3, plot=True, method='w+', save=True)
