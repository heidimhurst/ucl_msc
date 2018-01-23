import matplotlib.pyplot as plt
from matplotlib.patches import Polygon
import numpy as np
import mpld3
import csv

# import points from CSV
testpts = 'testData/testPoints.csv'
pts = {"x":np.empty([1,0]), "y":np.empty([1,0])}
with open(testpts, 'rb') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        if row[0].isdigit() and row[1].isdigit():
            pts["x"] = np.append(pts["x"],float(row[0]))
            pts["y"] = np.append(pts["y"],float(row[1]))

# import polygon from CSV
testpoly = 'testData/testPoly.csv'
vertices = np.empty([2,0])
with open(testpoly, 'rb') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        if row[0].isdigit() and row[1].isdigit():
            # print(row)
            # print([float(row[0])])
            # print(type([float(row[0])]))
            pt =  np.array([[float(row[0])], [float(row[1])]])
            # print(pt.shape)
            vertices = np.append(vertices, pt, axis=1)
            # poly["y"] = np.append(poly["y"],float(row[1]))

print(vertices)
print(vertices.shape)
# for point colors (replace with in/out colors)
N = len(pts['x'])
colors = np.random.random(size=N)

# initialize plot
fig, ax = plt.subplots(subplot_kw=dict(axisbg='#EEEEEE'))

# add polygon to image
poly = Polygon(np.transpose(vertices), alpha=0.7, color="grey")
ax.add_patch(poly)

# scatterplot of points
scatter = ax.scatter(pts['x'],
                     pts['y'],
                     c=colors,
                     s=1000 * np.random.random(size=N),
                     alpha=0.8,
                     cmap=plt.cm.jet)

# add mouseover for points with inside/outside information
labels = ['Point {0}: ({1}, {2})'.format(i + 1,vertices[0,i],vertices[1,i]) for i in range(N)]
tooltip = mpld3.plugins.PointLabelTooltip(scatter, labels=labels)
mpld3.plugins.connect(fig, tooltip)

# add title, axes
ax.grid(color='white', linestyle='solid')
ax.set_title("Point in Polygon", size=20)

# display in local web browser
mpld3.show()
pass



### attempt to get
def viewPIP(self, method='w'):
    # run PIP
    inOut = self.pointInPolygon(method=method)

    # convert true/false to colors
    # TODO: pick nice colors
    outcolor = "#00c0ce"
    incolor = "yellow"
    colors = ["#00c0ce" if val else "yellow" for val in inOut]

    # initialize plot
    fig, ax = plt.subplots(subplot_kw=dict(axisbg='#EEEEEE'))

    # add polygon to image
    poly = Pgon((self.__polygon[:, [0, 1]]),
                alpha=0.7,
                facecolor="grey",
                edgecolor='none',
                label="Polygon")
    ax.add_patch(poly)

    # TODO: add bounding box as visual guide?

    # scatter plot of all points
    scatter = ax.scatter(self.__points[:, 0],
                         self.__points[:, 1],
                         c=colors,
                         s=500,
                         alpha=0.6,
                         cmap=plt.cm.jet,
                         edgecolors='none',
                         label="Test Points")

    # make list of just inpoints/justoutpoints
    # inPoints, \
    # inPoints = np.array([0, 3])
    # outPoints = np.array([0, 3])
    # print(outPoints.shape)
    # print(self.__points[1,:].shape)
    # # outPoints = np.array([0,2])
    # for i in range(len(inOut)):
    #     if inOut[i]:
    #         inPoints = np.vstack([inPoints, self.__points[i, :]])
    #     else:
    #         outPoints = np.vstack([outPoints, self.__points[i, :]])

    # make list of just outpoints

    # create label for each point
    lab = ["Inside" if val else "Outside" for val in inOut]
    labels = ['Point {0}: ({1}, {2})\n\r {3}'.format(i + 1, self.__points[i, 0], self.__points[i, 1], lab[i])
              for i in range(self.__points.shape[0])]

    # scatter plot of points out
    scatter = ax.scatter(self.__points[:, 0],
                         self.__points[:, 1],
                         c=colors)  # ,
    # # c=colors,
    # s=500,
    # # alpha=0.6,
    # cmap=plt.cm.jet,
    # # edgecolors='none',
    # label=lab)


    # scatter plot of points in
    # scatter1 = ax.scatter(inPoints[:, 0],
    #                       inPoints[:, 1],
    #                       c=incolor,
    #                       s=500,
    #                       alpha=0.6,
    #                       cmap=plt.cm.jet,
    #                       edgecolors='none',
    #                       label="Test Points")

    # scatter plot pf points out



    # add label to tooltip
    # tooltip = mpld3.plugins.PointLabelTooltip(scatter, labels=labels)
    # link tooltip to figure
    # mpld3.plugins.connect(fig, tooltip)

    # # add title, axes
    # title = "Point in Polygon"
    # if 'w' in method:
    #     title += ": Winding Number Method"
    # elif 'rc' in method:
    #     title += ": Ray Casting Method"
    # elif method == 'ol':
    #     title += ": Points on Lines (only)"
    # elif method == 'ov':
    #     title += ": Points on Vertices (only)"
    # elif method == 'lv':
    #     title += ": Points on Lines and Vertices (only)"
    #
    # if '+' in method:
    #     title += ", (including L,V)"
    #
    # ax.set_title(title, size=20)
    # ax.grid(color='white', linestyle='solid')
    # ax.set_xlabel("x coordinate")
    # ax.set_ylabel("y coordinate")

    # TODO: add legend to plot
    # mpld3.plugins.connect(fig, mpld3.plugins.InteractiveLegendPlugin(scatter, ["points"]))
    # mpld3.plugins.connect(fig, mpld3.plugins.InteractiveLegendPlugin(poly, ["Polygon"]))
    scatterlabel = ["in", "out"]
    scat = ["scat"]
    # interactive_legend = mpld3.plugins.InteractiveLegendPlugin(scatter, scatterlabel)
    # mpld3.plugins.connect(fig, interactive_legend)

    # mpld3.plugins.connect(fig, mpld3.plugins.InteractiveLegendPlugin(scatter, lab))
    # mpld3.display()

    # display in local web browser
    # (hit Ctrl-C to exit)
    mpld3.show()
    pass

