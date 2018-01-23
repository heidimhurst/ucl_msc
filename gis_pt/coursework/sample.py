def windingNumber(self, pt, p=False):
    # initialize counter
    wn = 0
    # iterate through all edges
    numEdge = self.__polygon.shape[0]
    for i in range(numEdge):

        # define each edge by start/end points
        e1 = self.__polygon[i, :]
        e2 = self.__polygon[(i + 1) % numEdge, :]

        # define slope of each line & point (for l/r/on)
        # will fail for vertical lines doe
        m = (e1[1] - e2[1]) / (e1[0] - e2[0])
        b = e1[1] - m * e1[0]
        pcx = m * pt[0] + b

        # if on horizontal line, inside
        # if e1[1] == pt[1] == e2[1] and min(e1[0], e2[0]) <= pt[0] <= max(e1[0], e2[0]):
        #     wn += numEdge**2
        # if on boundary line, inside (calculated y is real y)
        # elif pt[1] == m*pt[0] + b:
        #     wn += numEdge**2
        # if crossing is upwards (ensure that points wrap using modulo)
        # and P is left of segment, increment WN by 1
        if e1[1] <= pt[1] < e2[1]:
            if pt[0] < pcx:
                # if e1[0] <= pt[0] < e2[0]:
                wn += 1
        # if crossing is downwards and P is right, decrement WN by 1
        elif e2[1] < pt[1] < e1[1]:
            if pt[0] > pcx:
                wn -= 1

        # if i == 9:
        if p:
            print("========== " + str(i) + "=========")
            print("point:")
            print(pt)
            print("edge1:")
            print(e1)
            print("edge2:")
            print(e2)
            print(wn)

    return wn


# if edge crosses upward and p strictly left
if e1[1] <= pt[1] < e2[1]:
    if pt[1] < e2[1]:
        if self.isLeft(pt, e1, e2) == 1:  # must be left of line
            wn += 1
# if edge crosses downward
elif e2[1] < pt[1] <= e1[1]:  # second condition may not be needed here
    if self.isLeft(pt, e1, e2) == -1:  # must be right of line
        wn -= 1
