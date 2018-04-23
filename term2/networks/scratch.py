
def remove_one_way(G):
    """
    Removes one way streets by adding symmetric edges as needed.

    Parameters
    ----------
    G : networkx multidigraph

    Returns
    -------
    networkx multidigraph
    """
    # locate all oneway edges
    oneway_edges = [e for e in G.edges(data='oneway', keys=True) if e[3]]
    # for each, create edges the other way
    for segment in oneway_edges:
        data = G.get_edge_data(*segment)
        G.add_edge(*segment[0:2],)

    return G



def evaluate_edges(graph, route, freq={}, eval_function=lambda x: x['length'], *args, **kwargs):
    """
    This function evaluates edges based on some evaluation function, returning the id of the best next node.

    Parameters
    ----------
    graph : networkx graph object
        graph containing OSM data for area of interest
    route : list
        list of nodes traversed by a route
    freq : dict
        dictionary of frequency of traversal, where keys are in format (startnode, endnode, 0)

    Returns
    -------
    next_node : int
        id of next best node
    """
    # get all neighbors of current node
    current_node = route[-1]

    # get all neighbors
    neighbors = [neighbor for neighbor in nx.all_neighbors(graph, current_node)]
    neighbors = list(set(neighbors))
    # ensure that you cannot revisit the previous nodes
    if len(route) > 1:
        previous_node = route[-2]
        neighbors = [x for x in neighbors if x != previous_node]

    # remove neighbor nodes that are dead ends (only point to one node)
    n = []
    badnode = []
    for neighbor in neighbors:
        nneighbor = [neighbor for neighbor in nx.all_neighbors(graph, neighbor)]
        if len(set(nneighbor)) > 1:
            n.append(neighbor)
        else:
            badnode.append(neighbor)
    neighbors = n

    # initialize suitability list
    suitability = []

    # for each neighbor, query edge attributes
    for neighbor in neighbors:
        # initialize attribute dictionary
        attributes = dict()

        # road length
        attributes['length'] = seg_attribute(graph, current_node, neighbor)

        # frequency
        attributes['frequency'] = 0
        if (current_node, neighbor, 0) in freq.keys():
            attributes['frequency'] += freq[(current_node, neighbor, 0)]
        elif (neighbor, current_node, 0) in freq.keys():
            attributes['frequency'] += freq[(neighbor, current_node, 0)]

        # bearings
        if len(route) > 1:
            attributes['previous_bearing'] = seg_attribute(graph, previous_node,
                                                           current_node, attribute='bearing')
        else:
            attributes['previous_bearing'] = None

        attributes['next_bearing'] = seg_attribute(graph, current_node, neighbor, attribute='bearing')
        # attributes['home_bearing'] =

        # has it been traveled before on this trip?
        attributes['traveled'] = (current_node, neighbor) in zip(route, route[1:])

        # get home distance and home bearing

        # append suitability to list
        suitability.append(eval_function(attributes))

    # what if no nodes are suitable (dead end only)
    # TODO: fix
    if len(suitability) == 0:
        n = [x for x in nx.all_neighbors(graph, route[-2]) if x != route[-1] and x != route[-3]]
        n = list(set(n))
        # n = [x for x in n if x != route[-1] and x != route[-3]]
        next_node = [route[-2], n[0]]
        log("Whoops! We've hit a dead end at node {}.  Backing out via {}".format(current_node,
                                                                                  next_node))
    # select node with optimized suitability
    else:
        next_node = neighbors[suitability.index(max(suitability))]
        log("Suitability indices: {}.  Selecting node {} with suitability {}".format(suitability,
                                                                                 next_node,
                                                                                 max(suitability)))
    return next_node
    