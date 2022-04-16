# network analysis

# using Python to conduct basic network analysis across Auckland
# includes downloading of network data from from openstreetmap (osm), cleaning and wrangling, then some basic routing

# libraries --------------------------------------------
import osmnx as ox # network analysis

# download + process osm network -----------------------
# defining download extent by a query of the osm api. could alternatively supply a polygon to overlay or create a bounding box
# specifying a driving network for argument "network_type" but can specify other transport modes, such as walking or public transit
# cleaning network data can be onerous, so is great that this function does an excellent job automatically doing it for us :)

G = ox.graph_from_place('Auckland, New Zealand', network_type= 'drive')

# download comes in wgs so reproject to nztm (rcs 2193)
G = ox.project_graph(G, to_crs= 'EPSG:2193')

# add columns for speed and travel times along edges
G = ox.add_edge_speeds(G)
G = ox.add_edge_travel_times(G)

# plot graph to see what we downloaded
# argument "node_size = 0" means we size the nodes to 0, effectively only visualising the edges of the network
ox.plot_graph(G, node_size = 0)

# route through network ---------------------------------
# define origin + destination by finding nearest nodes to supplied coordinates
# coordinates are in nztm (as we changed the crs earlier)
dest_node = ox.distance.nearest_nodes(G, 1758060.4480746957, 5927129.813028354)
orig_node = ox.distance.nearest_nodes(G, 1761610.5863462097, 5922711.2433362715)

# route from orig_node to dest_node using length as edge weight
route1 = ox.shortest_path(G, orig_node, dest_node, weight = 'length')

# route from orig_node to dest_node using travel time as edge weight
route2 = ox.shortest_path(G, orig_node, dest_node, weight = 'travel_time')

# plot the two routes to compare differences in using length vs travel time
fig, ax = ox.plot_graph_routes(G, routes = [route1, route2], route_colors = ['r', 'y'], route_linewidth = 6, node_size = 0)

# all done :)
