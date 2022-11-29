# using a data mining approach to finding clusters

# libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
# for density-based data mining tools
library(dbscan)
# for h3 hexbin index
library(h3jsr)

# apply dbscan ------------------------------------------------------------
# this approach uses a common data mining technique called density based spatial clustering application with noise (dbscan).
# there are several dbscan variants, however, we will just use the base version to find where dense clusters of crashes exist

model_dbscan <- dbscan(
  data_crashes %>%
    # we need only the xy coordinates as seperate columns so that the algorithm can calculate the euclidean (straight line) distance between events 
    # so will extract them from the geometry column
    mutate(
      x = unlist(map(data_crashes$geometry,1)),
      y = unlist(map(data_crashes$geometry,2))) %>%
    # drop geometry, converting to table object
    st_drop_geometry() %>% 
    # keep only the ceated xy columns
    select(x, y),
  # hyperparameter #1: minimum number of points
  # hyperparameter #2: distance threshold (in m, as per our crs)
  # to be a cluster, then, a crash needs at least two (each point counting itself in this calculation) other crashes <= 50m from itself
  # euclidean (straight line) distance is computed by function, but could calculate the network distance between each crash and feed that 
  # distance matrix into dbscan as well
  minPts = 3,
  eps = 50)

# join the results back to the sf data
result_dbscan_crashes <- data_crashes %>% 
  # cbind == column bind, will bind the selected column to the df, provided both are of the same length
  # as the source for both df's is the data_crashes df, they should line up unless something have gone very super badly wrong
  cbind(model_dbscan$cluster) %>% 
  # rename the added column
  rename(dbscan_cluster = model_dbscan.cluster)

# create a basic interactive map showing only clustered points
qtm(result_dbscan_crashes %>% filter(dbscan_cluster > 0))

# add a column identifying whether a point is deemed part of a cluster or noise
result_dbscan_crashes <- data_crashes %>% 
  mutate(is_cluster = if_else(dbscan_cluster > 0, "cluster", "noise"))

# create a second interactive map showing clustered + noise points
tm_shape(result_dbscan_crashes ) +
  tm_dots(col = "is_cluster", 
          # colour by created column
          palette = c(cluster = 'cyan', 
                      noise   = 'grey'))

# aggregate to h3 hexbins -------------------------------------------------

# h3 is in wgs84, or crs 4326
result_dbscan_crashes_wgs84 <- result_dbscan_crashes %>% 
  # selecting only clustered points  
  filter(dbscan_cluster > 0) %>% 
  # reprojecting crs
  st_transform(4326)

# we will now get h3 resolution 11 hexagons that intersect points
# resolution 11 is ~50m across and an area of ~0.002 km2
# resolutions 0-15 are available, with 0 being huge and 15 tiny
# see source for further details: https://h3geo.org/docs/core-library/restable/

# find h3 cells that intersect the clustered points
process_hexes_1 <- point_to_cell(result_dbscan_crashes_wgs84, res = 11, simple = FALSE)
# get the selected cell geometries as sf objects
process_hexes_2 <- cell_to_polygon(unlist(process_hexes_1[, 'h3_resolution_11'], use.names = FALSE), simple = FALSE)

# create a map of the intersecting cells
qtm(process_hexes_2)

# we may want to generalise the pattern a bit by buffering to include the immediate neighbouring cells
# use the h3 index to grab the immediate neighbours
process_hexes_3 <- get_ring(process_hexes_2, 
                            ring_size = 1, 
                            simple = FALSE)
# get the selected cell geometries as sf objects
process_hexes_4 <- cell_to_polygon(unlist(process_hexes_3, use.names = FALSE), simple = FALSE)

# create a map of the buffered cells
qtm(process_hexes_4)

# now we have the h3 cells as sf objects, we can run a spatial join to get a count of crashes per cell
result_h3_cell <- result_dbscan_crashes_wgs84 %>% 
  st_join(process_hexes_2, 
          join = st_within) %>% 
  # drop geometry, thus converting to a table object
  # do this as r will otherwise try to dissolve the geometries, which we don't want to do
  # wouldn't be an issue here, but can sometimes take a good while to dissolve complex shapes 
  st_drop_geometry() %>% 
  # summarise crashes per h3 cell + retain dbscan cluster id
  group_by(h3_address) %>% 
  summarise(sum_crashes = sum(bicycle),
            hdbscan_cluster = first(dbscan_cluster)) %>%  
  # join summarised counts back to cell geometry
  left_join(process_hexes_2,
            by = "h3_address") %>% 
  # convert to sf object
  st_as_sf() %>% 
  # transform projection back to nztm/crs 2193
  st_transform(2193)

# basic interactive map
qtm(result_h3_cell)

# it's trivial to aggregate the values to the entire dbscan cluster
# equivalent of the gis dissolve operation
result_h3_cell_dissolved <- result_h3_cell %>% 
  group_by(hdbscan_cluster) %>% 
  # summarise the crashes per cluster id
  # as it is a sf object, r will automatically dissolve the geometries
  # can take a while for more complex geometries, but shouldn't be an issue for this small dataset
  summarise(sum_crashes = sum(sum_crashes))

# check map to see that it worked
qtm(result_h3_cell_dissolved)

# this can also be easily repeated for the buffered/generalised shapes for process_hexes_4

#
