# using a data mining approach to finding clusters

# libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
# for density-based data mining tools
library(dbscan)

# apply dbscan ------------------------------------------------------------
# this approach uses a common data mining technique called density based spatial clustering application with noise (dbscan).
# there are several dbscan variants, however, we will just use the base version to find where dense clusters of crashes exist

model_dbscan <- dbscan(
  data_crashes %>%
    # we need only the xy coordinates as seperate columns so that the algorithm can calculate the
    # euclidean (straight line) distance between events. so will extract them from the geometry column
    mutate(
      x = unlist(map(data_crashes$geometry,1)),
      y = unlist(map(data_crashes$geometry,2))) %>%
    # drop geometry, converting to table object
    st_drop_geometry() %>% 
    # keep only the ceated xy columns
    select(x, y),
  # hyperparameter #1: minimum number of points
  # hyperparameter #2: distance threshold (in m, as per our crs)
  # so, to be a cluster, a crash needs at least two (each point counting itself in this calculation) other crashes <= 50m from itself
  minPts = 3,
  eps = 50)

# join the results back to the sf data
result_dbscan_crashes <- data_crashes %>% 
  # cbind == column bind, will bind the selected column to the df, provided both are of the same length
  cbind(model_dbscan$cluster) %>% 
  # rename the added column
  rename(dbscan_cluster = model_dbscan.cluster)

# create basic interactive map showing only clustered points
qtm(y %>% filter(dbscan_cluster > 0))

#
