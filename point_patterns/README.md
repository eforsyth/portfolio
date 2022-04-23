# Point patterns analysis

There are two types of point pattern analysis: density-based and distance-based, both of which are briefly demonstrated here. To briefly outline each type, density-based methods focus on the *first-order* properties of a dataset. This covers variation in the locations of the points across the area of interest, potentially with reference to an underlying variable (such as population density). Distanced-based methods, meanwhile, focus on the *second-order* properties. This describes interactions between the points themselves, looking whether they appear to influence on one another and form clusters or dispersions.

These point pattern analysis methods are useful for telling us if we have spatial clusters present in our point data, but they cannot tell us precisely *where* these clusters are. However, we can use a Density-Based Spatial Clustering of Applications with Noise (DBSCAN) algorithm to properly delineate these clusters. 

Note, in contrast to the other R examples, we do not use much of the Tidyverse syntax, including sf, here as the point pattern library, spatstat, is so old that it uses its own data structures. The spatstat syntax is closer to the base R language so can be a bit ugly...
