Multivariate clustering

These clustering methods focus on k-means and REDCAP, a spatially constrained hierarchical clustering method. The first of these, k-means, is a popular technique that is frequently applied to spatial data. It is, however, a pure attribute-space method that does not consider geographic-space. When we need to produce geographically contigious clusters, then we can add spatial constraints forcing the algorithms to respect geography.

Note that value of k is arbitrarily chosen for these examples.

could have used k-means instead of agglomerative hierarchical clustering but didn't for reasons of fairness
k-means is a partitioning method where as spatially constrained clustering is a hierarchical method. thus, we use agglomerative clustering as it is also a 
hierarchical method. additionally, the agglomerative hierarchical clustering method is incorporated into our spatially constrained method
k-means results would be equally as geographically discontigious as the agglomerative method
