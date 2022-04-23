# Geodemographics

Geodemographics can be simply described as the study of people by where they live. Geodemographic classifications utilise multivariate statistics to group individual neighbourhoods into similar broader *types of neighbourhoods*. Such classifications are useful means of segmenting the population into distinctive groups that are easier to work with, and understand, than a myriad of unique individual areas. When properly implemented, these classifications work because - consciously or not - people with similar characteristics tend to cluster together within cities creating distinctive, and complex, urban geographies.

In this brief demonstration we'll use both k-means clustering and spatially constrained clustering techniques to classify Auckland SA1's into simple classified clusters, based on selected census variables. k-means clustering is a popular technique that is frequently applied to spatial data for geodemographic segementation. While popular, k-means is what we call a pure *attribute-space* method that does not consider *geographic-space*. When we need to produce geographically contigious clusters, we can add constraints forcing the algorithms to consider both attribute-space *and* geographic-space. Thus we shift from clustering to spatially constrained clustering.

Note that value of k (the number of clusters) is arbitrarily chosen for these examples.
