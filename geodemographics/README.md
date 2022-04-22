# Geodemographics

Geodemographics can be simply described as the study of people by where they live. Geodemographic classifications/segmentations utilise multivariate statistics to...

In this brief demonstration we'll use both k-means clustering and spatially constrained clustering techniques to classify Auckland SA1's into simple clusters, based on selected census variables. The first of these, k-means, is a popular technique that is frequently applied to spatial data. While popular, k-means, is what we call a pure *attribute-space* method that does not consider *geographic-space*. When we need to produce geographically contigious clusters, we can add constraints forcing the algorithms to consider both attribute-space *and* geographic-space. Thus we shift from clustering to spatially constrained clustering.

Note that value of k (the number of clusters) is arbitrarily chosen for these examples.
