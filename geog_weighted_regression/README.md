# Geographically weighted regression

Geographically weighted regression (GWR) is a spatialised form of regression that aims to account for geographic variance, or spatial heterogeneity. 

When applied to geographic data, regular regression assumes that the <ins>relationship(s) between predictors and the response are constant across space</ins> (or, in the parlance of the statistician, that the relationship is stationary). GWR relaxes this asusmption by allowing the <ins>relationshp between the predictors and the response to vary across space</ins>. A conventional regression model will, therefore, give us the average global response, while GWR will tell us where the relationship diverges significantly from this average.
