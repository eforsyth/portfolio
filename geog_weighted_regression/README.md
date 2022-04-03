# Geographically weighted regression

Geographically weighted regression (GWR) is a special form of regression that aims to account for geographic variance, or spatial heterogeneity. 

Regular regression assumes that the relationship(s) between predictors and the response are constant (or, in the parlance of the statistician, that the relationship is stationary). In the context of geography this translates as "I assume this relationship does not vary across sapce". GWR relaxes this asusmption by allowing the relationshp between the predictors and the response to vary across space. 

Regular regression can be thought of, then, as a global average while GWR will tell us where the relationship diverges significantly from this average.
