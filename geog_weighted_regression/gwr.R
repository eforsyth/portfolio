# geographically weighted regression

# geographically weighted regression (gwr) is a special form of regression that incorporates geographic impacts into the regression analysis
# regular regression assumes that the relationship(s) between predictors and the response are constant (or, in the parlance of the statistician, that the relationship 
# is stationary). in the context of geography this translates as "I assume this relationship does not vary across sapce". gwr relaxes this asusmption by allowing the 
# relationshp between the predictors and the response to vary across space. regular regression can be thought of, then, as a global average while gwr will tell us where 
# the relationship diverges significantly from this average


# libraries ---------------------------------------------------------------
library(sf)        # 
library(spdep)     # Moran's I
library(spreg)     # spatial regression models
library(tmap)      # 
library(tidyverse) # 

tmap_mode("view")


# read in census data -----------------------------------------------------
# read in sa1 geometries


# read in census data


# linear regression -------------------------------------------------------


model_linear <- lm(...,
                   data = as_tibble(data_akl_sa2_census))

# summarise results of linear model
summary(regression_income)


# test residuals for spatial strucutres
# presence of any structure is bad as means model is systematically over- or under-predicting
# when the residual is negative, the observed value is lower than predicted (over-prediction)
# when the residual is positive, the observed value is higher than the predicted (under-prediction)

# add response and residual as columns
data_akl_sa2_census_with_residuals <- data_akl_sa2_census %>% 
  mutate(predicted_income = fitted(regression_income),
         residuals_income = residuals(regression_income))
         
# quickly map residuals
qtm(data_akl_sa2_census_with_residuals,
    fill = "residuals_income") 
         
 # formally test for spatial structuring
 # utilising a method termed Global Moran's I, specifically a unique version for use with regression residuals

# create a spatial weight matrix
# first define who is a neighbour (nb) then ... (lw)
nb <- poly2nb(data_akl_sa2_census_with_residuals,
              queen = TRUE)

lw <- nb2listw(nb,
               style = "W",
               zero.policy = TRUE)

# apply Moran's I test to linear model residuals
lm.morantest(model_linear, lw)

# observed Moran's I is: ...
# interpretation is that values >0.3 is an indication of significant clustering of high and/or low residuals (i.e. systematic over- or under-prediction)

# apply gwr ---------------------------------------------------------------
# using gwr to account for geography and produce better analysis



# apply Moran's I to gwr residuals
lm.morantest(model_gwr, lw)

# observed Moran's I is: ...
# is now below the 0.3 threshold 


# map results -------------------------------------------------------------
# ADD SYNCHRONIZED FACETING BETWEEN RESULTS
qtm(x, fill = gwr)

#####