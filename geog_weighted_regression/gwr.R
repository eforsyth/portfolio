# geographically weighted regression


# libraries ---------------------------------------------------------------
library(sf)        # spatial data handling
library(spdep)     # Moran's I
library(spreg)     # spatial regression models
library(tmap)      # mapping
library(tidyverse) # assorted 'tidy' libraries

# switch tmap from static to interactive mapping
tmap_mode("view")


# read in census data -----------------------------------------------------
# read in sa1 geometries


# read in census data


# linear regression -------------------------------------------------------
# define regression equation
model_equation <-

# create linear regression model
model_linear <- lm(...,
                   data = as_tibble(data_akl_sa2_census))

# summarise results of linear model
summary(regression_income)

# add response and residual as columns
data_akl_sa2_census_with_residuals <- data_akl_sa2_census %>% 
  mutate(predicted_income = fitted(regression_income),
         residuals_income = residuals(regression_income))
         
# quickly map residuals
qtm(data_akl_sa2_census_with_residuals,
    fill = "residuals_income") 

# test residuals for spatial strucutres with global Moran's i
# presence of any structure is bad as means model is systematically over- or under-predicting
# when the residual is negative, the observed value is lower than predicted (over-prediction)
# when the residual is positive, the observed value is higher than the predicted (under-prediction)

# create a spatial weight matrix
lw <- nb2listw(poly2nb(data_akl_sa2_census_with_residuals,
                       queen = TRUE),
               style = "W",
               zero.policy = TRUE)

# apply Moran's i test to linear model residuals
lm.morantest(model_linear, lw)

# observed value of 0.00 for linear model
# indication of significant clustering of high and/or low residuals (i.e. systematic over- or under-prediction)


# apply gwr ---------------------------------------------------------------
# using gwr to account for geography and produce better analysis


# apply Moran's i test to gwr residuals
lm.morantest(model_gwr, lw)

# observed value is 0.00 for gwr model
# is now below the 0.3 threshold 


# map results -------------------------------------------------------------
# ADD SYNCHRONIZED FACETING BETWEEN RESULTS
qtm(x, fill = gwr)

#####
