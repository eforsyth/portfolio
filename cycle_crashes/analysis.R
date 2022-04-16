# exploring cycle craches across Auckland

# main source to follow:
# https://carto.com/blog/predicting-traffic-accident-hotspots-with-spatial-data-science/

# libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(httr)
library(spdep)
library(tmap)

tmap_mode("view")


# get nzta crash data -----------------------------------------------------
# build query as url
url_crashes       <- parse_url("https://services.arcgis.com/CXBb7LAjgIIdcsPt/arcgis/rest/services")
url_crashes$path  <- paste(url_crashes$path, "CAS_Data_Public/FeatureServer/0/query", sep = "/")
url_crashes$query <- list(where = "tlaName = 'Auckland' AND crashYear >= 2016 AND bicycle > 0",
                          outFields = "bicycle, crashYear, tlaName, crashSeverity, minorInjuryCount, seriousInjuryCount, fatalCount, speedLimit",
                          returnGeometry = "true",
                          f = "geojson")

request_crashes <- build_url(url_crashes) # what our final url request looks like

# grab response from the API
data_crashes <- read_sf(request_crashes) %>% 
  st_transform(2193) %>% 
  select(crashYear, bicycle, speedLimit, crashSeverity, minorInjuryCount, seriousInjuryCount, fatalCount) %>% 
  drop_na() %>% 
  st_as_sf()

# looks to be a lot of data in the table which could undoubtedly be useful, but will just focus on some basic columns to keep things simple


# get ancillary data ------------------------------------------------------
# Auckland land area
url_akl       <- parse_url("https://services1.arcgis.com/n4yPwebTjJCmXB6W/arcgis/rest/services")
url_akl$path  <- paste(url_akl$path, "Base_Region/FeatureServer/0/query", sep = "/")
url_akl$query <- list(where = "STATUS = 'Land'",
                  outFields = "STATUS", 
                  returnGeometry = "true",
                  f = "geojson")

# created object url_traffic for demonstrative purposes
# here we bundle the final url building into the read_sf function
data_akl_extent <- read_sf(build_url(url_akl)) %>% 
  st_transform(2193)

# peak daily traffic counts
# column count_date is unix timestamp
url_traffic       <- parse_url("https://services2.arcgis.com/JkPEgZJGxhSjYOo0/arcgis/rest/services")
url_traffic$path  <- paste(url_traffic$path, "TrafficService/FeatureServer/0/query", sep = "/")
url_traffic$query <- list(where = "count_date >= '2016-01-01 00:00:00' AND peaktraffic IS NOT NULL",
                          outFields = "count_date, peaktraffic",
                          returnGeometry = "true",
                          f = "geojson")

data_traffic <- read_sf(build_url(url_traffic)) %>% 
  st_transform(2193)


# create hexbins ----------------------------------------------------------
data_hexbin <- st_make_grid(data_crashes,
                            cellsize = 300,
                            square = FALSE)  %>% 
  st_as_sf() %>% 
  rowid_to_column() %>% 
  rename(hex_id = rowid,
         geom = x) %>% 
  mutate(hex_area_km2 = round(as.numeric(st_area(geom) / 1000), 2)) %>% 
  relocate(geom, .after = last_col()) %>% 
  .[data_akl_extent,] # return only hexbins inside Auckland's terrestrial extent

# join crashes to hexbins
data_hexbin_crashes <- st_join(data_crashes,
                               data_hexbin,
                               join = st_within) %>% 
  as_tibble() %>% 
  group_by(hex_id) %>% 
  summarise(across(c(bicycle, minorInjuryCount, seriousInjuryCount, fatalCount), sum)) %>% 
  rename(crashes = bicycle) %>% 
  right_join(data_hexbin,
             by = "hex_id")  %>% 
  replace_na(list(crashes = 0)) %>% 
  mutate(crashes_density_km2 = round(crashes / hex_area_km2, 2)) %>% 
  select(hex_id, hex_area_km2, crashes, crashes_density_km2, minorInjuryCount, seriousInjuryCount, fatalCount, geom) %>% 
  st_as_sf()


# quick map of hexbinned crashes (filtered to show only those with >0 crashes)
qtm(data_hexbin_crashes %>% filter(crashes > 0), fill = "crashes")


# temporal analysis -------------------------------------------------------
# plot 1: line plot with daily average + seven day moving average
# plot 2: bar/lollipop plot of accident counts by day of week
# plot 3: bar/lollipop plot of accident counts by month
# plot 4: temporal heatmap of accident counts by day per year
# unfortunately crashes are only recorded to the year so is little temporal analysis to be done


# create spatial weight matrix --------------------------------------------
model_weights_queen_hex <- nb2listw(poly2nb(data_hexbin_crashes,
                                            queen = TRUE),
                                    style = "W",
                                    zero.policy = TRUE)


# global Moran I ----------------------------------------------------------
# value for bicycle crashes = 0.25 (p-value of 0.001)
# evidence of clustering across space, meaning pattern is not driven by random process(es)
moran.mc(data_hexbin_crashes$crashes,
         listw = model_weights_queen_hex, 
         nsim = 999, 
         zero.policy = TRUE)

z <- moran.mc(data_hexbin_crashes$crashes,
              listw = model_weights_queen_hex, 
              nsim = 999, 
              zero.policy = TRUE)

# default spdep Moran scatter plot
moran.plot(data_hexbin_crashes$crashes,
           listw = model_weights_queen_hex, 
           zero.policy = TRUE,
           labels = FALSE,
           xlab = "Crashes per hexbin",
           ylab = "Spatially lagged crashes per hxbin")


# local Moran I -----------------------------------------------------------
localMoran_hex <- localmoran(data_hexbin_crashes$crashes,
                             model_weights_queen_hex,
                             zero.policy = TRUE)

# LISA classification
# re-scale crash count so mean is zero (mean-centering) & compute spatial lag
# use case_when to classify clusters or outliers
# testing for p-value of 0.01 means has <1% chance to be random result, and value can easily change be changed
#   e.g. 0.05 for <5%, 0.02 for <2%, or 0.001 for <0.1%
#   typical thresholds are 0.1% (0.001) and 1% (0.01), though 5% (0.05), and 10% (0.1) are also used

# set our p-value threshold
# defining it as a value means it can easily be changed without having to edit the code below each time
p1 = 0.01

results_hex_crashes <- data_hexbin_crashes %>% 
  mutate(LISA_crashes_rescale = scale(crashes),
         LISA_crashes_lag = lag.listw(model_weights_queen_hex, 
                                           LISA_crashes_rescale,
                                           zero.policy = TRUE),
         LISA_type = case_when(LISA_crashes_rescale >0 & LISA_crashes_lag >0 & localMoran_hex[,5] <= p1 ~ "High-high cluster",
                               LISA_crashes_rescale <0 & LISA_crashes_lag <0 & localMoran_hex[,5] <= p1 ~ "Low-low cluster",
                               LISA_crashes_rescale >0 & LISA_crashes_lag <0 & localMoran_hex[,5] <= p1 ~ "High-low outlier",
                               LISA_crashes_rescale <0 & LISA_crashes_lag >0 & localMoran_hex[,5] <= p1 ~ "Low-high outlier",
                               TRUE ~ as.character("Not-significant"))) %>% 
  relocate(geom, .after = last_col())

# summarise LISA classification
table(results_hex_crashes$LISA_type)


# map results
tm_shape(results_hex_crashes %>% filter(str_detect(LISA_type, "High-high"))) +
  tm_polygons("crashes",
              alpha = 0.5)


#####
