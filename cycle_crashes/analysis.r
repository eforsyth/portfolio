# Exploring cycle craches across Auckland

# main source to follow:
# https://carto.com/blog/predicting-traffic-accident-hotspots-with-spatial-data-science/

# libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(arcpullr)  # accessing Esri REST API's
library(spdep)
library(tmap)

tmap_mode("view")


# average daily traffic counts (points)
# https://data-atgis.opendata.arcgis.com/datasets/ATgis::average-daily-traffic-counts/explore?location=-36.912635%2C174.773565%2C15.55

# read in data ------------------------------------------------------------
# crashes from nzta
data_crashes <- get_spatial_layer("https://services.arcgis.com/CXBb7LAjgIIdcsPt/arcgis/rest/services/CAS_Data_Public/FeatureServer/0/query?outFields=*&where=1%3D1") %>% 
  st_transform(2193) %>%
  st_centroid()

# create hexbins
data_hexbin <- st_make_grid(data_crashes,
                            cellsize = 500,
                            square = FALSE)  %>% 
  st_as_sf() %>% 
  rowid_to_column() %>% 
  rename(hex_id = rowid,
         geom = x) %>% 
  mutate(hex_area_km2 = round(as.numeric(st_area(geom) / 1000), 2)) %>% 
  relocate(geom, .after = last_col())


# join crashes to hexbins
data_hexbin_crashes <- st_join(data_crashes,
                               data_hexbin,
                               join = st_within) %>% 
  as_tibble() %>% 
  group_by(hex_id) %>% 
  summarise(crashes = sum(crashes)) %>% 
  right_join(data_hexbin,
             by = "hex_id")  %>% 
  replace_na(list(crashes = 0)) %>% 
  mutate(crashes_density_km2 = round(crashes / hex_area_km2, 2)) %>% 
  relocate(crashes, .before = crashes_density_km2) %>% 
  relocate(geom, .after = last_col()) %>% 
  st_as_sf()


# temporal analysis -------------------------------------------------------
# plot 1: line plot with daily average + seven day moving average
# plot 2: bar/lollipop plot of accident counts by day of week
# plot 3: bar/lollipop plot of accident counts by month
# plot 4: temporal heatmap of accident counts by day per year


# create spatial weight matrix --------------------------------------------
nb_hex <- poly2nb(data_hexbin_crashes, 
                   queen = TRUE)

lw_hex <- nb2listw(neighbours = nb_hex, 
                    style = "W", 
                    zero.policy = TRUE)

lw_hex <- nb2listw(poly2nb(data_hexbin_crashes,
                           queen = TRUE),
                   style = "W",
                   zero.police = TRUE)

listw <- nb2listw(poly2nb(as(net_Richmond, "Spatial"), queen = TRUE))

# global Moran I ----------------------------------------------------------
# crashes_density_km2 = 0.nn
# evidence of significant clustering across space
moran.mc(data_rotorua_yields_hex$infill_yield_density_km2,
         listw = lw_hex, 
         nsim = 999, 
         zero.policy = TRUE)

# default spdep Moran scatter plot
moran.plot(data_rotorua_yields_grid$infill_yield_density_km2,
           listw = lw_grid, 
           zero.policy = TRUE,
           labels = data_rotorua_yields_grid$grid_id,
           xlab = "Infill yield per km2",
           ylab = "Neighbours infill yield per km2")

# create a better looking Moran's I scatter plot with ggplot
vis_LISA_scatter_plot <- vis_data_moran_plot_final %>% 
  ggplot(aes(x = LVm2,
             y = LVm2_lagged)) + 
  geom_point(size = 2,
             colour = "purple",
             alpha = 0.1) +
  geom_vline(xintercept = 2000,
             linetype = "dashed") +
  geom_hline(yintercept = 1800,
             linetype = "dashed") +
  geom_smooth(method = lm , 
              color = "black", 
              se = FALSE) +
  labs(title = "Spatial autocorrelation of Auckland land values",
       subtitle = "Global Moran's I returns a value of 0.286 for all parcels (p-value = 0.001)",
       x = "Parcel LV/m2 ($)",
       y = "Spatially lagged (500m) parcel LV/m2") +
  theme_ipsum(grid = FALSE) +
  facet_wrap(~UP_ZoneGroup)
  
vis_LISA_scatter_plot_marginal <- ggMarginal(vis_LISA_scatter_plot,
                                             type = "histogram",
                                             fill = "grey",
                                             size = 10)


vis_LISA_scatter_plot
vis_LISA_scatter_plot_marginal




# local Moran I -----------------------------------------------------------
localMoran_hex <- localmoran(data_hexbin_crashes$crashes,
                              lw_hex,
                              zero.policy = TRUE)

# LISA classification
# re-scale crash count so mean is zero (mean-centering) & compute spatial lag
# use case_when to classify clusters or outliers
# testing for p-value of 0.01 means has <1% chance to be random result, and value can easily change be changed
#   e.g. 0.05 for <5%, 0.02 for <2%, or 0.001 for <0.1%
#   typical thresholds are 0.1% (0.001) and 1% (0.01), though 5% (0.05), and 10% (0.1) are also used

p1 = 0.01
p2 = 0.1

results_hex_crashes <- data_rotorua_yields_hex %>% 
  mutate(LISA_infill_yield_rescale = scale(infill_yield_density_km2),
         LISA_infill_yield_lag = lag.listw(lw_hex, 
                                           LISA_infill_yield_rescale,
                                           zero.policy = TRUE),
         LISA_type = case_when(LISA_infill_yield_rescale >0 & LISA_infill_yield_lag >0 & localMoran_hex[,5] <= p1 ~ "High-high cluster - significant",
                               LISA_infill_yield_rescale >0 & LISA_infill_yield_lag >0 & localMoran_hex[,5] <= p2 ~ "High-high cluster - less significant",
                               LISA_infill_yield_rescale <0 & LISA_infill_yield_lag <0 & localMoran_hex[,5] <= p1 ~ "Low-low cluster - significant",
                               LISA_infill_yield_rescale <0 & LISA_infill_yield_lag <0 & localMoran_hex[,5] <= p2 ~ "Low-low cluster - less significant",
                               LISA_infill_yield_rescale >0 & LISA_infill_yield_lag <0 & localMoran_hex[,5] <= p1 ~ "High-low outlier - significant",
                               LISA_infill_yield_rescale >0 & LISA_infill_yield_lag <0 & localMoran_hex[,5] <= p2 ~ "High-low outlier - less significant",
                               LISA_infill_yield_rescale <0 & LISA_infill_yield_lag >0 & localMoran_hex[,5] <= p1 ~ "Low-high outlier - significant",
                               LISA_infill_yield_rescale <0 & LISA_infill_yield_lag >0 & localMoran_hex[,5] <= p2 ~ "Low-high outlier - less significant",
                               TRUE ~ as.character("Not-significant"))) %>% 
  relocate(geom, .after = last_col())

# summarise LISA classification
table(results_hex_crashes$LISA_type)


# quickly map results
qtm(results_hex_crashes %>% filter(str_detect(LISA_type, "High-high")), fill = "LISA_type")
qtm(results_hex_crashes %>% filter(str_detect(LISA_type, "High-high")), fill = "LISA_type")



#####
