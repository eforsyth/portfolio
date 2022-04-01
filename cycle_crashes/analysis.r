# Exploring cycle craches across Auckland


# libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(arcpullr)  # accessing Esri REST API's
library(spdep)
library(tmap)

tmap_mode("view")


# read in data ------------------------------------------------------------
# parcels with yields
data_crashes <- get_spatial_layer("https://services2.arcgis.com/JkPEgZJGxhSjYOo0/ArcGIS/rest/services/CycleRacks_View/FeatureServer/0") %>% 
  st_transform(2193) 
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


# create spatial weight matrix --------------------------------------------
nb_hex <- poly2nb(data_hexbin_crashes, 
                   queen = TRUE)

lw_hex <- nb2listw(neighbours = nb_hex, 
                    style = "W", 
                    zero.policy = TRUE)

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
# for grid
localMoran_grid <- localmoran(data_rotorua_yields_grid$infill_yield_density_km2,
                              lw_grid,
                              zero.policy = TRUE)

# for hex
localMoran_hex <- localmoran(data_rotorua_yields_hex$infill_yield_density_km2,
                              lw_hex,
                              zero.policy = TRUE)

# LISA classification
# Re-scale yield data so mean is zero (mean-centering) & compute spatial lag
# Use case_when to classify clusters or outliers
# Testing for p-value of 0.01 means has <1% chance to be random result, and value can easily change be changed.
#   E.g. 0.05 for <5%, 0.02 for <2%, or 0.001 for <0.1%
#   Typical thresholds are 1% (0.01), 5% (0.05), and 10% (0.1)

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
table(results_rotorua_infill_hex_yields$LISA_type)


# quickly map results
qtm(results_rotorua_infill_grid_yields %>% filter(str_detect(LISA_type, "High-high")), fill = "LISA_type")
qtm(results_rotorua_infill_hex_yields %>% filter(str_detect(LISA_type, "High-high")), fill = "LISA_type")
