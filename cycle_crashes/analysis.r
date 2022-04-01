# Exploring cycle craches across Auckland


# libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(spdep)
library(tmap)

tmap_mode("view")


# read in data ------------------------------------------------------------
# parcels with yields
data_rotorua_infill_parcels <- read_sf("U:/Home/Projects/ROT007.20 Eastside Assessment and HBA/Mapping/Shiny apps/Rotorua app data.gpkg",
                                       'rotorua_capacity') %>% 
  st_centroid()

# create fishnet grid
data_fishnet_grid <- st_make_grid(data_rotorua_infill_parcels,
                                  cellsize = 200,
                                  square = TRUE) %>% 
  st_as_sf() %>% 
  rowid_to_column() %>%
  rename(grid_id = rowid,
         geom = x) %>% 
  mutate(grid_area_km2 = round(as.numeric(st_area(geom) / 1000), 2)) %>% 
  relocate(geom, .after = last_col())

# create hexagons
data_fishnet_hexagons <- st_make_grid(data_rotorua_infill_parcels,
                                      cellsize = 200,
                                      square = FALSE)  %>% 
  st_as_sf() %>% 
  rowid_to_column() %>% 
  rename(hex_id = rowid,
         geom = x) %>% 
  mutate(hex_area_km2 = round(as.numeric(st_area(geom) / 1000), 2)) %>% 
  relocate(geom, .after = last_col())

# join yield to grid
data_rotorua_yields_grid <- st_join(data_rotorua_infill_parcels,
                                    data_fishnet_grid,
                                    join = st_within) %>% 
  as_tibble() %>% 
  group_by(grid_id) %>% 
  summarise(infill_yield = sum(Infill_Yield)) %>% 
  right_join(data_fishnet_grid,
             by = "grid_id")  %>% 
  replace_na(list(infill_yield = 0)) %>% 
  mutate(infill_yield_density_km2 = round(infill_yield / grid_area_km2, 2)) %>% 
  relocate(infill_yield, .before = infill_yield_density_km2) %>% 
  relocate(geom, .after = last_col()) %>% 
  st_as_sf()

# join yield to hex
data_rotorua_yields_hex <- st_join(data_rotorua_infill_parcels,
                                   data_fishnet_hexagons,
                                   join = st_within) %>% 
  as_tibble() %>% 
  group_by(hex_id) %>% 
  summarise(infill_yield = sum(Infill_Yield)) %>% 
  right_join(data_fishnet_hexagons,
             by = "hex_id")  %>% 
  replace_na(list(infill_yield = 0)) %>% 
  mutate(infill_yield_density_km2 = round(infill_yield / hex_area_km2, 2)) %>% 
  relocate(infill_yield, .before = infill_yield_density_km2) %>% 
  relocate(geom, .after = last_col()) %>% 
  st_as_sf()


# create spatial weight matrix --------------------------------------------
# for grid
nb_grid <- poly2nb(data_rotorua_yields_grid, 
              queen = TRUE)

lw_grid <- nb2listw(neighbours = nb_grid, 
               style = "W", 
               zero.policy = TRUE)

# for hex
nb_hex <- poly2nb(data_rotorua_yields_hex, 
                   queen = TRUE)

lw_hex <- nb2listw(neighbours = nb_hex, 
                    style = "W", 
                    zero.policy = TRUE)

# global Moran I ----------------------------------------------------------
# apply to columns infill_yield or infill_yield_density_km2
# grid infill_yield_density_km2 = 0.48
# hex infill_yield_density_km2 = 0.53

# for grid
moran.mc(data_rotorua_yields_grid$infill_yield_density_km2,
         listw = lw_grid, 
         nsim = 999, 
         zero.policy = TRUE)

moran.plot(data_rotorua_yields_grid$infill_yield_density_km2,
           listw = lw_grid, 
           zero.policy = TRUE,
           labels = data_rotorua_yields_grid$grid_id,
           xlab = "Infill yield per km2",
           ylab = "Neighbours infill yield per km2")

# for hex
moran.mc(data_rotorua_yields_hex$infill_yield_density_km2,
         listw = lw_hex, 
         nsim = 999, 
         zero.policy = TRUE)

moran.plot(data_rotorua_yields_grid$infill_yield_density_km2,
           listw = lw_grid, 
           zero.policy = TRUE,
           labels = data_rotorua_yields_grid$grid_id,
           xlab = "Infill yield per km2",
           ylab = "Neighbours infill yield per km2")


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

# for grid
results_rotorua_infill_grid_yields <- data_rotorua_yields_grid %>% 
  mutate(LISA_infill_yield_rescale = scale(infill_yield_density_km2),
         LISA_infill_yield_lag = lag.listw(lw_grid, 
                                           LISA_infill_yield_rescale,
                                           zero.policy = TRUE),
         LISA_type = case_when(LISA_infill_yield_rescale >0 & LISA_infill_yield_lag >0 & localMoran_grid[,5] <= p1 ~ "High-high cluster - significant",
                               LISA_infill_yield_rescale >0 & LISA_infill_yield_lag >0 & localMoran_grid[,5] <= p2 ~ "High-high cluster - less significant",
                               LISA_infill_yield_rescale <0 & LISA_infill_yield_lag <0 & localMoran_grid[,5] <= p1 ~ "Low-low cluster - significant",
                               LISA_infill_yield_rescale <0 & LISA_infill_yield_lag <0 & localMoran_grid[,5] <= p2 ~ "Low-low cluster - less significant",
                               LISA_infill_yield_rescale >0 & LISA_infill_yield_lag <0 & localMoran_grid[,5] <= p1 ~ "High-low outlier - significant",
                               LISA_infill_yield_rescale >0 & LISA_infill_yield_lag <0 & localMoran_grid[,5] <= p2 ~ "High-low outlier - less significant",
                               LISA_infill_yield_rescale <0 & LISA_infill_yield_lag >0 & localMoran_grid[,5] <= p1 ~ "Low-high outlier - significant",
                               LISA_infill_yield_rescale <0 & LISA_infill_yield_lag >0 & localMoran_grid[,5] <= p2 ~ "Low-high outlier - less significant",
                               TRUE ~ as.character("Not-significant"))) %>% 
  relocate(geom, .after = last_col())

# summarise LISA classification
table(results_rotorua_infill_parcel_yields$LISA_type)

# for hex
results_rotorua_infill_hex_yields <- data_rotorua_yields_hex %>% 
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
