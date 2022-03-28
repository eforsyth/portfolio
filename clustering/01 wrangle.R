# wrangling census data for clustering

# libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(GGally) # data vis & extension to ggplot

# Create SA1 level data ---------------------------------------------------
data_sa1_census <- read_sf("M:/Map Data/Data/Base Data.gpkg",
                           'data_SNZ_SA1_demographics') %>% 
  rename(sa1 = SA1) %>% 
  select(sa1, starts_with("C18")) %>% 
  select(1:2, 8:9, 31:34, 75:83, 131:138) %>% 
  mutate(C18_Ethnicity_L1_Other = C18_Ethnicity_L1_Other + C18_Ethnicity_L1_MELAA + C18_Ethnicity_L2_New_Zealander + C18_Ethnicity_L2_Other_nec) %>% 
  select(-C18_Ethnicity_L1_MELAA, -C18_Ethnicity_L2_New_Zealander, -C18_Ethnicity_L2_Other_nec) %>% 
  # add income data from census app
  right_join(read_csv("T:/Models/Spatial Economy Models/Auckland Spatial Economy 2.0/Spatial tools/Data/AKL SA1 income.csv") %>% 
               rename(sa1 = SA12018,
                      C18_income_0_to_5k   = `$5,000 or less`,
                      C18_income_5_to_10k  = `$5,001 – $10,000`,
                      C18_income_10_to_20k = `$10,001 – $20,000`,
                      C18_income_20_to_30k = `$20,001 – $30,000`,
                      C18_income_30_to_50k = `$30,001 – $50,000`,
                      C18_income_50_to_70k = `$50,001 – $70,000`,
                      C18_income_over_70k  = `$70,001 or more`) %>%  
               select(sa1, starts_with("C18_")),
             by = "sa1"
             ) %>% 
  # add SA2 identifiers + urban rural classes + geometry
  left_join(read_sf("M:/Map Data/Data/Base Data.gpkg",
                    'Data_SNZ_SA1') %>% 
              as_tibble() %>% 
              rename(sa1 = SA1,
                     sa2 = SA2_ID,
                     sa2_name = SA2_Name,
                     urban_rural_class = UrbanRural_class) %>% 
              select(sa1, sa2, sa2_name, urban_rural_class, geom),
            by = "sa1") %>% 
  select(sa1, sa2, sa2_name, urban_rural_class, starts_with("C18_"), geom) %>% 
  filter(!is.na(sa2))


# Create SA2 level data ---------------------------------------------------
data_sa2_census <- data_sa1_census %>% 
  group_by(sa2) %>% 
  summarise(across(where(is.numeric) & !sa1, sum)) %>% 
  filter(!is.na(sa2)) %>% 
  left_join(read_sf("M:/Map Data/Data/Base Data.gpkg",
                    'data_SNZ_SA2') %>% 
              as_tibble() %>% 
              rename(sa2 = SA2_ID) %>% 
              select(sa2, geom),
            by = "sa2")


# Standardise all variables -----------------------------------------------
data_sa1_census_standardised <- data_sa1_census %>% 
  mutate(across(starts_with("C18_"), ~ as.numeric(scale(.)))) %>%
  st_as_sf()
  
data_sa2_census_standardised <- data_sa2_census %>% 
  mutate(across(starts_with("C18_"), ~ as.numeric(scale(.)))) %>%
  st_as_sf()


# exploratory data vis ----------------------------------------------------
# create correlogram of (selected) variables
# warning, can be slow for many (and definitely all) variables...
vis_explore_correlogram <- ggpairs(data_sa1_census_standardised,
                                   columns = c(12:17, 23:29), # selecting columns to use
                                   title = "Correlogram of SA1 yrs @ usual residence + incomes")
vis_explore_correlogram


#####
