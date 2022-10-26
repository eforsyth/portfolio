# simple task of grabbing and formatting linz titles data for selected tla's
# rather than manually downloading will simply use the linz rest api to grab the data

# libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(httr)

# connect to postgis database ---------------------------------------------
conn <- DBI::dbConnect(RPostgres::Postgres(),
                       user = "<username>",
                       password = "<password>",
                       host = "localhost",
                       dbname = "base_gis_data")

# get data from linz api --------------------------------------------------

# build query
url_titles       <- parse_url("https://services.arcgis.com/xdsHIIxuCWByZiCB/arcgis/rest/services")
url_titles$path  <- paste(url_titles$path, "LINZ_NZ_Property_Titles/FeatureServer/0/query", sep = "/")
# seems to require an argument for where, so just set to id >= 1 to get everything
# downloading the whole thing will take a while... for development used: where = "land_district = 'Westland'"
url_titles$query <- list(where = "id >= 1",
                         outFields = "id, title_no, status, type, issue_date, land_district",
                         returnGeometry = "true",
                         f = "geojson")

# read in api response as a sf object and convert projection from web standard wgs84 (4326) to nztm (2193)
data_titles <- read_sf(build_url(url_titles)) %>% 
  st_transform(2193)

# read in snz sa1 dataset -------------------------------------------------

# reading in the base sa1 data from the postgis database
data_sa1 <- read_sf(conn, query = paste('SELECT sa1, sa2_id AS sa2, sa2_name, ta_name AS tla, geom', 
                                        'FROM base_gis_data.data_snz_sa1')) %>% 
  # retain only the required tla's
  filter(tla == c("Hamilton City", "Rotorua District", "Auckland", "Queenstown-Lakes District", "Tauranga City", "Matamata-Piako District",
                  "Taupo District", "Waipa District", "Waikato District", "Whangarei District", "Christchurch City", "Gisborne District"))

# identify parcels inside target tla --------------------------------------

# set aside parcel geometry
data_title_parcel_geom <- data_titles %>% 
  select(id)

# identify parcels inside target tla's
result_titles <- data_titles %>%
  mutate(
    # unix timestamp is in milliseconds, whereas lubridate works in seconds, so need to transform accordingly
    issue_date = lubridate::as_datetime(issue_date/1000),
    # extract year from data and overwrite column
    issue_date = lubridate::year(issue_date)) %>%
  # convert to point on surface instead of centroid to ensure point is inside parcel
  # centroids could place it outside for weird shaped parcels
  st_point_on_surface() %>% 
  # join to tla extents
  st_join(data_sa1,
          join = st_within) %>% 
  # drop points outside areas of interest
  filter(!is.na(tla)) %>%
  # drop point geometry
  st_drop_geometry() %>%
  # join table back to parcel geometry
  left_join(data_title_parcel_geom,
            by = "id")

# write to geopackage
result_titles %>% write_sf("linz_titles.gpkg")

#####
