knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(sf)
library(tidygeocoder)
library(units)
library(osmdata)

# List of 48 Choke Points identified via media survey
choke_points <- tribble(
  ~choke_id, ~name,"CP01", "Yusuf Sarai Market, Safdarjung Hospital, Delhi","CP02", "Rangpuri Roundabout, under NH-48 Flyover, Delhi","CP03", "Andheria Mor, MG Road, Delhi","CP04", "CDR Chowk, Mehrauli-Gurugram Road, Delhi","CP05", "Under Chirag Delhi Flyover, Delhi","CP06", "Savitri Flyover, Outer Ring Road, Delhi","CP07", "Tara Apartments, Alaknanda Road, Delhi","CP08", "Mathura Road, Madanpur Khadar, Delhi","CP09", "Crowne Plaza, Maa Anandmayee Marg, Okhla, Delhi","CP10", "Shaheed Bijender Singh Gurjar Marg, Jaitpur, Delhi","CP11", "Sarai Kale Khan, Delhi","CP12", "Saket Metro Station, Delhi","CP13", "Khanpur T-Point, Delhi","CP14", "Hamdard T-Point, Delhi","CP15", "Pul Prahladpur to Lal Kuan Traffic Signal, MB Road, Delhi","CP16", "Netaji Subhash Marg, Delhi Gate to Nukkad Faiz Bazar, Delhi","CP17", "New Delhi Railway Station, Paharganj Side","CP18", "New Delhi Railway Station, Ajmeri Gate Side","CP19", "Rani Jhansi Road, Delhi","CP20", "Anand Parbat, Delhi","CP21", "Majnu ka Tila, Delhi","CP22", "Tis Hazari Court, Delhi","CP23", "Red Fort, Netaji Subhash Marg, Delhi","CP24", "Road No. 62, Seemapuri Roundabout, Delhi","CP25", "Nirman Vihar, Vikas Marg, Delhi","CP26", "Laxmi Nagar, Vikas Marg, Delhi","CP27", "ISBT Anand Vihar Road, Delhi","CP28", "Sardar Patel Marg, Delhi","CP29", "Teen Murti Marg to GKP, Chanakyapuri, Delhi","CP30", "Purana Quila Road, India Gate, Delhi","CP31", "Tughlaq Road Roundabout, Delhi","CP32", "Bhinder Point, Delhi","CP33", "Punjabi Bagh to Raja Garden Flyover, Delhi","CP34", "Janakpuri District Centre to Uttam Nagar Chowk, Delhi","CP35", "Najafgarh Road, Delhi","CP36", "Uttam Nagar Terminal to Nawada Metro Station, Delhi","CP37", "Road No. 201, Dwarka Sector-1, Rudra Cut to Palam Flyover, Delhi","CP38", "Mayapuri Chowk to Kirti Nagar Metro Station, Delhi","CP39", "Banda Bahadur (Barapullah) Bairangi Marg Flyover, Delhi","CP40", "Sarita Vihar Flyover, Delhi","CP41", "Geeta Colony Flyover, Delhi","CP42", "Azadpur Chowk Flyover, Delhi","CP43", "Punjabi Bagh Flyover, Delhi","CP44", "Mukarba Chowk Flyover, Delhi","CP45", "Hanuman Setu Flyover, Delhi","CP46", "Rani Jhansi Flyover, Delhi", "CP47", "Palam Dwarka Flyover, Delhi","CP48","Peeragarhi Flyover, Delhi")

# Geocoding via OpenStreetMap 

# choke_geo <- choke_points %>%
#   geocode(address = name, method  = "osm",lat     = latitude,long    = longitude,full_results = FALSE)

# Clean for NA and save the results
# choke_geo_clean <- choke_geo %>%
#   filter(!is.na(latitude) & !is.na(longitude))

# write_csv(choke_geo_clean, "geocoded_delhi_chokepoints.csv")

# Loading Data
stations_raw <- read_csv("delhi_pm25_2018_stations_latlon.csv")
traffic_cp   <- read_csv("geocoded_delhi_chokepoints.csv") # Only 19 of the total were geocoded to the map.

# Converting to SF (Simple Features) objects 
stations_sf <- st_as_sf(stations_raw, coords = c("longitude", "latitude"), crs = 4326) # WGS 84
choke_sf    <- st_as_sf(traffic_cp, coords = c("longitude", "latitude"), crs = 4326) # WGS 84

# Getting Delhi boundary from OpenStreetMap
bb_delhi <- getbb("Delhi, India") 
delhi_boundary_raw <- opq(bb_delhi) %>%
  add_osm_feature(key = "admin_level", value = "4") %>%
  add_osm_feature(key = "name", value = "Delhi") %>%
  osmdata_sf()

delhi_boundary <- delhi_boundary_raw$osm_multipolygons

delhi_boundary <- st_union(delhi_boundary) %>% st_as_sf() # Dissolve points into a single polygon

target_crs <- 32643  # UTM zone 43N (Delhi) to ensure accuracy in measuring distances | https://epsg.io/32643

stations_utm       <- st_transform(stations_sf, target_crs)
choke_utm          <- st_transform(choke_sf, target_crs)
delhi_boundary_utm <- st_transform(delhi_boundary, target_crs)


dist_threshold <- set_units(2, "km") #Distance threshold was chosen as 2 km as a reasonable estimate

# Identify which stations are within 2km of any choke point
# st_is_within_distance returns a sparse list. lengths > 0 means a match exists.
is_near_choke_index <- st_is_within_distance(stations_utm, choke_utm, dist = dist_threshold)
stations_utm$near_choke <- lengths(is_near_choke_index) > 0

# Create a clean classification df
station_class_df <- stations_utm %>%
  st_drop_geometry() %>%
  select(station, near_choke) %>%
  mutate(location_type = if_else(near_choke, "Near Choke Point", "Control Station"))

# Check counts
print(table(station_class_df$location_type))

main_plot<- ggplot() +
  # Delhi Boundary
  geom_sf(data = delhi_boundary_utm, fill = "white", color = "black", linewidth = 0.5) +

  # Choke Points
  geom_sf(data = choke_utm, aes(shape = "Traffic Choke Point"), color = "grey50", size = 2) + 

  # Plotting PM 2.5 Monitoring Stations
  geom_sf(data = stations_utm, aes(color = near_choke), size = 2) +
  scale_color_manual(
    values = c("TRUE" = "navy", "FALSE" = "red"),
    labels = c("TRUE" = "Near Choke Point", "FALSE" = "Control Station")) +

  scale_shape_manual(name = NULL, values = c("Traffic Choke Point" = 3)) +
  # Labels and Theme
  labs(
    title = "Delhi PM2.5 Stations vs. Traffic Choke Points",
    subtitle = "Map of stations within 2km of congestion zones",
    color = "Station Location" # Legend Title
  ) +
  theme_minimal()

print(main_plot)

library(jtools) # for formatted regression results
pm <- read_csv("delhi_hourly_pm25_2006-2018.csv") # Sourced from doi: 10.17632/snp7sbkb36.2

pm_analysis <- pm %>%
  inner_join(station_class_df, by = "station") %>% 
  filter(!is.na(value)) # Remove missing PM2.5 readings

pm_analysis <- pm_analysis %>%
  mutate(
    tod_period = case_when(
      hour >= 8  & hour < 11 ~ "Morning_Peak",
      hour >= 17 & hour < 21 ~ "Evening_Peak",
      hour >= 13 & hour < 14 ~ "Lunch",
      TRUE                   ~ "Off_Peak"),
    tod_period = factor(tod_period, levels = c("Off_Peak", "Morning_Peak", "Lunch", "Evening_Peak")),
    location_type = factor(location_type, levels = c("Control Station", "Near Choke Point")))
    

choke_stn<- filter(pm_analysis, location_type == "Near Choke Point")
model1 <- lm(value ~ tod_period, data = choke_stn)
summ(model1)

model2 <- lm(value ~ location_type, data = pm_analysis)
summ(model2)

model3 <- lm(value ~ tod_period * location_type, data = pm_analysis)
summ(model3)
