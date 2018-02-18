library(raster)
library(leaflet)
library(leaflet.extras)

# source('read_data.R')

# Create a "place" id based on lat lng
streams <- streams %>%
  group_by(activity_id) %>%
  mutate(x = round(dplyr::first(lng), digits = 1),
           y = round(dplyr::first(lat), digits = 1)) %>%
  mutate(xy = paste0(x, ',', y)) %>%
  ungroup %>%
  mutate(place_id = as.numeric(factor(xy))) %>%
  dplyr::select(-x, -y, -xy)
  
polylines <- polylines %>%
  group_by(activity_id) %>%
  mutate(x = round(dplyr::first(lng), digits = 1),
         y = round(dplyr::first(lat), digits = 1)) %>%
  mutate(xy = paste0(x, ',', y)) %>%
  ungroup %>%
  mutate(place_id = as.numeric(factor(xy))) %>%
  dplyr::select(-x, -y, -xy)
